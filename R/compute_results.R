###############
#### setup ####
###############
# load libraries
library(tidyverse)
library(glue)
library(openxlsx)
library(ggrain)
library(ggrepel)
library(patchwork)
library(paletteer)
library(brms)
library(tidybayes)
library(coda)
library(bayesplot)
library(ggtext)

# custom functions
source("R/functions.R")

# plotting config
extrafont::loadfonts(quiet = TRUE)
fontfam <- "Segoe UI"
gradcolors <- c("#bdc9e1", "#67a9cf", "#02818a")

#################################
#### import and process data ####
#################################
# load all csv tables
map(list.files("data", ".csv"), function(x){
  assign(x = str_remove(x, ".csv"), value = read_csv(glue("data/{x}")), envir = .GlobalEnv)
})

# name items and harmonize ordered response scale, add item type
df_codescores <- name_the_tiers(df_codescores) %>% 
  mutate(score = ifelse(grepl("binary", dimension_coding) & score == 2, 3, score)) %>% 
  mutate(score = factor(score, ordered = TRUE, levels = 1:3),
         itemtype = ifelse(grepl("Outcome", tier1), "Outcome", "Governance"))

# create bare ground cover variable
df_bareground <- df_bareground %>% 
  mutate(period = str_to_title(period)) %>% 
  mutate(period = factor(period, ordered = TRUE, levels = c("Pre", "Post"))) %>% 
  mutate(barecover = area_ha / total_area)

# create averaged rainfall variable
df_rainfall <- df_rainfall %>% 
  group_by(village) %>% 
  summarise(rainfall = mean(mean_annual_precip))

################################################
#### create data summary tables and figures ####
################################################
# item response frequencies
table_items <- df_codescores %>% 
  group_by(tier1, tier3) %>% 
  summarise(
    score1 = sum(score == "1"),
    score2 = sum(score == "2"),
    score3 = sum(score == "3"),
    binary = unique(grepl("binary", dimension_coding)),
    descr = unique(descr),
    .groups = "drop"
  )

write.xlsx(table_items, "results/table_items.xlsx")

# radar plot
d_radar <- df_codescores %>%
  mutate(is_outcome = grepl("Outcomes", tier1)) %>%
  mutate(tier1 = paste(unique(tier1), collapse = "\n"), .by = is_outcome)%>% 
  mutate(tier3 = gsub("\\:.*","", tier3)) %>% 
  select(village, tier3, score, tier1) %>%
  mutate(score = as.numeric(as.character(score))) %>%
  pivot_wider(names_from = tier3, values_from = score) 

input_spec <- expand_grid(
  village = unique(d_radar$village),
  tier1 = unique(d_radar$tier1)) %>% 
  arrange(village, desc(tier1))

n_items_outcome <- sum(grepl("Outcomes", table_items$tier1))
n_items_others  <- sum(!grepl("Outcomes", table_items$tier1))

radarplots <- map(split(input_spec, seq(nrow(input_spec))),
                   function(x){

                     polar_ticks <- tibble(
                       z = c(1:(if(grepl("Outcomes", x$tier1)) n_items_outcome else n_items_others), 1),
                       y1 = rep(1, if(grepl("Outcomes", x$tier1)) (n_items_outcome+1) else (n_items_others+1)),
                       y2 = rep(2, if(grepl("Outcomes", x$tier1)) (n_items_outcome+1) else (n_items_others+1)),
                       y3 = rep(3, if(grepl("Outcomes", x$tier1)) (n_items_outcome+1) else (n_items_others+1)),
                       tier1 = x$tier1
                     )
  
                     d_radar %>%
                       rename_with(~ paste0("    ", .x), .cols = c("GS5.4", "RS2.2")) %>% 
                       rename_with(~ paste0(.x, "    "), .cols = c("GS6.2")) %>% 
                       filter(village == x$village,
                              tier1 == x$tier1) %>%
                       discard(~all(is.na(.x))) %>%
                       map_df(~.x) %>% 
                       ggRadar2(aes(facet = tier1),
                                rescale = FALSE,
                                alpha = 0.2,
                                size = 1.5,
                                clip = "off") +
                       theme_minimal(14) +
                       theme(
                         text = element_text(family = fontfam),
                         axis.text.y = element_blank(),
                         axis.text.x = element_text(size = 8.75, color = "black"),
                         panel.grid.minor = element_blank(),
                         panel.grid.major.y = element_blank(),
                         strip.text = element_text(size = 11),
                         plot.subtitle = element_text(size = 15),
                         plot.margin = unit(c(0,0,0,0), "cm"),
                         legend.position = "none",
                       ) +
                       scale_y_continuous(
                         limits = c(0.9, 3),
                         expand = c(0, 0.4)
                       ) +
                       labs(subtitle = if(grepl("Outcomes", x$tier1)) "" else x$village)  + 
                       scale_colour_manual(values = "black") + 
                       scale_fill_manual(values = "black") + 
                       geom_path(data = polar_ticks, aes(x = z, y = y1), col = "black", alpha = 0.25)+ 
                       geom_path(data = polar_ticks, aes(x = z, y = y2), col = "black", alpha = 0.25)+ 
                       geom_path(data = polar_ticks, aes(x = z, y = y3), col = "black", alpha = 0.25)

                   })

theme_border <- theme_gray() + 
  theme(plot.background = element_rect(fill = NA, colour = 'black', linewidth = 0.5))

wrapped_radars <- map(split(1:length(radarplots), rep(1:(length(radarplots)/2), each = 2)), function(x){
  wrap_elements(radarplots[[x[1]]] + radarplots[[x[2]]] + plot_annotation(theme = theme_border))
})

png("results/plot_radar.png", width = 2750, height = 3000, res = 215)
wrap_plots(wrapped_radars,
           ncol = 3,
           nrow = 4)
dev.off()

# raincloud plot for distribution of satellite data variables
# ... bare ground
p_bare <- df_bareground %>% 
  ggplot(aes(period, barecover)) + 
  geom_rain(rain.side = 'f1x1', 
            id.long.var = "village", 
            fill = "black", 
            alpha = 0.2
  ) + 
  theme_bw(14) +
  theme(
    text = element_text(family = fontfam),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption = element_text(family = fontfam, size= 12)
  ) +
  labs(x = "Period",
       caption = "*Bare ground area cover as proportion of total area\n(period indicated by subscript)",
       y = expression('Bare'['pre']*''^'*')) + 
  geom_text_repel(
    data = df_bareground %>% filter(period == "Pre"),
    aes(x = period, 
        y = barecover,
        label = village),
    size = 3.4, 
    family = fontfam, 
    hjust = -1/3,
    segment.color = NA,
    box.padding = 0.02,
    max.overlaps = 10) +
  scale_y_continuous(trans = "logit", breaks = seq(0.1,0.5,0.1),
                     limits = c(0.1, 0.5),
                     sec.axis = sec_axis(~., name=expression('Bare'['post']*''^'*')))

# ... rainfall
p_rain <- df_rainfall %>% 
  ggplot(aes(1, rainfall)) + 
  geom_rain(
    fill = "black", 
    alpha = 0.2,
  ) +
  theme_bw(14) +
  theme(
    text = element_text(family = fontfam),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    plot.caption = element_text(family = fontfam, size= 12)
  ) +
  labs(caption = "*Mean annual precipitation in mm\n(post period)",
       y = expression('Rainfall'*''^'*')) + 
  geom_text_repel(
    aes(x = 1, 
        y = rainfall, 
        label = village),
    size = 3.4, 
    family = fontfam, 
    vjust = 0,
    segment.color = NA,
    box.padding = 0.02,
    max.overlaps = 6) 

set.seed(12)
png("results/plot_satellite.png", width = 2100, height = 1400, res = 220)
p_bare + p_rain + plot_layout(widths = c(5/3,1)) + plot_annotation(tag_levels = "a") & theme(text = element_text(family = fontfam))
dev.off()

#######################
#### fit IRT model ####
#######################
# data frame for model fitting
df_model <- df_codescores %>% 
  left_join(df_admlevels,
            by = join_by(village))

# model config
family <- brmsfamily("cumulative", "logit")
priors <-
  prior("constant(1)",    class = "sd", group = "village") + # identifiability restriction
  prior("exponential(2)", class = "sd") + 
  prior("exponential(2)", class = "sd", dpar = "disc") + 
  prior("normal(0,1)",    class = "Intercept", dpar = "disc") + # this is also the default
  prior("normal(0,3)",    class = "Intercept") +
  prior("normal(0,3)",    class = "b") # instead of default improper flat prior
formula <- bf(
  score ~ 1 + (1 |i| tier3) + (0 + itemtype | village) + district,
  disc  ~ 1 + (1 |i| tier3))

# run sampling
fit_irt <- brm(
  formula = formula,
  data = df_model,
  family = family,
  prior = priors,
  control   = list(adapt_delta = 0.995),
  warmup    = 2e3,
  iter      = 6e3,
  chains    = 5,
  cores     = 5,
  seed      = 123,
  backend   = "cmdstanr"
)

# export model object
saveRDS(fit_irt, file = "Robjects/fit_irt.rds")

# some mcmc inspections
get_variables(fit_irt)
mcmc_trace(fit_irt, regex_pars = c("b_"))
mcmc_trace(fit_irt, regex_pars = c("sd_"))
mcmc_trace(fit_irt, regex_pars = c("cor_"))
mcmc_rhat(brms::rhat(fit_irt))
mcmc_acf(fit_irt, regex_pars = "cor", lags = 20)
mcmc_acf(fit_irt, regex_pars = "sd", lags = 20)

####################################
#### fit bare ground regression ####
####################################
# extract village-level posteriors of latent theta_gov
df_gov_posteriors <- gather_draws(fit_irt, r_village[village,dimension]) %>% 
  filter(dimension == "itemtypeGovernance") %>% 
  mutate(village = str_replace(village, "\\.", " ")) %>% 
  group_by(village) %>% 
  summarise(
    govpostmean = mean(.value),
    govpostsd = sd(.value)
  )

# prepare modeling data frame
df_model2 <- df_bareground %>% 
  # reshape
  select(village, period, barecover) %>% 
  pivot_wider(names_from = period, values_from = barecover) %>% 
  # logit transform proportions
  mutate(across(.cols = c(Pre, Post), qlogis)) %>% 
  # join with rainfall variable
  left_join(
    df_rainfall,
    by = join_by(village)
  ) %>% 
  # standardize rainfall variable
  mutate(rainfall_std = as.numeric(scale(rainfall))) %>% 
  # join with invasives variables
  left_join(
    df_codescores %>% 
      filter(grepl("invasiv", dimension)) %>% 
      transmute(village, invasive = as.numeric(as.character(score))),
    by = join_by(village)
  ) %>% 
  # join with theta_gov posteriors
  left_join(df_gov_posteriors,
            by = join_by(village))

# priors
prior_bare <-
  prior("normal(0,3)",    class = "Intercept", resp = "Post") +
  prior("normal(0,3)",    class = "b",         resp = "Post") +
  prior("exponential(1)", class = "sigma",     resp = "Post") +
  prior("constant(1)",    class = "sigma",     resp = "govpostmean")

# double model (due to measurement error)
formula_bare1 <- bf(Post ~ mi(govpostmean) + Pre + rainfall_std + invasive) + gaussian(identity)
formula_bare2 <- bf(govpostmean | mi(sdy = govpostsd) ~ 0) + gaussian(identity)

# run sampling
fit_bare <- brm(
  formula   = formula_bare1 + formula_bare2 + set_rescor(FALSE),
  data      = df_model2,
  prior     = prior_bare,
  control   = list(adapt_delta = 0.995),
  warmup    = 2e3,
  iter      = 6e3,
  chains    = 5,
  cores     = 5,
  seed      = 123,
  backend   = "cmdstanr",
  save_pars = save_pars(latent = TRUE)
)

# export model object
saveRDS(fit_bare, file = "Robjects/fit_bare.rds")

# some mcmc inspections
get_variables(fit_bare)
mcmc_trace(fit_bare)
mcmc_rhat(brms::rhat(fit_bare))
mcmc_acf(fit_bare, lags = 20)

######################################
#### format model estimate tables ####
######################################
# IRT model estimates (only non-village and non-item specific parameters)
other_pars <- get_variables(fit_irt)[grepl("b_|sd_|cor",  get_variables(fit_irt))]
pars_tab <- map_dfr(other_pars, function(x){
  gather_draws(fit_irt, !!as.symbol(x)) %>% 
    summarise(mean = mean(.value),
              sd = sd(.value),
              q05 = quantile(.value, 0.05),
              q50 = median(.value),
              q95 = quantile(.value, 0.95))
})  %>% 
  mutate(varlabel = 
           case_when(
             .variable == "b_Intercept[1]"                       ~ "$\\beta_1$",
             .variable == "b_Intercept[2]"                       ~ "$\\beta_2$",
             .variable == "b_disc_Intercept"                     ~ "$\\alpha$",
             .variable == "b_districtKiteto"                     ~ "$\\mu_1$",
             .variable == "b_districtLongido"                    ~ "$\\mu_2$",
             .variable == "b_districtMonduli"                    ~ "$\\mu_3$",
             .variable == "b_districtSimanjiro"                  ~ "$\\mu_4$",
             .variable == "sd_tier3__Intercept"                  ~ "$\\sigma_b$",
             .variable == "sd_tier3__disc_Intercept"             ~ "$\\sigma_a$",
             .variable == "sd_village__itemtypeGovernance"       ~ "$\\sigma_\\text{Gov}$",
             .variable == "sd_village__itemtypeOutcome"          ~ "$\\sigma_\\text{Out}$",
             .variable == "cor_tier3__Intercept__disc_Intercept" ~ "$\\rho_{a, b}$",
             .variable == "cor_village__itemtypeGovernance__itemtypeOutcome" ~ "$\\rho_{\\text{Gov},\\text{Out}}$"
           ), .before = ".variable"
  ) %>% 
  mutate(across(where(is.numeric), ~style_number(.x))) %>% 
  select(-.variable)

write.xlsx(pars_tab, file = "results/table_parameters_irt.xlsx")

# bare ground regression estimates
# table with other parameters posteriors
bare_pars <- get_variables(fit_bare)[grepl("Post_|govpost|sigma",  get_variables(fit_bare))]
bare_pars_tab <- map_dfr(bare_pars, function(x){
  gather_draws(fit_bare, !!as.symbol(x)) %>% 
    summarise(mean = mean(.value),
              sd = sd(.value),
              q05 = quantile(.value, 0.05),
              q50 = median(.value),
              q95 = quantile(.value, 0.95))
}) %>% 
  filter(!grepl("Yl", .variable)) %>%
  filter(!grepl("_govpostmean", .variable))

bare_pars_tab <- bare_pars_tab %>% 
  mutate(varlabel = 
           case_when(
             .variable == "b_Post_Intercept"          ~ "$\\gamma_0$",
             .variable == "b_Post_Pre"                ~ "$\\gamma_1$",
             .variable == "b_Post_rainfall_std"       ~ "$\\gamma_2$",
             .variable == "b_Post_invasive"           ~ "$\\gamma_3$",
             .variable == "bsp_Post_migovpostmean"    ~ "$\\gamma_4$",
             .variable == "sigma_Post"                ~ "$\\sigma$",
           ), .before = ".variable"
  ) %>% 
  mutate(across(where(is.numeric), ~style_number(.x))) %>% 
  select(-.variable)

write.xlsx(bare_pars_tab, file = "results/table_parameters_bare.xlsx")

##################################
#### plot IRT model estimates ####
##################################
p_re1a <- mcmc_intervals(fit_irt, pars = vars(contains("r_village[") & contains("Governance")),
                         prob = 0.5, prob_outer = 0.9, point_est = "median") +
  theme_classic(14) +
  labs(subtitle = "Governance processes (*&#952;<sup>Gov</sup>)*") +
  theme(text = element_text(family = fontfam),
        plot.subtitle = element_markdown()) + 
  scale_y_discrete(
    labels = function(x) 
      str_replace_all(str_remove_all(str_remove_all(x, "r_village\\["), ",itemtypeGovernance\\]"), "\\.", " ")
  )

p_re1b <- mcmc_intervals(fit_irt, pars = vars(contains("r_village[") & contains("Outcome")),
                         prob = 0.5, prob_outer = 0.9, point_est = "median") +
  theme_classic(14) +
  labs(subtitle = "Governance outcomes (*&#952;<sup>Out</sup>*)") +
  theme(text = element_text(family = fontfam),
        plot.subtitle = element_markdown())  + 
  scale_y_discrete(
    labels = function(x) 
      str_replace_all(str_remove_all(str_remove_all(x, "r_village\\["), ",itemtypeOutcome\\]"), "\\.", " ")
  )

p_re2 <- mcmc_intervals(fit_irt, regex_pars = "r_tier3\\[",
                        prob = 0.5, prob_outer = 0.9, point_est = "median")+
  labs(subtitle = "*b<sub>i</sub>*") +
  theme_classic(14) +
  theme(text = element_text(family = fontfam),
        plot.subtitle = element_markdown()) + 
  scale_y_discrete(
    labels = function(x) str_replace_all(str_remove_all(str_remove_all(x, "r_tier3\\["), ",Intercept\\]"), "\\.(?=[A-Za-z])", " ")
  )

p_re3 <- mcmc_intervals(fit_irt, regex_pars = "r_tier3__disc",
                        prob = 0.5, prob_outer = 0.9, point_est = "median")+
  labs(subtitle = "*a<sub>i</sub>*") +
  theme_classic(14) +
  theme(text = element_text(family = fontfam),
        plot.subtitle = element_markdown()) + 
  scale_y_discrete(
    labels = function(x) str_replace_all(str_remove_all(str_remove_all(x, "r_tier3__disc\\["), ",Intercept\\]"), "\\.(?=[A-Za-z])", " ")
  )

png("results/plot_param_irt.png", width = 4000, height = 5000, res = 420)
(p_re1a + p_re1b) / p_re2 / p_re3
dev.off()

####################################
#### posterior predictive plots ####
####################################
# IRT model
yrep_char <- posterior_predict(fit_irt)
ppc1 <- ppc_bars_grouped(
  y = as.numeric(as.character(df_model$score)),
  yrep = yrep_char,
  group = df_model$tier3,
  facet_args = list(ncol =4))+ 
  theme_classic(12) +
  theme(strip.text = element_text(size = 8.5),
        text = element_text(family = fontfam),
        strip.background = element_blank()) + 
  scale_y_continuous(breaks = c(0,5,10), limits = c(0,13))+ 
  scale_x_continuous(breaks = c(1,2,3))

ppc2 <- ppc_bars_grouped(
  y = as.numeric(as.character(df_model$score)),
  yrep = yrep_char,
  group = df_model$village)  +
  theme_classic(12) +
  theme(strip.text = element_text(size = 8.5),
        text = element_text(family = fontfam),
        strip.background = element_blank()) + 
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,23))+ 
  scale_x_continuous(breaks = c(1,2,3))

png("results/plot_ppc_irt.png", width = 4500, height = 6000, res = 380)
ppc1/ppc2 + plot_layout(heights = c(1,2/3)) +
  plot_annotation(tag_levels = "a") & theme(text = element_text(family = fontfam))
dev.off()

# bare ground regression
set.seed(123)
ppoverlay <- pp_check(fit_bare, resp = "Post", 
                                 ndraws = 100) +
  labs(x = "logit(Bare<sub>post</sub>)") +
  theme_classic(12) +
  theme(text = element_text(family = fontfam),
        axis.title.x = element_markdown(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

png("results/plot_ppc_bare.png", width = 1750, height = 1500, res = 300)
ppoverlay
dev.off()

####################################
#### item characteristic curves ####
####################################
outcome_items <- df_codescores %>% 
  filter(grepl("Outcome", tier1)) %>% 
  pull(tier3) %>% unique

draws <- spread_draws(fit_irt,
                      b_Intercept[threshold],
                      b_disc_Intercept,
                      r_tier3[item, par],
                      r_tier3__disc[item, par]) %>% 
  mutate(item = str_replace_all(item, "\\.(?=[A-Za-z])", " "))

icc_plots <- map(outcome_items, function(icc_item){
  
  draws_icc <- draws %>% 
    filter(item == icc_item) %>% 
    pivot_wider(names_from = "threshold", values_from = "b_Intercept", names_prefix = "threshold") 
  
  df_icclines <- expand_grid(
    draws_icc,
    x = seq(-4,4,0.05)
  ) %>% 
    mutate(
      pi_1 = plogis(exp(b_disc_Intercept + r_tier3__disc) * (threshold1 - (r_tier3 + x))),
      pi_2 = plogis(exp(b_disc_Intercept + r_tier3__disc) * (threshold2 - (r_tier3 + x))) - pi_1,
      pi_3 = 1 - pi_1 - pi_2
    ) %>% 
    pivot_longer(
      cols = contains("pi_"),
      names_to = "response",
      values_to = "prob"
    ) %>% 
    mutate(response = str_remove(response, "pi_"))
  
  df_icclines_smry <- df_icclines %>% 
    group_by(response, x) %>% 
    summarise(
      point = median(prob),
      lwr = quantile(prob, 0.05),
      upr = quantile(prob, 0.95),
      .groups = "drop"
    )
  
  p_icc1 <- df_icclines_smry %>% 
    ggplot() + 
    geom_ribbon(aes(x = x, ymin = lwr, ymax = upr, fill = response), alpha = 1/10) +
    geom_line(aes(x = x, y = point, col = response),
              lwd = 1.5) +
    theme_classic(14) +
    theme(text = element_text(family = fontfam),
          axis.title.x  = element_markdown(),
          legend.position = "right",
          axis.title.y = element_markdown()) + 
    scale_color_manual(values = gradcolors) + 
    scale_fill_manual(values = gradcolors) + 
    labs(y = paste0("Expected response probability for item<br>", "*", icc_item, "*"),
         x = "Governance outcomes (*&#952;<sup>Out</sup>*)",
         col = "Score",
         fill = "Score")
  
  return(p_icc1)
  
})

png("results/icc.png", width = 2000, height = 3400, res = 300)
wrap_plots(icc_plots) + plot_layout(ncol = 1, guides = "collect")
dev.off()

###################################
#### build main results figure ####
###################################
# plot factor change bare ground
dg <- marginaleffects::datagrid(model = fit_bare,
                                govpostmean = seq(-3,3,0.1),
                                govpostsd = median(fit_bare$data$govpostsd),
                                FUN_numeric = median)

preds_bare <- posterior_epred(
  object = fit_bare,
  resp = "Post",
  newdata = dg)

df_preds_bare <- as_tibble(preds_bare)
names(df_preds_bare) <- as.character(seq(-3,3,0.1))

df_preds_bare_grp <- df_preds_bare %>% 
  mutate(across(.cols = everything(), ~plogis(.x) / plogis(unique(dg$Pre)))) %>% 
  pivot_longer(cols = everything(), names_to = "x", values_to = "y") %>% 
  mutate(x = as.numeric(x)) %>% 
  group_by(x) %>% 
  summarise(
    mean = mean(y),
    hpdi = list(as_tibble(HPDinterval(as.mcmc(y), prob = 0.9)))
  ) %>% 
  unnest(hpdi)
  
p_pred <- df_preds_bare_grp %>%
  ggplot() +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_ribbon(aes(x = x, ymin = lower, ymax = upper),
              alpha = 0.2,
              fill = "black") +
  geom_line(aes(x = x, y = mean), size = 1.25, col = "black") +
  theme_classic(14) +
  theme(text = element_text(family = fontfam), axis.title.x  = element_markdown(),
        plot.margin = unit(c(1,0,0,0), "cm")) +
  scale_y_continuous(breaks = seq(0.5, 3, 0.5)) +
  labs(y = "Expected factor change\nin bare ground cover", 
       x = "Governance processes (*&#952;<sup>Gov</sup>*)", 
       fill = "Response\ncategory")

# plot correlation density
cor_draws <- spread_draws(fit_irt,
                          cor_village__itemtypeGovernance__itemtypeOutcome) %>% 
  rename(y = cor_village__itemtypeGovernance__itemtypeOutcome)

cor_hpdi <- cor_draws %>% 
  summarise(
    mean = mean(y),
    hpdi = list(as_tibble(HPDinterval(as.mcmc(y), prob = 0.9)))
  ) %>% 
  unnest(hpdi)

p_cor1 <- ggplot(cor_draws) +
  geom_histogram(aes(x = y),
                 bins = 40,
                 fill = "black",
                 alpha = 0.2) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_classic(14) +
  theme(text = element_text(family = fontfam), axis.title.x = element_markdown()) +
  labs(x = "Correlation of governance processes<br>and governance outcomes (*&#961;<sub>Gov, Out</sub>*)", y = "Posterior density") +
  scale_y_discrete(labels = "", expand = c(0, 0)) +
  geom_point(
    data = cor_hpdi,
    aes(y = 35, x = mean),
    size = 3,
    col = "black"
  ) +
  geom_errorbar(
    data = cor_hpdi,
    aes(y = 35, xmin = lower, xmax = upper),
    size = 1.25,
    width = 0,
    col = "black"
  )

# plot bivariate posterior
bivar_draws <- gather_draws(fit_irt, r_village[village, itemtype]) %>%
  ungroup %>% 
  pivot_wider(values_from = .value, names_from = itemtype)%>% 
  mutate(village = str_replace_all(village, "\\.", " "))

p_cor2 <- bivar_draws %>%
  ggplot(
    aes(
      x = itemtypeGovernance,
      y = itemtypeOutcome,
      color = village,
      group = village,
      fill = village
    )
  ) +
  stat_ellipse(size = 1.25) +
  # geom_density_2d(lwd = 1, contour_var = "ndensity", breaks = c(0.1)) + # alternative plot
  theme_classic(14) +
  theme(
    text = element_text(family = fontfam),
    axis.title.x  = element_markdown(),
    axis.title.y  = element_markdown(),
    legend.position = "none"
  ) +
  scale_color_paletteer_d("rcartocolor::Vivid") +
  coord_cartesian(xlim = c(-3.3, 3.3), ylim = c(-3.3, 3.3)) +
  labs(x = "Governance processes (*&#952;<sup>Gov</sup>*)", 
       y = "Governance outcomes (*&#952;<sup>Out</sup>*)", 
       color = "Village")

# plot parameter estimates
p_pars <- map(
  c("itemtypeGovernance", "itemtypeOutcome"), function(v) {
    bivar_draws %>%
      group_by(village) %>%
      summarise(mean = mean(!!as.symbol(v)),
                hpdi = list(as_tibble(HPDinterval(
                  as.mcmc(!!as.symbol(v)), prob = 0.9
                )))) %>%
      mutate(village = factor(village, ordered = TRUE, levels = rev(sort(
        unique(bivar_draws$village)
      )))) %>%
      unnest(hpdi) %>%
      ggplot() +
      geom_vline(xintercept = 0, linetype = 2) +
      geom_point(aes(y = village, x = mean, col = village), size = 3) +
      geom_errorbar(
        aes(
          y = village,
          xmin = lower,
          xmax = upper,
          col = village
        ),
        size = 1.25,
        width = 0
      ) +
      scale_color_paletteer_d("rcartocolor::Vivid", direction = -1) +
      theme_classic(14) +
      labs(
        y = "",
        x = if (v == "itemtypeGovernance")
          "Governance processes (*&#952;<sup>Gov</sup>)*"
        else
          "Governance outcomes (*&#952;<sup>Out</sup>*)"
      ) +
      theme(
        text = element_text(family = fontfam),
        axis.title.x = element_markdown(),
        legend.position = "none"
      )
  }
)

p_lowerpanel <- p_pred
p_middlepanel <- p_cor2 + p_cor1 
p_upperpanel <- p_pars[[1]] + p_pars[[2]]

png("results/plot_mainresults.png", width = 3150, height = 3400, res = 350)
p_upperpanel / free(p_middlepanel, type = "space") / free(p_lowerpanel, type = "label") & 
  plot_annotation(tag_levels = "a") & theme(text = element_text(family = fontfam))
dev.off()

#######################################################
#### plot comparison of raw data on commons trends ####
#######################################################
df_out_obj <- df_bareground %>% 
  mutate(logit_barecover = qlogis(barecover)) %>% 
  group_by(village) %>% 
  summarise(
    delta_logit_bare = logit_barecover[period == "Post"] - logit_barecover[period == "Pre"]
  )

df_out_subj <- df_codescores %>% 
  filter(tier3 == "O2.1: Commons condition trend") %>% 
  select(
    village,
    commons_trend_score = score
  )

df_out <- left_join(
  df_out_obj,
  df_out_subj,
  by = join_by(village)
)

p_out <- df_out %>% 
  ggplot(aes(x = commons_trend_score,
             y = delta_logit_bare)) + 
  geom_point(size = 3,
             alpha = 0.5) + 
  theme_bw(14) +
  theme(text = element_text(family = fontfam), 
        axis.title.y = element_markdown(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(y = "logit(Bare<sub>post</sub>) - logit(Bare<sub>pre</sub>)", 
       x = expression(italic("O2.1: Commons condition trend"))) + 
  geom_text_repel(
    aes(label = village),
    size = 3, 
    family = fontfam, 
    segment.color = NA,
    hjust = 1.5)

png("results/plot_raw_outcometrend.png", width = 1600, height = 1600, res = 275)
p_out
dev.off()

#################################################################
#### plot supporting analyses for bare ground satellite data ####
#################################################################
# import additional satellite data sources
df_bare_glad <- read.csv("data/satellite_supporting/glad_mod44_bgr_areas.csv")
df_bare_rap  <- read.csv("data/satellite_supporting/rap_mod44_bgr_areas.csv")

# GLAD bare ground vs MOD bare ground for study villages
df_bare_glad <- df_bare_glad %>% 
  mutate(
    glad_bare = glad_bgr_area / glad_area,
    modi_bare = mod_bgr_area / mod_area
  )

p_satellite1 <- df_bare_glad %>% 
  ggplot() + 
  geom_point(aes(x = glad_bare, y = modi_bare)) + 
  theme_classic(14) +
  theme(text = element_text(family = fontfam)) +
  labs(y = "MOD44B bare ground cover (2010)", 
       x = "GLAD bare ground cover (2010)") + 
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) + 
  scale_x_continuous(limits = c(0,0.5)) + 
  scale_y_continuous(limits = c(0,0.5))

with(df_bare_glad, cor(glad_bare, modi_bare)) # 0.9335193
  
# RAP bare ground vs MOD bare ground for random sample from US
df_bare_rap <- df_bare_rap %>% 
  mutate(
    mod_percent_dif = (mod_bgr_post - mod_bgr_pre) / mod_bgr_pre * 100,
    rap_percent_dif = (rap_bgr_post - rap_bgr_pre) / rap_bgr_pre * 100
  ) %>% 
  mutate(
    mod_percent_dif = case_when(
      mod_bgr_post == 0 & mod_bgr_pre == 0 ~ 0,
      TRUE ~ mod_percent_dif
    )
  )

p_satellite2 <- df_bare_rap %>% 
  ggplot() + 
  geom_point(aes(x = rap_percent_dif, 
                 y = mod_percent_dif)) + 
  theme_classic(14) +
  theme(text = element_text(family = fontfam)) +
  labs(y = "MOD44B change in bare ground cover (%)", 
       x = "RAP change in bare ground cover (%)") + 
  geom_abline(aes(intercept = 0, slope = 1), linetype = 2) 

with(df_bare_rap, cor(rap_percent_dif, mod_percent_dif)) # 0.4140066
mean(sign(df_bare_rap$mod_percent_dif) == sign(df_bare_rap$rap_percent_dif)) # 0.699

png("results/plot_satellite_supporting.png", width = 2500, height = 1300, res = 260)
p_satellite1 + p_satellite2 + plot_annotation(tag_levels = "a") & theme(text = element_text(family = fontfam))
dev.off()

#############################
#### export session info ####
#############################
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")
