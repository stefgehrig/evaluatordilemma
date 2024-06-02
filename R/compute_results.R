###############
#### setup ####
###############
# load libraries
library(tidyverse)
library(glue)
library(openxlsx)
library(ggradar2)
library(ggrain)
library(ggrepel)
library(patchwork)

# custom functions
source("R/functions.R")

# plotting config
extrafont::loadfonts(quiet = TRUE)
fontfam <- "Segoe UI"
palcolors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(12)
gradcolors <- c("#bdc9e1", "#67a9cf", "#02818a")

#################################
#### import and process data ####
#################################
# load all csv tables
map(list.files("data", ".csv"), function(x){
  assign(x = str_remove(x, ".csv"), value = read_csv(glue("data/{x}")), envir = .GlobalEnv)
})

# name items and harmonize ordered response scale
df_codescores <- name_the_tiers(df_codescores) %>% 
  mutate(score = ifelse(grepl("binary", dimension_coding) & score == 2, 3, score)) %>% 
  mutate(score = factor(score, ordered = TRUE, levels = 1:3))

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

radarplots <- map(split(input_spec, seq(nrow(input_spec))),
                   function(x){
                     d_radar %>%
                       filter(village == x$village,
                              tier1 == x$tier1) %>%
                       discard(~all(is.na(.x))) %>%
                       map_df(~.x) %>% 
                       ggRadar2(aes(facet = tier1),
                                rescale = FALSE,
                                alpha = 0.2,
                                size = 3/4,
                                clip = "off") +
                       theme_minimal(14) +
                       theme(
                         text = element_text(family = fontfam),
                         axis.text.y = element_blank(),
                         axis.text.x = element_text(size = 10),
                         panel.grid.minor = element_blank(),
                         panel.grid.major.y = element_blank(),
                         strip.text = element_text(size= 10),
                         plot.margin = unit(c(0,0,0,0), "cm"),
                         legend.position = "none",
                       ) +
                       scale_y_continuous(
                         limits = c(1, 3),
                         breaks = c(1, 2, 3),
                         expand = c(0, 0)
                       ) +
                       labs(subtitle = if(grepl("Outcomes", x$tier1)) "" else x$village)  + 
                       scale_colour_manual(values = c("grey20"))+ 
                       scale_fill_manual(values =   c("grey20"))
                   })

theme_border <- theme_gray() + 
  theme(plot.background = element_rect(fill = NA, colour = 'black', linewidth = 0.5))

wrapped_radars <- map(split(1:24, rep(1:12, each = 2)), function(x){
  wrap_elements(radarplots[[x[1]]] + radarplots[[x[2]]] + plot_annotation(theme = theme_border))
})

png("results/plot_radar.png", width = 3075, height = 3050, res = 200)
wrap_plots(wrapped_radars,
           ncol = 3,
           nrow = 4)
dev.off()

# raincloud plot for distribution of satellite data variables
# ... bare ground
p_bare <- df_bareground %>% 
  ggplot(aes(period, barecover)) + 
  geom_rain(rain.side = 'f1x1', 
            id.long.var = "village", fill = "grey20", alpha = 0.2
  ) + 
  theme_bw(14) +
  theme(
    text = element_text(family = fontfam),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(x = "Period",
       y = "Bare ground area cover\n(Proportion of total area)") + 
  geom_text_repel(
    data = df_bareground %>% filter(period == "Pre"),
    aes(x = period, 
        y = barecover,
        label = village),
    size = 3, 
    family = fontfam, 
    hjust = -1/3,
    segment.color = NA,
    box.padding = 0.02,
    max.overlaps = 10) +
  scale_y_continuous(trans = "logit", breaks = seq(0.1,0.5,0.1),
                     limits = c(0.1, 0.5))

# ... rainfall
p_rain <- df_rainfall %>% 
  ggplot(aes(1, rainfall)) + 
  geom_rain(fill = "grey20", alpha = 0.2) +
  theme_bw(14) +
  theme(
    text = element_text(family = fontfam),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), axis.ticks.x = element_blank()
  ) +
  labs(y = "Mean annual precipitation (mm)")+ 
  geom_text_repel(
    aes(x = 1, 
        y = rainfall, 
        label = village),
    size = 3, 
    family = fontfam, 
    vjust = 0,
    segment.color = NA,
    box.padding = 0.02,
    max.overlaps = 6) 

set.seed(12)
png("results/plot_satellite.png", width = 2650, height = 1450, res = 265)
p_rain_bare + p_rain_rain + plot_layout(widths = c(2,1)) + plot_annotation(tag_levels = "a") & theme(text = element_text(family = fontfam))
dev.off()
