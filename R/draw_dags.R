library(tidyverse)
library(patchwork)
library(dagitty)
library(ggdag)

# plotting config
extrafont::loadfonts(quiet = TRUE)
fontfam <- "Segoe UI"
onecolor_target <- "#67a9cf"
onecolor_nuisance <- "grey75"
onecolor_addition <- "#fc9272"

##############
#### DAG 1 ###
##############
dag <- dagify(gov ~ u1 + u2 + u3 + usaid + distr,
              out ~ u0 + u2 + u4 + usaid + gov + distr,
              sel ~ u3 + u4 + usaid + out + distr,
              usaid ~ u0 + u1,
              exposure = "gov",
              outcome = "out",
              latent = c("u0", "u1","u2","u3","u4"),
              coords = list(
                
                x = c(usaid = 1, u1 = 2, u3 = 2.5, u0 = 3, gov = 3, distr = 3, sel = 3,
                      u2 = 4, out = 5, u4 = 4.5),
                
                y = c(u0 = 5, u1 = 4, u2 = 4, usaid = 3, gov = 3, out = 3,
                      u3 = 2, distr = 1.5, sel = 0.5, u4 = 0.75))
)

df_dag1 <- dag %>% 
  tidy_dagitty(layout = "auto") %>%
  arrange(name) %>% # sort them alphabetically
  mutate(type = 
           case_when(
             name %in% c("u0", "u1", "u2", "u3", "u4") ~ "unobserved",
             name %in% c("out", "gov") ~ "target",
             name %in% c("usaid", "sel") ~ "select",
             name %in% c("distr") ~ "observed"
           )) %>% 
  mutate(
    interest_edge = case_when(
      to == "out" & name == "gov" ~ onecolor_target, 
      name == "u0" ~ onecolor_nuisance,
      name == "u1" ~ onecolor_nuisance,
      name == "usaid" ~ onecolor_nuisance,
      TRUE ~ "black"),
    interest_node = case_when(
      name == "u0" ~ onecolor_nuisance,
      name == "u1" ~ onecolor_nuisance,
      name == "usaid" ~ onecolor_nuisance,
      name %in% c("gov", "out") ~ onecolor_target,
      TRUE ~ "black")
  ) 

df_dag1_straight <- df_dag1 %>% filter(
  !(name == "usaid" & to == "out"),
  !(name == "u0")
)
df_dag1_curved <- df_dag1 %>% filter(
  (name == "usaid" & to == "out") |
    (name == "u0")
)

p_dag1 <- ggplot() + 
  theme_dag_blank() + 
  geom_dag_point(
    data = df_dag1,
    mapping = aes(
      x = x,
      y = y,
      shape = type,
      color = interest_node,
      size = type == "select"
    ),
    show.legend = FALSE
  ) +
  geom_dag_text(
    data = df_dag1,
    aes(x = x,
        y = y,
        color = interest_node), 
    # sort them alphabetically
    label = c(distr = expression("District"),
              gov   = expression(theta^'Gov'),
              out   = expression(theta^'Out'),
              sel   = expression(atop("Sample", "selection")),
              u0    = expression(italic(U[0])),
              u1    = expression(italic(U[1])),
              u2    = expression(italic(U[2])^"*"),
              u3   = expression(italic(U[3])),
              u4   = expression(italic(U[4])^"*"),
              usaid = expression(atop("USAID-", "EENT"))
    ),
    parse = TRUE,
    show.legend = FALSE,
    family = fontfam
  ) +
  geom_dag_edges_arc(
    data = df_dag1_curved,
    mapping = aes(
      edge_color = interest_edge,
      xend = xend,
      yend = yend,
      y = y,
      x = x),
    curvature = c(0.275,-0.275, 0.04),
    edge_width = 1/2) +
  
  geom_dag_edges_link(
    data = df_dag1_straight,
    mapping = aes(
      edge_color = interest_edge,
      xend = xend,
      yend = yend,
      y = y,
      x = x),
    edge_width = 1/2) +
  scale_color_manual(values = c(onecolor_target, "black", onecolor_nuisance)) + 
  scale_shape_manual(values = c(NA, 0, NA, 1)) + 
  scale_size_manual(values = c(20, 21)) + 
  theme(text = element_text(family = fontfam),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) + 
  coord_cartesian(clip = "off")

##############
#### DAG 2 ###
##############
dag2 <- dagify(gov ~ u2 + u3 + distr + barepre + rain,
               barepost ~ u2 + u4 + gov + distr + barepre + rain + inv,
               sel ~ u3 + u4 + barepost + distr,
               inv ~ gov,
               exposure = "gov",
               outcome = "barepost",
               latent = c("u2","u3","u4"),
               coords = list(
                 x = c(u3 = 2.5,gov = 3,distr = 3,sel = 3,u2 = 4,barepost = 5,u4 = 4.5,inv = 4,barepre = 4,rain = 4),
                 y = c(u2 = 4,gov = 3,barepost = 3,sel = 0.5,u3 = 2,distr = 1.5,u4 = 0.75,inv = 2.5,barepre = 4.5,rain = 5)
               )
)

df_dag2 <- dag2 %>% 
  tidy_dagitty(layout = "auto") %>%
  arrange(name) %>% # sort them alphabetically
  mutate(type = 
           case_when(
             name %in% c("u2", "u3", "u4") ~ "unobserved",
             name %in% c("barepost", "gov") ~ "target",
             name %in% c("sel") ~ "select",
             name %in% c("distr", "rain", "inv", "barepre") ~ "observed"
           )) %>% 
  mutate(
    interest_edge = case_when(
      to == "barepost" & name == "gov" ~ onecolor_target, 
      to == "inv" & name == "gov" ~ onecolor_addition,
      name %in% c("inv", "rain", "barepre") ~ onecolor_addition,
      TRUE ~ "black"),
    interest_node = case_when(
      name %in% c("inv", "rain", "barepre") ~ onecolor_addition,
      name %in% c("gov", "barepost") ~ onecolor_target,
      TRUE ~ "black")
  ) 

p_dag2 <- ggplot() + 
  theme_dag_blank() +
  geom_dag_point(
    data = df_dag2,
    mapping = aes(
      x = x,
      y = y,
      shape = type,
      color = interest_node,
      size = type == "select"
    ),
    show.legend = FALSE
  ) +
  geom_dag_text(
    data = df_dag2,
    aes(x = x,
        y = y,
        color = interest_node),
    # sort them alphabetically
    label = c(
      barepost  = expression('Bare'['post']),
      barepre  = expression('Bare'['pre']),
      distr = expression("District"),
      gov   = expression(theta^'Gov'),
      inv  = expression('Invasives'),
      rain  = expression('Rainfall'),
      sel   = expression(atop("Sample", "selection")),
      u2    = expression(italic(U[2])^"§"),
      u3   = expression(italic(U[3])),
      u4   = expression(italic(U[4])^"§")
    ),
    parse = TRUE,
    show.legend = FALSE,
    family = fontfam
  ) +
  geom_dag_edges_link(
    data = df_dag2,
    mapping = aes(
      edge_color = interest_edge,
      xend = xend,
      yend = yend,
      y = y,
      x = x),
    edge_width = 1/2) +
  scale_color_manual(values = c(onecolor_target,  onecolor_addition, "black")) +
  scale_shape_manual(values = c(NA, 0, NA, 1)) +
  scale_size_manual(values = c(20, 21)) +
  theme(text = element_text(family = fontfam),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) +
  coord_cartesian(clip = "off")

##############
#### DAG 3 ###
##############
# ...

png("results/plot_dags.png", width = 3100, height = 1600, res = 290)
p_dag1 + p_dag2 + plot_annotation(tag_level = "a") & theme(text = element_text(family = fontfam))
dev.off()
