###############
#### setup ####
###############
# load libraries
library(tidyverse)
library(glue)
library(openxlsx)

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

