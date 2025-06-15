# -------------------------------------
# Date: Tue Apr  1 18:43:54 2025
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(ggpointdensity)
library(viridis)
library(patchwork)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

data_path_list <- list(
  cs_n='V20250427/output/Fig2/clock/cs_normal_test/predict_age_in_test_data.xlsx',
  cs_d='V20250427/output/Fig2/clock/cs_disease_test/predict_age_in_test_data.xlsx',
  bj='V20250427/output/Fig2/clock/bj_test/predict_age_in_test_data.xlsx'
)

outpath <- 'V20250427/output/Fig2/clock/'

title_list <- list(
  'cs_n'='CS \"Healthy\" (n=',
  'cs_d'='CS \"Unhealthy\" (n=',
  'bj'='BJ All (n='
)

# process -----------------------------------------------------------------

plt_list <- list()
for(g in names(data_path_list)){
  dat <- read_excel(data_path_list[[g]])
  print(min(dat$predict_age))
  cor_res <- cor(dat$predict_age, dat$Age) %>% 
    round(., 2)
  
  mae <- dat %>% 
    mutate(aa=abs(Age-predict_age)) %>% 
    pull(aa) %>% mean() %>% 
    round(., 2)
  
  samp_numb <- nrow(dat)
  
  plt <- ggplot(dat, aes(Age, predict_age)) +
    geom_pointdensity(alpha=1) +
    scale_color_viridis() +
    annotate('text', x=80, y=30, label=paste0('Pearson: ', cor_res, '\nMAE: ', mae)) +
    geom_smooth(method = 'lm', color = "red") +
    # geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(x='Chronological age(years)', y='Predicted age(years)', title = paste0(title_list[[g]], samp_numb, ')')) +
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125), limits = c(15,130), expand = c(0,0)) +
    scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125), limits = c(15,100), expand = c(0,0)) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 12, color = 'black'),
      legend.key.size = unit(.1, 'inches'),
      text = element_text(size = 12, color = 'black'),
      axis.text.x = element_text(size = 12, color = 'black'),
      axis.text.y = element_text(size = 12, color = 'black'),
      axis.ticks = element_line(color = 'black'),
      panel.border = element_rect(color = 'black'),
      legend.position = 'none',
      panel.grid = element_blank(),
      plot.title = element_text(hjust = .5)
    )
  plt_list[[g]] <- plt
}

plt_all <- wrap_plots(plt_list, ncol = 3)

ggsave(
  filename = paste0(outpath, '2E_cor_plot_all.pdf'),
  plt_all,
  width = 9,
  height = 3
)

