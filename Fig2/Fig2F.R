# -------------------------------------
# Date: Thu Aug 15 09:47:28 2024
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
library(patchwork)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

data_path_list <- list(
  bj='output/3.clock/changsha_clock/elastic_net_scale//test_beijing/predict_age_in_test_data.xlsx',
  cs_d='output/3.clock/changsha_clock/elastic_net_scale/test_changsha_disease/predict_age_in_test_data.xlsx',
  cs_n='output/3.clock/changsha_clock/elastic_net_scale/test_changsha_normal/predict_age_in_test_data.xlsx'
)

outpath <- 'output/0.figure/figure2/'


# process -----------------------------------------------------------------

plt_list <- list()
for(g in names(data_path_list)){
  dat <- read_excel(data_path_list[[g]])
  print(min(dat$predict_age))
  cor_res <- cor(dat$predict_age, dat$age) %>% 
    round(., 2)
  
  mae <- dat %>% 
    mutate(aa=abs(age-predict_age)) %>% 
    pull(aa) %>% mean() %>% 
    round(., 2)
  
  plt <- ggplot(dat, aes(age, predict_age)) +
    geom_point() +
    annotate('text', x=80, y=30, label=paste0('Pearson: ', cor_res, '\nMAE: ', mae)) +
    geom_smooth(method = 'lm') +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(x='Chronological age(years)', y='Predicted age(years)') +
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125), limits = c(15,130), expand = c(0,0)) +
    scale_x_continuous(breaks = c(0, 25, 50, 75, 100, 125), limits = c(15,100), expand = c(0,0)) +
    theme_bw() +
    theme(
      legend.key.size = unit(.1, 'inches'),
      text = element_text(size = 12, color = 'black'),
      axis.text.x = element_text(size = 12, color = 'black'),
      axis.text.y = element_text(size = 12, color = 'black'),
      axis.ticks = element_line(color = 'black'),
      panel.border = element_rect(color = 'black'),
      legend.position = 'top',
      panel.grid = element_blank()
    )
  plt_list[[g]] <- plt
}

plt_all <- wrap_plots(plt_list, ncol = 3)

ggsave(
  filename = paste0(outpath, 'cor_plot_all.pdf'),
  plt_all,
  width = 9,
  height = 3
)


