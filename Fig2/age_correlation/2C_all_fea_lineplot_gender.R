# -------------------------------------
# Date: Fri Apr 18 17:55:59 2025
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(readxl)
library(patchwork)
library(writexl)
library(ggrepel)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

dat_path <- 'V20250427/output/use_dat/merge_data.xlsx'
age_gap_path <- 'V20250427/output/Fig2/clock/age_gaps.xlsx' # 筛选用于时钟的样本分析

outpath <- 'V20250427/output/Fig2/age_correlation/'

age_gap <- read_excel(age_gap_path)
dat <- read_excel(dat_path) %>% 
  filter(pt_id %in% age_gap$pt_id)

use_fea <- dat %>% 
  select(-pt_id, -Gender, -Age, -q_id, -ID_numb, -sour) %>% 
  colnames(.)

plt_list <- list()
for (fe in use_fea) {
  pdata <- dat %>%
    select(Age, all_of(fe), Gender) %>%
    mutate(Gender = ifelse(Gender == 1, 'male', 'female')) %>%
    rename(ff = 2) %>%
    mutate(title = fe) %>%
    filter(!is.na(ff))
  
  
  
  plt <- ggplot(pdata, aes(Age, ff, color = Gender)) +
    # geom_point() +
    geom_smooth(se = T) +
    # facet_grid( ~ title) +
    scale_color_manual(values = c(male = '#3773B6', female = '#B8281D')) +
    labs(x = '',
         y = '',
         title = unique(pdata$title)) +
    theme_bw() +
    theme(
      legend.key.size = unit(.1, 'inches'),
      text = element_text(size = 24, color = 'black'),
      axis.text.x = element_text(size = 24, color = 'black'),
      axis.text.y = element_text(size = 24, color = 'black'),
      axis.ticks = element_line(color = 'black'),
      panel.border = element_rect(color = 'black'),
      legend.position = 'none',
      panel.grid = element_blank(),
      # plot.margin = unit(c(0, 0, 0, 0.6), "cm"),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(),
      plot.title = element_text(size = 24, hjust = 0.5)
    )
  plt_list[[fe]] <- plt
}

out_plt <- wrap_plots(plt_list, ncol = 12)
ggsave(
  filename = paste0(outpath,  'all_fea_lineplot_gender.pdf'),
  out_plt,
  width = 40,
  height = 20,
  limitsize = FALSE
)

