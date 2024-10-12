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
gender_dat_path <- list(
  bj='data/beijing/beijing_add_eGFR_corrected.xlsx',
  cs='data/changsha/changsha_add_eGFR_corrected.xlsx'
)

outpath <- 'output/0.figure/figure2/'


# gender annot ------------------------------------------------------------

g_df_list <- list()
for(g in names(gender_dat_path)){
  
  if(g == 'bj'){
    gender_df <- read_excel(gender_dat_path[[g]]) %>% 
      select(patient_id, gender) %>% 
      rename(pt_id=1) %>% 
      mutate(gender=ifelse(gender==1, 'male', 'female'))
  }else{
    gender_df <- read_excel(gender_dat_path[[g]]) %>% 
      select(pid, `s1_性别`) %>% 
      rename(pt_id=1, gender=2) %>% 
      mutate(gender=ifelse(gender=='男', 'male', 'female'))
  }
  g_df_list[[g]] <- gender_df
}
g_df <- bind_rows(g_df_list)

# process -----------------------------------------------------------------

plt_list <- list()
for(g in names(data_path_list)){

  
  dat <- read_excel(data_path_list[[g]]) %>% 
    merge(., g_df, by='pt_id')
  
  plt <- ggplot(dat, aes(age, predict_age, color=gender)) +
    geom_point(size=.01) +
    # annotate('text', x=80, y=30, label=paste0('Pearson: ', cor_res, '\nMAE: ', mae)) +
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
  filename = paste0(outpath, 'cor_plot_all_gender.pdf'),
  plt_all,
  width = 9,
  height = 3
)


