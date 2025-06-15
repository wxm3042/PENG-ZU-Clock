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

cor_dat_path <- 'V20250427/output/Fig9/cs_normal_train/select_model/predict_age.xlsx'
norm_cs_dat_path <- 'V20250427/output/use_dat/cs/clock_data_normal.xlsx'

outpath <- 'V20250427/output/Fig9/'

cs_norm_pt <- read_excel(norm_cs_dat_path) %>% 
  pull(pt_id)

predict_dat <- read_excel(cor_dat_path) %>% 
  select(sample_id, Age, num11) %>% 
  rename(pt_id=sample_id, predict_age='num11')

write_xlsx(
  predict_dat,
  paste0(outpath, 'all_predict_data.xlsx')
)

title_list <- list(
  'cs_n'='CS \"Healthy\" (n=',
  'cs_d'='CS \"Unhealthy\" (n=',
  'bj'='BJ All (n='
)

# process -----------------------------------------------------------------

predict_dat_grp <- predict_dat %>% 
  mutate(grp=ifelse(pt_id %in% cs_norm_pt, 'cs_n', '')) %>% 
  mutate(grp=ifelse(grepl('^patient', pt_id), 'bj', grp)) %>% 
  mutate(grp=ifelse(grp=='', 'cs_d', grp))


plt_list <- list()
for(g in unique(predict_dat_grp$grp)){
  
  dat <- predict_dat_grp %>% 
    filter(grp==g)
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
  filename = paste0(outpath, '9B_cor_plot_all.pdf'),
  plt_all,
  width = 9,
  height = 3
)

