# -------------------------------------
# Date: Wed Apr  9 10:57:04 2025
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
library(ggpubr)
library(patchwork)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------
age_gap_path <- 'V20250427/output/Fig5/UKB/predict_UKB/age_gaps_with_disease.xls'
hrt_dat_path <- '../00database/UKB/output/feature/HRT_use.csv'

outpath <- 'V20250427/output/Fig6/UKB/HRT/'

age_gap <- read.delim(
  file = age_gap_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
) %>% rename(eid=1) %>% 
  select(-Age)

hrt_dat <- read.csv(hrt_dat_path) %>% 
  mutate(keep=ifelse(Time!=0 | is.na(Time), 'y', 'n')) %>% 
  filter(keep=='y') %>% 
  merge(., age_gap, by='eid') %>% 
  mutate(Ever_used_HRT=factor(Ever_used_HRT, c(0,1)))



# start_time --------------------------------------------------------------

pdat <- hrt_dat %>% 
  filter(Age>=50) %>%
  filter(Age_started>=50 | is.na(Age_started))

plt <- ggplot(pdat, aes(Age_started)) +
  geom_density()

plt <- ggplot(pdat, aes(Age, predict_age, color=Ever_used_HRT)) +
  # geom_point() +
  geom_smooth(method = 'lm') +
  scale_color_manual(values = c('1' = '#3773B6', '0'='#B8281D')) +
  labs(x='Chronological age (years)', y='Predicted age (years)') +
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
ggsave(
  filename = paste0(outpath, 'lager_than_50_HRT-vs-noHRT.pdf'),
  plt,
  width = 3.5,
  height = 3.5
)


# v2

sub_pdata <- hrt_dat %>% 
  filter(Age_started %in% c(49,50,51) | (Age>49  & Ever_used_HRT==0)) %>% 
  filter(Age_last_used== -11 | Ever_used_HRT==0) %>% 
  filter(Age<70)
sub_pdata$Age_Group <- cut(sub_pdata$Age,
                           breaks = c(seq(40,70,5)),  # 定义年龄段的分界点
                           labels = c("40-44", "45-49", "50-54", "55-59", "60-64", "65-69"),  # 定义标签
                           right = F) 


plt <- ggline(
  sub_pdata,
  x = "Age_Group",
  y = "age_gaps",
  color = "Ever_used_HRT",
  palette = "jco",
  plot_type = 'p',
  add = "mean_se") +
  scale_color_manual(values = c('1' = '#3773B6', '0'='#B8281D')) +
  labs(title='', x='') +
  theme_bw() +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black',angle = 45, hjust = 1, vjust = 1),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(color = 'black'),
    legend.position = 'nonw',
    panel.grid = element_blank()
  )


ggsave(
  filename = paste0(outpath, 'HRT used_Time_Group.pdf'),
  plt,
  width = 3.5,
  height = 3.5
)







