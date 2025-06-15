# -------------------------------------
# Date: Wed May 14 18:10:09 2025
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
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("select", "dplyr")

# detach(unload='conflicted')
# input -------------------------------------------------------------------

age_gap_path <- 'V20250427/output/Fig5/NHANES/predict_NHANES/age_gaps_with_disease.xls'
HRT_path <- '../00database/NHANES/All_250516/5_Questionnaire_Data/HRT.txt'
fea_dat_path <- '../00database/NHANES/All_250516/1_Demographics_Data/single_year/merge_Demographics_Data_clean.xlsx'

outpath <- 'V20250427/output/Fig5/NHANES/predict_NHANES/'

fea_dat <- read_xlsx(fea_dat_path) %>% 
  select(pt_id, Gender) %>% 
  filter(Gender == 0)
age_gap <- read.delim(
  file = age_gap_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
)
hrt_dat <- read.delim(
  file = HRT_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
) %>% rename(pt_id=1)
# process -----------------------------------------------------------------

pdata <- merge(age_gap, hrt_dat, by='pt_id') %>% 
  mutate(HRT=ifelse(is.na(HRT), 0, HRT)) %>% 
  filter(pt_id %in% fea_dat$pt_id) %>% 
  filter(Age>=50)

plt <- ggplot(pdata, aes(x = Age, fill = factor(HRT))) +
  geom_density(alpha = 0.5) +
  labs(title = "Age Density Plot by Group", x = "Age", y = "Density", fill = "Group") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()




plt <- ggplot(pdata, aes(Age, predict_age, color=factor(HRT))) +
  # geom_point() +
  geom_smooth(method = 'lm') +
  labs(x='Age', y='Age gap') +
  scale_color_manual(values = c('0'='#B8281D', '1'='#3773B6')) +
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
  filename = paste0(outpath, 'HRT_line.pdf'),
  plt,
  width = 3,
  height = 3
)

