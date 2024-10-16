# -------------------------------------
# Date: Mon Jul 15 00:15:00 2024
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
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

dat_path <- 'output/3.clock/changsha_clock/elastic_net_scale/simplify_clock/iter1000_fea_freq.xlsx'
annot_path <- 'data/beijing/副本北京医院2596例final原表_full-矫正.xlsx'
change_path <- 'data/rename_feature_name.xlsx'

outpath <- 'output/3.clock/changsha_clock/elastic_net_scale/simplify_clock/'

dat <- read_excel(dat_path) %>% 
  rename(id=1)
annot <- read_excel(annot_path, sheet = 2) %>% 
  filter(keep=='y') %>% 
  select(id, new_name)
rename_df <- read_excel(change_path) %>% 
  rename(id=1) %>% 
  select(-short)

# process -----------------------------------------------------------------

pdata <- merge(dat, annot, by='id') %>% 
  merge(., rename_df, by.x='new_name', by.y='id', all.x = T) %>% 
  mutate(new_name=ifelse(!is.na(name), name, new_name)) %>%  
  select(-name) %>% 
  arrange(Count) %>% 
  mutate(new_name=factor(new_name, rev(new_name)))


plt <- ggplot(pdata, aes(new_name, Count)) +
  geom_bar(stat='identity') +
  # coord_flip() +
  labs(x='', y='Frequency') +
  theme_bw() +
  theme(
      legend.key.size = unit(.1, 'inches'),
      text = element_text(size = 12, color = 'black'),
      axis.text.x = element_text(size = 12, color = 'black', angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 12, color = 'black'),
      axis.ticks = element_line(color = 'black'),
      panel.border = element_rect(color = 'black'),
      legend.position = 'top',
      panel.grid = element_blank()
  )
ggsave(
  filename = paste0(outpath, 'iter1000_fea_barplot.pdf'),
  plt,
  width = 8,
  height = 4
)












