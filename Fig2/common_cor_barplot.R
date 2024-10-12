# -------------------------------------
# Date: Fri Jul  5 01:30:15 2024
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

dat_path <- 'output/0.figure/figure2/merge_correlation.xlsx'
change_path <- 'data/rename_feature_name.xlsx'

outpath <- 'output/0.figure/figure2/'

dat <- read_excel(dat_path)
rename_df <- read_excel(change_path) %>% 
  rename(id=1) %>% 
  select(-short)
# process -----------------------------------------------------------------

pdata <- dat %>% 
  filter(sig=='y') %>% 
  merge(., rename_df, by='id', all.x = T) %>% 
  mutate(id=ifelse(!is.na(name), name, id)) %>%  
  select(-name) %>% 
  arrange(col) %>% 
  mutate(id=factor(id, id)) %>% 
  select(id, correlation_bj, correlation_cs) %>% 
  pivot_longer(., -1, names_to = 'grp', values_to = 'corr') %>% 
  mutate(grp=gsub('correlation_','',grp)) %>% 
  mutate(grp=gsub('bj','BJ sub-cohort',grp)) %>% 
  mutate(grp=gsub('cs','CS sub-cohort',grp))

plt <- ggplot(pdata, aes(id, corr, fill=corr)) +
  geom_bar(stat='identity', color='black') +
  scale_fill_gradient2(low='#516db2', mid='white', high='#b75388') +
  labs(x='', y='') +
  facet_grid(~grp) +
  coord_flip() +
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
  filename = paste0(outpath, 'common_cor_barplot.pdf'),
  plt,
  width = 6,
  height = 6.5
)






