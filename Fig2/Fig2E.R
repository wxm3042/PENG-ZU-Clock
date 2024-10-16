# -------------------------------------
# Date: Fri Jul  5 01:39:03 2024
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

coef_path <- 'output/3.clock/changsha_clock/elastic_net_scale/train_changsha_common_with_BJ/elasticNet_coefficients.xlsx'
annot_path <- 'data/beijing/副本北京医院2596例final原表_full-矫正.xlsx'
outpath <- 'output/0.figure/figure2/'

annot_df <- read_excel(annot_path, sheet = 2) %>% 
  select(id, new_name)
coef_df <- read_excel(coef_path) %>% 
  filter(Coefficient !=0) %>% 
  merge(., annot_df, by='id', all.x=T) %>% 
  arrange(Coefficient) %>% 
  mutate(new_name=factor(new_name, new_name))
# process -----------------------------------------------------------------

plt <- ggplot(coef_df, aes(new_name, Coefficient, fill=Coefficient)) +
  geom_bar(stat = 'identity', color='black') +
  scale_fill_gradient2(low='darkblue', mid='white', high='darkred') +
  labs(x='', y='') +
  # coord_flip() +
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
  filename = paste0(outpath, 'train_coef_bar.pdf'),
  plt,
  width = 8,
  height = 5
)
