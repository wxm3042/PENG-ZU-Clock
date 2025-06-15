# -------------------------------------
# Date: Fri Apr 18 01:20:31 2025
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

dat_path <- 'V20250427/output/Fig2/clock/cs_normal_train/elasticNet_coefficients.xlsx'

outpath <- 'V20250427/output/Fig2/clock/'

dat <- read_excel(dat_path) %>% 
  filter(Coefficient!=0) %>% 
  mutate(type=ifelse(Coefficient>0, 'Positive', 'Negative')) %>% 
  mutate(id=factor(id, id))

# process -----------------------------------------------------------------

plt <- ggplot(dat, aes(id, Coefficient, fill=type)) +
  geom_bar(stat='identity', color='black') +
  scale_fill_manual(values = c(Positive='#B8281D', Negative='#3773B6')) +
  labs(x='', y='Regression Coefficient') +
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
  filename = paste0(outpath, '2D_Coefficient_barplot.pdf'),
  plt,
  width = 8,
  height = 4
)





