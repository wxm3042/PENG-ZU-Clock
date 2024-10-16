# -------------------------------------
# Date: Mon Sep 30 16:51:12 2024
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

dat_path <- 'output/0.figure/figure5/beijing_data_res/all_deg_pt.xlsx'

outpath <- 'output/0.figure/figure5/beijing_data_res/'

dat_df <- read_excel(dat_path)

# process -----------------------------------------------------------------

pdata <- dat_df %>% 
  mutate(type=ifelse(pval<0.05 & beta>0, 'up', 'none')) %>% 
  mutate(type=ifelse(pval<0.05 & beta<0, 'down', type)) %>% 
  mutate(logp=-log10(pval))

plt <- ggplot(pdata, aes(beta, logp)) +
  geom_point(data=subset(pdata, type=='up'), color='#b75388') +
  geom_point(data=subset(pdata, type=='down'), color='#516db2') +
  geom_point(data=subset(pdata, type=='none'), color='grey') +
  labs(x='Coefficient', y='-Log10(P value)') +
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
  filename = paste0(outpath, 'slowdown_vs_accelerate_volplot.pdf'),
  plt,
  width = 4,
  height = 4
)


