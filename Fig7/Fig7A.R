# -------------------------------------
# Date: Mon Sep 30 14:07:58 2024
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

dat_path <- 'output/3.clock/changsha_clock/elastic_net_scale/test_beijing/slowdown_vs_accelerate/common_id.xlsx'

outpath <- 'output/0.figure/figure5/beijing_data_res/'

dat <- read_excel(dat_path) %>% 
  mutate(type=gsub('Slowdown','Decelerated',type))
# process -----------------------------------------------------------------

plt <- ggplot(dat, aes(x=age, col=type)) +
  geom_density() +
  labs(x='Chronological age(years)', y='Density') +
  scale_color_manual(values = c(Accelerated='#b75388', Decelerated='#516db2')) +
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
  filename = paste0(outpath, 'denisty.pdf'),
  plt,
  width = 4.5,
  height = 4.5
)



