# -------------------------------------
# Date: Fri May 23 12:19:27 2025
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
conflict_prefer("select", "dplyr")

# detach(unload='conflicted')
# input -------------------------------------------------------------------

dat_path <- 'output/3.clock/changsha_clock/elastic_net_scale/algorithm_select/test_bj/res.xlsx'

outpath <- 'V20250427/output/Fig2/clock/'

dat <- read_excel(dat_path) %>% 
  mutate(algorithm=factor(algorithm, algorithm))
# process -----------------------------------------------------------------

plt <- ggplot(dat, aes(algorithm, cor, fill=algorithm)) +
  geom_bar(stat='identity') +
  labs(x='', y='') +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black', angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(colour = 'black', fill=NA),
    legend.position = 'none',
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
    )
ggsave(
  filename = paste0(outpath, 'algorithm_select.pdf'),
  plt,
  width = 4,
  height = 4
)