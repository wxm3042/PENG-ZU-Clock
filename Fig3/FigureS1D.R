# -------------------------------------
# Date: Wed Jun  4 18:00:53 2025
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
conflict_prefer("select", "dplyr")

# detach(unload='conflicted')
# input -------------------------------------------------------------------

dat_path <- 'V20250427/output/Fig3/age_gaps_with_disease.xls'

outpath <- 'V20250427/output/Fig3/'

dat <- read.delim(
  file = dat_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
)
# process -----------------------------------------------------------------

cor_res <- cor(dat$age_gaps, dat$Age) %>% round(., 2)

plt <- ggplot(dat, aes(Age, age_gaps)) +
  geom_pointdensity(alpha=1) +
  scale_color_viridis() +
  annotate('text', x=80, y=-25, label=paste0('Pearson: ', cor_res)) +
  geom_smooth(method = 'lm') +
  labs(x='', y='') +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black'),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(colour = 'black', fill=NA),
    legend.position = 'none',
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
    )
ggsave(
  filename = paste0(outpath, 'FigureS1D.pdf'),
  plt,
  width = 3,
  height = 3
)
