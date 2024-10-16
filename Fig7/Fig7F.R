# -------------------------------------
# Date: Mon Sep 30 18:13:57 2024
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

dat_path <- 'output/3.clock/changsha_clock/elastic_net_scale/test_beijing/slowdown_vs_accelerate/metabolite/enrich/pathway_results.csv'

outpath <- 'output/0.figure/figure5/beijing_data_res/mtb/'

res <- read.csv(dat_path, check.names = F)
# process -----------------------------------------------------------------

pdata <- res %>% 
  rename(aa=1) %>% 
  select(1, 6) %>% 
  rename(pathway=1, logp=2) %>% 
  mutate(pathway=factor(pathway, rev(pathway)))

plt <- ggplot(pdata, aes(pathway, logp, fill=logp)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient(low='#ced5ec', high = '#4c6fbb') +
  coord_flip() +
  labs(x='', y='') +
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
  filename = paste0(outpath, 'metabolite_enrich_bar.pdf'),
  plt,
  width = 6,
  height = 4
)


