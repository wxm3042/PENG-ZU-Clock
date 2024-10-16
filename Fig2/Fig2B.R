# -------------------------------------
# Date: Fri Jul  5 12:35:34 2024
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(readxl)
library(patchwork)
library(writexl)
library(ggrepel)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

dat_path <- 'output/0.figure/figure2/merge_correlation.xlsx'
type_path <- 'output/2.deg/correlation_fea_common_type.xlsx'
exp_dat_path_list <- list(
  bj='output/2.deg/beijing/feature_for_correlation.xlsx',
  cs='output/2.deg/changsha/feature_for_correlation.xlsx'
)

outpath <- 'output/0.figure/figure2/'

type_df <- read_excel(type_path)
dat <- read_excel(dat_path)


# class order -------------------------------------------------------------

sort_dat <- dat %>% 
  select(id, col, sig) %>% 
  merge(., type_df, by='id') %>% 
  mutate(abs_col=abs(col)) %>% 
  arrange(type, desc(abs_col)) %>% 
  group_by(type)
  
sig_dat_cor <- sort_dat %>% 
  filter(sig=='y') %>% 
  select(type, abs_col) %>% 
  group_by(type) %>% 
  summarise(mean_cor=mean(abs_col)) %>% 
  arrange(desc(mean_cor)) %>% 
  mutate(rank_cor=min_rank(desc(mean_cor)))

sig_ratio <- sort_dat %>%
  select(type, sig) %>% 
  mutate(total_count = n()) %>%
  summarise(element_count = sum(sig == 'y'),
            proportion = element_count / total_count) %>%
  unique() %>% ungroup() %>%
  arrange(desc(proportion)) %>% 
  filter(element_count!=0) %>% 
  mutate(rank_prop=min_rank(desc(proportion)))

tmp_pdata <- merge(sig_ratio, sig_dat_cor, by='type') %>% 
  mutate(mean_rank=rowMeans(select(., starts_with('rank')))) %>% 
  arrange(mean_rank)

plt <- ggplot(tmp_pdata, aes(proportion, mean_cor, label=type, size=1/mean_rank, color=1/mean_rank)) +
  geom_point() +
  geom_text_repel(max.overlaps = Inf, size = 4) +
  scale_size_continuous(range = c(1, 10), limits = c(0, 1)) +
  coord_cartesian(xlim = c(0, max(tmp_pdata$proportion) + .1), ylim = c(0, max(tmp_pdata$mean_cor) + .1)) +
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
  filename = paste0(outpath, 'type_rank_dotplot.pdf'),
  plt,
  width = 4,
  height = 4
)






