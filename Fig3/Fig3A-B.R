# -------------------------------------
# Date: Mon Jul 15 17:28:47 2024
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(readxl)
library(ggpubr)
library(writexl)
library(scales)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------
dat_path <- 'output/3.clock/changsha_clock/elastic_net_scale/merge_bj_cs/age_gaps.xlsx'
pt_disease_path <- 'output/3.clock/changsha_clock/elastic_net_scale/merge_bj_cs/all_disease_pt.xls'
pt_disease_mtx_path <- 'data/bj_cs_disease_mtx.xlsx'
disease_numb_path <- 'output/3.clock/changsha_clock/elastic_net_scale/merge_bj_cs/disease_numb.xls'

outpath <- 'output/0.figure/figure3/merge/'

pt_disease <- read.delim(
  file = pt_disease_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
) %>% pull(1)

dat <- read_excel(dat_path) %>% 
  mutate(disease=ifelse(pt_id %in% pt_disease, 'y', 'n'))

disease_numb <- read.delim(
  file = disease_numb_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
) %>% select(pt_id, Freq, grp) %>% 
  rename(disease_num=2, disease_num_grp=3)

pt_disease_mtx <- read_excel(pt_disease_mtx_path, )
# sigma 1 plot ------------------------------------------------------------

sigma_val <- sd(dat$age_gaps)

dat2 <- dat %>% 
  mutate(type=ifelse(age_gaps>sigma_val, 'Accelerated', 'Normal')) %>% 
  mutate(type=ifelse(age_gaps< -sigma_val, 'Decelerated', type)) %>% 
  merge(., disease_numb, by='pt_id')
table(dat2$type)

plt <- ggplot(dat2, aes(age, predict_age, color=type)) +
  geom_point(size=.5) +
  scale_color_manual(values = c(Accelerated='darkred', Slowdown='darkblue', Normal='grey')) +
  labs(x='Chronological age(years)', y='Predicted age(years)') +
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
  filename = paste0(outpath, 'cor_plot_sigma.pdf'),
  plt,
  width = 4,
  height = 4
)

# disease_numb bar

pdata <- dat2 %>% 
  # filter(disease_num>0) %>% 
  mutate(disease_num_grp=factor(disease_num_grp, c('n=0','n=1','n=2','n=3','n=4','n>=5')))

plt <- ggboxplot(
  pdata,
  x='disease_num_grp',
  y='age_gaps',
  color='disease_num_grp',
  palette = seq_gradient_pal("lightblue", "blue", "Lab")(seq(0, 1, length.out = 6)),
  size=.8,
  outlier.size = .8
) +
  stat_compare_means(size=4) +
  labs(x='Number of Diseases', y='Delta age', color='') +
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
  filename = paste0(outpath, 'disease_num_boxplot.pdf'),
  plt,
  width = 4,
  height = 4
)

