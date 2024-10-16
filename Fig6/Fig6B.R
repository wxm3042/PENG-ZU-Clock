# -------------------------------------
# Date: Mon Jul 22 01:57:07 2024
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(openxlsx)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

data_path_list <- 'output/3.clock/changsha_clock/elastic_net_scale/simplify_clock/merge/predict_age_in_test_data.xlsx'

outpath <- 'output/3.clock/changsha_clock/elastic_net_scale/simplify_clock/merge/'


# process -----------------------------------------------------------------


dat <- read.xlsx(data_path_list)
cor_res <- cor(dat$predict_age, dat$age) %>% 
  round(., 2)

mae <- dat %>% 
  mutate(aa=abs(age-predict_age)) %>% 
  pull(aa) %>% mean() %>% 
  round(., 2)

plt <- ggplot(dat, aes(age, predict_age)) +
  geom_point() +
  annotate('text', x=70, y=20, label=paste0('Pearson: ', cor_res, '\nMAE: ', mae)) +
  geom_smooth(method = 'lm') +
  labs(x='age', y='predict_age') +
  theme_bw() +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black'),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(color = 'black'),
    legend.position = 'top'
  )
ggsave(
  filename = paste0(outpath, 'cor_plot.pdf'),
  plt,
  width = 3,
  height = 3
)


