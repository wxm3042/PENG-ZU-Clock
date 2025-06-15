# -------------------------------------
# Date: Tue Apr 29 17:04:41 2025
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
library(viridis)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("select", "dplyr")

# detach(unload='conflicted')
# input -------------------------------------------------------------------

dat_path <- 'V20250427/output/Fig9/cs_normal_train/select_model/cor_res.xlsx'
rank_dat_path <- 'V20250427/output/Fig9/cs_normal_train/feature_rank.xlsx'

outpath <- 'V20250427/output/Fig9/'

dat <- read_excel(dat_path) %>% 
  mutate(model_name=factor(model_name, model_name)) %>% 
  mutate(numb=row_number())
rank_dat <- read_excel(rank_dat_path) %>% 
  mutate(fea=factor(fea, rev(fea)))
# process -----------------------------------------------------------------

plt <- ggplot(dat, aes(numb, correlation)) +
  geom_point() +
  labs(x='Top features', y='Correlation coefficients') +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black'),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(colour = 'black', fill=NA),
    legend.position = 'top',
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
    )
ggsave(
  filename = paste0(outpath, '9A_top_features_correlation_dotplot.pdf'),
  plt,
  width = 3,
  height = 3
)



plt <- ggplot(dat, aes(numb, MAE)) +
  geom_point() +
  labs(x='Top features', y='Correlation coefficients') +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black'),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(colour = 'black', fill=NA),
    legend.position = 'top',
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  )
ggsave(
  filename = paste0(outpath, '9A_top_features_MAE_dotplot.pdf'),
  plt,
  width = 3,
  height = 3
)

rank_dat_sub <- rank_dat %>% 
  head(n=11) %>% 
  mutate(abs_coef=abs(coef_m))
plt <- ggplot(rank_dat_sub, aes(fea, abs_coef, fill=abs_coef)) +
  geom_bar(stat='identity', color='black') +
  scale_fill_viridis(discrete = F) +
  labs(x='', y='') +
  coord_flip() +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black'),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(colour = 'black', fill=NA),
    legend.position = 'top',
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
    )

ggsave(
  filename = paste0(outpath, '9A_rank_bar.pdf'),
  plt,
  width = 4,
  height = 4
)


