# -------------------------------------
# Date: Wed Apr  2 16:16:09 2025
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

library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

data_path_list <- 'V20250427/output/Fig5/UKB/predict_UKB/predict_age_in_test_data.xlsx'

outpath <- 'V20250427/output/Fig5/UKB/predict_UKB/'

# process -----------------------------------------------------------------

dat <- read_excel(data_path_list) %>% 
  mutate(deage=predict_age-Age)

cor_res <- cor(dat$predict_age, dat$Age) %>% 
  round(., 2)

mae <- dat %>% 
  mutate(aa=abs(Age-predict_age)) %>% 
  pull(aa) %>% mean() %>% 
  round(., 2)

plt <- ggplot(dat, aes(Age, predict_age)) +
  geom_pointdensity(alpha=0.5, size=2) +
  scale_color_viridis() +
  annotate('text', x=65, y=10, label=paste0('Pearson: ', cor_res, '\nMAE: ', mae)) +
  geom_smooth(method = 'lm', color = "red") +
  labs(x='Chronological age (years)', y='Biological age (years)') +
  ylim(0,200) +
  theme_bw() +
  theme(
    legend.key.size = unit(.5, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black'),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(color = 'black'),
    legend.position = 'none',
    panel.grid = element_blank()
  )

ggsave(
  filename = paste0(outpath, 'cor_plot.tiff'),
  plt,
  device = "tiff",
  compression = "lzw",
  dpi = 3000,
  width = 4,
  height = 3.5
)





