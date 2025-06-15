# -------------------------------------
# Date: Fri Apr 25 11:34:13 2025
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
library(ggpubr)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

dat_path <- 'V20250427/output/Fig9/cs_normal_train/iter1000_coefficients_matrix.xlsx'
freq_path <- 'V20250427/output/Fig9/cs_normal_train/iter1000_fea_freq.xlsx'

outpath <- 'V20250427/output/Fig9/cs_normal_train/'

freq_df <- read_excel(freq_path) %>% 
  mutate(ratio=Count/1000) %>% 
  select(-Count) %>% 
  rename(fea=1)
dat <- read_excel(dat_path)
# process -----------------------------------------------------------------

mean_coef <- apply(dat, 2, mean) %>% 
  data.frame(coef_m=.) %>% 
  rownames_to_column('fea') %>% 
  arrange(coef_m)

pdata <- dat %>% 
  pivot_longer(., everything(), names_to = 'fea', values_to = 'coefficient') %>% 
  mutate(fea=factor(fea, mean_coef$fea))

plt <- ggboxplot(
  pdata, x = "fea", 
  y = "coefficient"
) +
  stat_compare_means()


pdata2 <- merge(freq_df, mean_coef, by='fea') %>% 
  mutate(rank=abs(ratio*coef_m)) %>% 
  arrange(desc(rank)) %>% 
  mutate(fea=factor(fea, fea))
write_xlsx(
  pdata2, 
  paste0(outpath, 'feature_rank.xlsx')
)

plt <- ggplot(pdata2, aes(fea, rank)) +
  geom_bar(stat='identity') +
  labs(x='', y='') +
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

