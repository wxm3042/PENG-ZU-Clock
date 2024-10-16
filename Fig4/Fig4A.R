# -------------------------------------
# Date: Tue Jul 16 09:21:11 2024
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(readxl)
library(ggsci)
library(writexl)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

dat_path <- 'output/3.clock/changsha_clock/elastic_net_scale/merge_bj_cs/questionary_features_cor_with_delta_age_all.xlsx'

cl_list <- pal_npg("nrc", alpha = 0.7)(9)

outpath <- 'output/0.figure/figure4/merge/'

psort <- c(
  'Basic information',
  'Life style',
  'Physical condition',
  'Medical treatment and illness',
  'Psychology',
  'Financial situation',
  'Social networks and support',
  'Living condition',
  'Cognitive appraisal'
)
# process -----------------------------------------------------------------



dat <- read_excel(dat_path) %>%
  filter(!(group %in% 'Satisfaction with Community Environment'))

pdata_raw <- dat %>%
  filter(grepl('feature', feature)) %>%
  select(class, group, feature, pval) %>%
  mutate(logp = -log10(pval)) %>%
  mutate(xname = paste0(group, feature)) %>%
  arrange(class, pval) %>%
  mutate(xname = factor(xname, xname)) %>%
  mutate(class = factor(class, unique(class)))


pdata <- pdata_raw %>% 
  mutate(class=factor(class, psort)) %>% 
  arrange(class) %>% 
  mutate(xname = factor(xname, xname))

names(cl_list) <- psort

saveRDS(cl_list, 'output/0.figure/figure4/class_col.rds')

plt <- ggplot(pdata, aes(xname, logp, fill = class)) +
  geom_bar(stat = 'identity') +
  scale_fill_manual(values = cl_list) +
  ylim(-5, 5.5) +
  geom_hline(
    yintercept = -log10(0.05),
    linetype = "dashed",
    color = "red",
    linewidth = 1
  ) +
  coord_polar(start = 0) +
  labs(x = '', y = '') +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1, 4), "cm")
  )

ggsave(
  filename = paste0(outpath, 'questionary_circle_barplot_all.pdf'),
  plt,
  width = 8,
  height = 8
)

