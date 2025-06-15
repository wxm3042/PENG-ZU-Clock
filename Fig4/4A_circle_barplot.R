# -------------------------------------
# Date: Mon May  5 17:44:46 2025
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

dat_path <- 'V20250427/output/Fig4/questionary_features_cor_with_delta_age_all.xlsx'
col_path <- 'V20250427/data/class_col.rds'



outpath <- 'V20250427/output/Fig4/'
# cl_list <- readRDS(col_path)

# process -----------------------------------------------------------------


dat <- read_excel(dat_path) %>% 
  filter(!(group %in% 'Satisfaction with Community Environment'))

pdata <- dat %>% 
  filter(grepl('feature', feature)) %>% 
  select(class, group, feature, pval) %>% 
  mutate(logp=-log10(pval)) %>% 
  mutate(xname=paste0(group, feature)) %>% 
  arrange(class, pval) %>% 
  mutate(xname=factor(xname, xname)) %>% 
  mutate(class=factor(class, unique(class)))

cl_list <- pal_npg("nrc", alpha = 0.7)(length(unique(pdata$class)))
names(cl_list) <- unique(pdata$class)

saveRDS(cl_list, paste0(outpath, 'class_col.rds'))

plt <- ggplot(pdata, aes(xname, logp, fill=class)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values = cl_list) +
  ylim(-5,5.5) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red", linewidth = 1) +
  coord_polar(start = 0) +
  labs(x='', y='') +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) 

ggsave(
  filename = paste0(outpath, '4A_circle_barplot_all.pdf'),
  plt,
  width = 8,
  height = 8
)


