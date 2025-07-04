# -------------------------------------
# Date: Fri Apr 18 01:30:23 2025
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


dat_path <- 'V20250427/output/Fig2/clock/age_gaps_related_fea_sig_p0.05.xlsx'

outpath <- 'V20250427/output/Fig2/clock/'

dat <- read_excel(dat_path)


# process -----------------------------------------------------------------

pdata <- dat %>% 
  arrange(estimate) %>% 
  mutate(fea=factor(fea, fea)) %>% 
  mutate(change=ifelse(estimate>0, 'pos', 'neg'))

plt <- ggdotchart(
  pdata,
  x = "fea",
  y = "estimate",
  color = "change",
  # Color by groups
  palette = c("#3773B6", "#B8281D"),
  # Custom color palette
  sorting = "descending",
  # Sort value in descending order
  rotate = TRUE,
  # Rotate vertically
  dot.size = 2,
  # Large dot size
  y.text.col = TRUE,
  # Color y text by groups
  ggtheme = theme_pubr()                        # ggplot2 theme
) +
  theme_cleveland()                                      # Add dashed grids

ggsave(
  filename = paste0(outpath, '2F_agegap_rel_fea_dotplot_without_WHR_CystatinC.pdf'),
  plt,
  width = 5,
  height = 10
)

