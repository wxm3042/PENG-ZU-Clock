# -------------------------------------
# Date: Mon Sep 30 18:05:01 2024
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
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------
dat_path <-
  'output/3.clock/changsha_clock/elastic_net_scale/test_beijing/slowdown_vs_accelerate/metabolite/sig_deg_p0.05.xlsx'
class_path <- 'output/0.figure/figure5/old/bj/show_metabolite_class.txt'

outpath <- 'output/0.figure/figure5/beijing_data_res/mtb/'

deg_res <- read_excel(dat_path) %>% 
  mutate(Class=ifelse(is.na(Class), 'None', Class))

# process -----------------------------------------------------------------

class_ord <- table(deg_res$Class) %>% 
  data.frame() %>% 
  arrange(Freq) %>% 
  filter(Var1!='None')

pdata <- deg_res %>%
  mutate(Class = factor(Class, c('None', class_ord$Var1 %>% as.character()))) %>%
  arrange(Class, beta) %>%
  mutate(name = factor(name, name))

plt <- ggdotchart(
  pdata,
  x = "name",
  y = "beta",
  color = "Class",
  sorting = "desc",
  group = "Class",
  # Sort value in descending order
  rotate = TRUE,
  # Rotate vertically
  dot.size = 2,
  # Large dot size
  y.text.col = TRUE,
  # Color y text by groups
  ggtheme = theme_pubr()                        # ggplot2 theme
) +
  # theme_cleveland()
  geom_hline(yintercept = 0)

ggsave(
  filename = paste0(outpath, 'metabolite_class_dotplot.pdf'),
  plt,
  width = 6,
  height = 6
)


