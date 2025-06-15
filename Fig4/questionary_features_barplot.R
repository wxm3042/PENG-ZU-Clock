# -------------------------------------
# Date: Tue Apr 22 13:41:56 2025
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
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("select", "dplyr")

# detach(unload='conflicted')
# input -------------------------------------------------------------------

annot_path <- 'V20250427/data/副本北京医院2596例final原表_full-矫正_250321更新组别命名.xlsx'
col_path <- 'V20250427/output/Fig4/class_col.rds'
dat_path <- 'V20250427/output/Fig4/questionary_features_sig_cor_annot.xlsx'

outpath <- 'V20250427/output/Fig4/'

col_list <- readRDS(col_path)

# process -----------------------------------------------------------------

dat_df <- read_excel(dat_path)

pdata <- dat_df %>% 
  # filter(!(group %in% rm_grp)) %>% 
  filter(!(choose_id %in% c('Not applicable', 'Not clear', 'Other', 'Student', 'Others'))) %>% 
  mutate(id=ifelse(is.na(choose_id), short_name, paste0(short_name, '_', choose_id))) %>% 
  select(id, class, group, Estimate)

grp_ord <- pdata %>% 
  select(group, Estimate) %>% 
  mutate(abs_Estimate=abs(Estimate)) %>% 
  group_by(group) %>%
  slice_max(n=1, order_by=abs_Estimate) %>% 
  arrange(Estimate)

class_ord <- pdata %>% 
  group_by(class) %>% 
  mutate(ord=abs(Estimate)) %>% 
  slice_max(n=1, order_by=ord) %>% 
  arrange(ord) %>% 
  pull(class)

pdata_ord <- pdata  %>% 
  mutate(class=factor(class, class_ord)) %>% 
  mutate(group=factor(group, grp_ord$group)) %>% 
  arrange(class, group, Estimate) %>% 
  mutate(id=factor(id, id))

write_xlsx(
  pdata_ord,
  paste0(outpath, 'sig_questionary_features_barplot.xlsx')
)

plt <- ggplot(pdata_ord, aes(id, Estimate, fill=class)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values = col_list) +
  labs(x='', y='') +
  coord_flip() +
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
  filename = paste0(outpath, 'sig_questionary_features_barplot.pdf'),
  plt,
  width = 8,
  height = 14
)

