# -------------------------------------
# Date: Mon Jul  8 16:17:06 2024
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
# detach(unload='conflicted')
# input -------------------------------------------------------------------


dat_path <- 'V20250427/output/Fig4/questionary_features_cor_with_delta_age_all.xlsx'
annot_path <- 'V20250427/data/beijing/副本北京医院2596例final原表_full-矫正.xlsx'

outpath <- 'V20250427/output/Fig4/'

dat <- read_excel(dat_path)
annot_df <- read.xlsx(annot_path, sheet='q_fea') %>% 
  fill(name, group)

# process -----------------------------------------------------------------

annot_df_short <- annot_df %>% 
  select(group, short_id, choose, choose_annot) %>% 
  mutate(choose=paste0('feature', as.character(choose))) %>% 
  rename(feature=3)

annot_res <- dat %>% 
  filter(grepl('^feature.*', feature)) %>% 
  filter(pval<0.05) %>% 
  merge(., annot_df_short, all.x = T, by=c('group', 'feature')) %>% 
  rename(choose_id=11) %>% 
  filter(!(feature!='feature' & is.na(choose_id)))

write_xlsx(
  annot_res,
  paste0(outpath, 'questionary_features_sig_cor_annot.xlsx')
)





