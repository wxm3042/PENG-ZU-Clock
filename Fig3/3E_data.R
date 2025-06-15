# -------------------------------------
# Date: Mon Apr 21 00:42:22 2025
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

raw_dat_path_list <- list(
  bj='V20250427/output/use_dat/bj/correlation_dat_with_ic_score.xlsx',
  cs='V20250427/output/use_dat/cs/correlation_dat_with_ic_score.xlsx'
)
age_gap_path <- 'V20250427/output/Fig3/age_gaps_with_disease.xls'

outpath <- 'V20250427/output/Fig3/'

age_gap <- read.delim(
  file = age_gap_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
) %>% select(pt_id, type, Age)

# process -----------------------------------------------------------------

sub_dat_list <- list()
for(g in names(raw_dat_path_list)){
  
  sub_dat_list[[g]] <- read_excel(raw_dat_path_list[[g]]) %>% 
    select(pt_id, Gender, ic)
  
}
sub_dat <- bind_rows(sub_dat_list) %>% 
  merge(., age_gap, by='pt_id') %>% 
  select(-pt_id) %>% 
  # filter(!is.na(ic)) %>% 
  rename(score='ic', A2='Gender', A4='Age') %>% 
  mutate(A2=ifelse(A2==1, '男', '女'))

write.csv(
  sub_dat,
  paste0(outpath, '3E_data.csv')
)

