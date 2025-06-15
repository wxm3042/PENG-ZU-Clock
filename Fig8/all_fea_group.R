# -------------------------------------
# Date: Sun Apr 27 12:47:05 2025
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

threshold_path <- 'V20250427/output/Fig8/all_25_75_threshold_criterial.txt'
all_fea_dat_path <- 'V20250427/output/Fig8/UKB_data_cor_feature.csv'
fea_norm_range_path <- 'V20250427/data/feature_ref_range2.xlsx'
outpath <- 'V20250427/output/Fig8/'

fea_norm_range <- read_excel(fea_norm_range_path) %>% filter(!is.na(low))

all_fea <- read.delim(
  file = all_fea_dat_path,
  sep = ',',
  quote = '',
  header = T,
  check.names = F
)

th_dat <- read.delim(
  file = threshold_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
)

fea_list <- unique(th_dat$fea)
# process -----------------------------------------------------------------

res_list <- list()
for(fe in fea_norm_range$feature %>% unique()){
  print(fe)
  
  if(fe == 'IGF'){
    sub_th_dat <- th_dat %>% 
      filter(fea == fe) %>% 
      select(1:3) %>% 
      rename(Age=1, C25=2, C75=3)
    
    range_norm <- fea_norm_range %>% 
      filter(feature==fe) %>% 
      filter(age_low>30) %>% 
      mutate(low=low*7.65) %>% 
      mutate(high=high*7.65) %>% 
      rowwise() %>%
      mutate(age = list(seq(age_low, age_high))) %>%
      unnest(age) %>% 
      select(low, high, age) %>% 
      rename(Age=3)
    
    norm_pt <- all_fea %>% 
      select(eid, all_of(fe), Age) %>% 
      rename(fea=2) %>%
      # filter(fea>low_range) %>% 
      # filter(fea<high_range) %>% 
      merge(., sub_th_dat, by='Age') %>% 
      mutate(grp_new='out') %>% 
      mutate(grp_new=ifelse(fea>C25 & fea<C75, 'in', grp_new)) %>% 
      merge(., range_norm, by='Age') %>% 
      mutate(grp = 'out') %>% 
      mutate(grp = ifelse(fea>low & fea<high, 'in', grp)) %>% 
      select(eid, grp, grp_new)
    
    colnames(norm_pt) <- c('eid', fe, paste0(fe, '_new'))
    
  }else{
    sub_th_dat <- th_dat %>% 
      filter(fea == fe) %>% 
      select(1:3) %>% 
      rename(Age=1, C25=2, C75=3)
    
    low_range <- fea_norm_range %>% 
      filter(feature==fe) %>% 
      pull(low)
    high_range <- fea_norm_range %>% 
      filter(feature==fe) %>% 
      pull(high)
    
    norm_pt <- all_fea %>% 
      select(eid, all_of(fe), Age) %>% 
      rename(fea=2) %>%
      # filter(fea>low_range) %>% 
      # filter(fea<high_range) %>% 
      merge(., sub_th_dat, by='Age') %>% 
      mutate(grp_new='out') %>% 
      mutate(grp_new=ifelse(fea>C25 & fea<C75, 'in', grp_new)) %>% 
      mutate(grp = 'out') %>% 
      mutate(grp = ifelse(fea>low_range & fea<high_range, 'in', grp)) %>% 
      select(eid, grp, grp_new)
    
    colnames(norm_pt) <- c('eid', fe, paste0(fe, '_new'))
    
  }
  
  
  res_list[[fe]] <- norm_pt
  
}


res_df <- Reduce(function(x, y)
  merge(x, y , by = 'eid'), res_list)

write.table(
  res_df,
  file = paste0(outpath, 'all_fea_group.txt'),
  sep = '\t',
  quote = F,
  row.names = F
)

