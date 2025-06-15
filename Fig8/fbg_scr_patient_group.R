# -------------------------------------
# Date: Thu Jan 23 14:14:30 2025
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

threshold_path_list <- list(
  male='V20250427/output/Fig8/male_25_75_threshold_criterial.txt',
  female='V20250427/output/Fig8/female_25_75_threshold_criterial.txt'
)
all_fea_dat_path <- 'V20250427/output/Fig8/UKB_data_cor_feature.csv'
fea_norm_range <- list(
  male = list(
    FBG=c(4.1, 5.6),
    `Cys c`=c(0.41, 0.99),
    SCr=c(64, 104)
  ),
  female = list(
    FBG=c(4.1, 5.6),
    `Cys c`=c(0.41, 0.99),
    SCr=c(49, 90)
  )
)

outpath <- 'V20250427/output/Fig8/'

all_fea <- read.delim(
  file = all_fea_dat_path,
  sep = ',',
  quote = '',
  header = T,
  check.names = F
) %>% 
  mutate(gender_use=ifelse(Gender==0, 'female', 'male')) %>% 
  select(eid, Age, Gender, gender_use, everything())



# process -----------------------------------------------------------------

res_list <- list()
for(g in names(fea_norm_range)){
  th_dat <- read.delim(
    file = threshold_path_list[[g]],
    sep = '\t',
    quote = '',
    header = T,
    check.names = F
  )
  sub_fea_dat <- all_fea %>% 
    filter(gender_use == g)
  fea_list <- unique(th_dat$fea)
  
  for(fe in names(fea_norm_range[[g]])){
    
    sub_th_dat <- th_dat %>% 
      filter(fea == fe) %>% 
      select(1:3) %>% 
      rename(Age=1, C25=2, C75=3)
    
    
    norm_pt <- sub_fea_dat %>% 
      select(eid, all_of(fe), Age) %>% 
      rename(fea=2) %>% 
      filter(fea>fea_norm_range[[g]][[fe]][1]) %>% 
      filter(fea<fea_norm_range[[g]][[fe]][2]) %>% 
      merge(., sub_th_dat, by='Age') %>% 
      mutate(grp='Within_Range') %>% 
      mutate(grp=ifelse(fea<C25, 'Below_Range', grp)) %>% 
      mutate(grp=ifelse(fea>C75, 'Above_Range', grp)) %>% 
      mutate(fea=fe) %>% 
      mutate(gender=g)

    res_list[[paste0(g, fe)]] <- norm_pt
  }
  
  
}
res_df <- bind_rows(res_list)

write.table(
  res_df,
  file = paste0(outpath, 'FBG_SCr_RI_group.txt'),
  sep = '\t',
  quote = F,
  row.names = F
)





