# -------------------------------------
# Date: Tue Jun 10 17:46:06 2025
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(readxl)
library(gamlss)
library(writexl)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

pt_id_path_list <- list(
  BJ='V20250427/output/use_dat/bj/clock_data.xlsx',
  CS='V20250427/output/use_dat/cs/clock_data.xlsx'
)
dat_path <- 'V20250427/output/use_dat/common_fea_for_correlation.xlsx'
type_path <- 'V20250427/output/Fig2/age_correlation/common_fea_for_correlation_pie.xlsx'
ranger_dat_path <- 'V20250427/output/use_dat/merge_data.xlsx'
age_gap_path <- 'V20250427/output/Fig3/age_gaps_with_disease.xls'

outpath <- 'V20250427/output/Fig7/gender/'

dat <- read_excel(dat_path)
type_df <- read_excel(type_path)


# 定义年龄段
breaks <- c(-Inf, 30, seq(40, 55, by = 10), seq(60, 75, by = 5), 80, Inf)
labels <- c("<30", "30-39", "40-49", "50-59", "60-64", "65-69", "70-74", "75-79", ">=80")
age_gap <- read.delim(
  file = age_gap_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
)

keep_id <- age_gap %>% 
  filter(type != 'Accelerated') %>% 
  pull(pt_id)

# pt ----------------------------------------------------------------------

pt_id_list <- list()
for(g in names(pt_id_path_list)){
  pt_id_list[[g]] <- read_excel(pt_id_path_list[[g]]) %>% 
    pull(pt_id)
  
}
pt_id_list <- unlist(pt_id_list)

range_dat_raw <- read_excel(ranger_dat_path) %>% 
  mutate(gender_use=ifelse(Gender==0, 'F', 'M')) %>% 
  select(pt_id, Gender, Age, gender_use, everything()) %>% 
  filter(pt_id %in% pt_id_list) %>% 
  filter(pt_id %in% keep_id)


# process -----------------------------------------------------------------


show_fea <- type_df %>% 
  filter(short_name %in% dat$fea) %>% 
  group_by(group)

show_type_list <- unique(show_fea$group)

for(gen in c('male', 'female')){

  
  if(gen =='male'){
    range_dat <- range_dat_raw %>% 
      filter(gender_use=='M')
  }else{
    range_dat <- range_dat_raw %>% 
      filter(gender_use=='F')
  }
  
  tmp_res_list <- list()
  for(fea in show_fea$short_name){
    print(fea)
    range_dat$age_grp <- cut(range_dat$Age, breaks = breaks, labels = labels, right = FALSE)
    
    pt_numb <- table(range_dat$age_grp) %>% data.frame() %>% 
      rename(group=1, n=2)
    
    age_gaps_res_list <- list()
    for(l in labels){
      
      subres <- range_dat %>% 
        filter(age_grp==l) %>% 
        pull(all_of(fea)) %>% 
        quantile(., probs = c(0.01, 0.03, 0.05, 0.25, 0.50, 0.75, 0.95, 0.97, 0.99), na.rm=T) %>% 
        data.frame(val=.) %>% 
        t() %>% data.frame(., check.names = F) %>% 
        remove_rownames() %>% 
        mutate(group=l)
      age_gaps_res_list[[l]] <- subres
    }
    
    tmp_res_list[[fea]] <- bind_rows(age_gaps_res_list) %>%
      merge(., pt_numb, by = 'group') %>%
      mutate(feature = fea) %>%
      select(feature, group, n, everything()) %>%
      mutate(group=factor(group, labels)) %>% 
      arrange(group) %>% 
      add_row(.)
  }
  all_res <- bind_rows(tmp_res_list) 
  write_xlsx(all_res, paste0(outpath, gen, '_centile_stat.xlsx'))
  
}


