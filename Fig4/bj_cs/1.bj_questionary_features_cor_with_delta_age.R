# -------------------------------------
# Date: Sun May 11 18:34:58 2025
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


dat_path_list <- list(
  bj='V20250427/data/beijing/副本北京医院2596例final原表_full-矫正.xlsx',
  cs='V20250427/data/changsha/changsha_colnamedf.xlsx'
)

age_path <- 'V20250427/output/Fig2/clock/age_gaps.xlsx'

exp_path_list <- list(
  bj='V20250427/data/beijing/beijing_add_eGFR_corrected.xlsx',
  cs='V20250427/data/changsha/changsha_add_eGFR_corrected.xlsx'
)

sport_list <- c('Aerobic Exercise','Balance Exercises','Strength Training','Flexibility Training','Ball Sports')

outpath <- 'V20250427/output/Fig4/bj_cs/'

samp_info_dat_bj <- read_excel(dat_path_list[['bj']], sheet=4) %>% 
  filter(keep=='y')
samp_info_dat_cs <- read_excel(dat_path_list[['cs']], sheet=2) %>%
  filter(keep=='y')

group_annot <- samp_info_dat_bj %>% 
  select(group, short_name, name, class) %>% 
  filter(group %in% samp_info_dat_cs$group) %>% 
  filter(!(group %in% sport_list))

expdat_bj <- read_excel(exp_path_list[['bj']])


delta_age <- read_excel(age_path) %>% 
  rename(samp_id=1) %>% 
  select(samp_id, age_gaps) %>% 
  rename(pt_id=1)
annot_df <- read_excel(dat_path_list[['bj']], sheet='q_fea') %>% 
  fill(name, group)
# process -----------------------------------------------------------------

res_list <- list()
for(i in group_annot$group){
  
  bj_id_use <- samp_info_dat_bj %>% 
    filter(group %in% i) %>% 
    pull(id)
  

  
  # merge pdata
  pdata_bj <- expdat_bj %>% 
    select(pt_id, all_of(bj_id_use), age, gender) %>% 
    rename(feature=2) %>% 
    mutate(sour='bj')

  
  if(i == 'Health Insurance Plans Participated In'){
    bj_cs_pdata <- pdata_bj %>% 
      merge(., delta_age, by='pt_id') %>% 
      filter(feature!=6) %>% 
      filter(feature!='1,5') %>% 
      mutate(feature=ifelse(feature=='1,2', '1', feature)) %>% 
      mutate(feature=ifelse(feature=='1,2,4', '1', feature)) %>% 
      mutate(feature=ifelse(feature=='1,3', '3', feature)) %>% 
      mutate(feature=ifelse(feature=='1,4', '1', feature))
    
    
  }else if(i == 'Current Participation in Pension Insurance Programs'){
    bj_cs_pdata <- pdata_bj %>% 
      merge(., delta_age, by='pt_id') %>% 
      mutate(feature=ifelse(feature=='1,2', '2', feature)) %>% 
      mutate(feature=ifelse(feature=='1,2,4', '2', feature)) %>% 
      mutate(feature=ifelse(feature=='1,3', '1', feature)) %>% 
      mutate(feature=ifelse(feature=='1,4', '1', feature)) %>% 
      mutate(feature=ifelse(feature=='1,5', '1', feature))
    
  }else if(i == 'Education'){
    bj_cs_pdata <- pdata_bj %>% 
      merge(., delta_age, by='pt_id') %>% 
      mutate(feature=ifelse(feature %in% c(1:3),1, feature)) %>% 
      mutate(feature=ifelse(feature >5 ,5, feature))
    
  }else if(i == 'Marital status'){
    bj_cs_pdata <- pdata_bj %>% 
      merge(., delta_age, by='pt_id') %>% 
      mutate(feature=ifelse(feature == 3, 2, feature))
  }else if(i == 'Average Daily Interactions'){
    bj_cs_pdata <- pdata_bj %>% 
      merge(., delta_age, by='pt_id') %>% 
      mutate(feature=ifelse(feature == 2, 1, feature))
  }else if(i == 'Primary Source of Income (First)'){
    bj_cs_pdata <- pdata_bj %>% 
      merge(., delta_age, by='pt_id') %>% 
      mutate(feature=ifelse(feature < 3, 1, 10))
  }else{
    bj_cs_pdata <- pdata_bj %>% 
      merge(., delta_age, by='pt_id')
  }
  
  
  type <- samp_info_dat_bj %>% 
    filter(group %in% i) %>% 
    pull(type)
  
  if(type == 'categorical_variable'){
    
    fea_list_tmp <- filter(annot_df, group==gsub(' $','',i)) %>% pull(choose)
    
    pdata <- bj_cs_pdata %>%
      filter(!is.na(feature)) %>%
      filter(feature %in% fea_list_tmp) %>%
      mutate(feature = factor(feature, unique(.$feature) %>% sort()))
    
    fea_num <- table(pdata$feature) %>% 
      data.frame(.) %>% 
      rename(feature=1) %>% 
      mutate(feature=paste0('feature', feature)) %>% 
      mutate(total=sum(Freq)) %>% 
      mutate(ratio=paste0(Freq, '/', total)) %>% 
      select(feature, ratio)
    
    if(length(unique(pdata$feature))!=1){
      fit_res <- lm(age_gaps ~ feature + age + gender, data = pdata)
      
      coef_res <- summary(fit_res)$coef %>% 
        data.frame() %>% 
        rownames_to_column('feature') %>% 
        mutate(group=i) %>% 
        merge(., fea_num, by='feature', all.x=T) %>% 
        select(feature, ratio, everything())
    }else{
      next
    }
    
    
  }else{
    pdata <- bj_cs_pdata %>% 
      mutate(feature=as.numeric(feature)) %>% 
      filter(!is.na(feature))
    
    if(nrow(pdata)!=0){
      fit_res <- lm(age_gaps ~ feature + age + gender, data = pdata)
      
      coef_res <- summary(fit_res)$coef %>% 
        data.frame() %>% 
        rownames_to_column('feature') %>% 
        mutate(group=i) %>% 
        mutate(ratio=ifelse(feature=='feature', as.character(nrow(pdata)), NA)) %>% 
        select(feature, ratio, everything())
    }else{
      next
    }
    
  }
  
  res_list[[i]] <- coef_res
}

res_df <- bind_rows(res_list) %>% 
  rename(pval=6) %>% 
  arrange(group, pval) %>% 
  merge(., group_annot, by='group')

write_xlsx(
  res_df,
  paste0(outpath, 'bj_questionary_features_cor_with_delta_age_all.xlsx')
)


sig_group <- res_df %>% 
  filter(pval < 0.05) %>% 
  filter(feature != '(Intercept)') %>% 
  pull(group) %>% unique()

sig_res <- res_df %>% 
  filter(group %in% sig_group)

write_xlsx(
  sig_res,
  paste0(outpath, 'bj_questionary_features_cor_with_delta_age.xlsx')
)





