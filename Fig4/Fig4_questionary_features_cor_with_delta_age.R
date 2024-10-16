# -------------------------------------
# Date: Mon Jul  8 09:52:49 2024
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(openxlsx)
library(readxl)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

# annot_path <- 'data/questionary_features_annot.xlsx'

dat_path_list <- list(
  bj='data/beijing/副本北京医院2596例final原表_full-矫正.xlsx',
  cs='data/changsha/changsha_colnamedf.xlsx'
)

age_path <- 'output/3.clock/changsha_clock/elastic_net_scale/merge_bj_cs/age_gaps.xlsx'

exp_path_list <- list(
  bj='data/beijing/beijing_add_eGFR_corrected.xlsx',
  cs='data/changsha/changsha_add_eGFR_corrected.xlsx'
)

sport_list <- c('Aerobic Exercise','Balance Exercises','Strength Training','Flexibility Training','Ball Sports')

outpath <- 'output/3.clock/changsha_clock/elastic_net_scale/merge_bj_cs/'

samp_info_dat_bj <- read.xlsx(dat_path_list[['bj']], sheet=4) %>% 
  filter(keep=='y')
samp_info_dat_cs <- read.xlsx(dat_path_list[['cs']], sheet=2) %>% 
  filter(keep=='y')

group_annot <- samp_info_dat_bj %>% 
  select(group, short_name, name, class) %>% 
  filter(group %in% samp_info_dat_cs$group) %>% 
  filter(!(group %in% sport_list))

expdat_bj <- read.xlsx(exp_path_list[['bj']])
expdat_cs <- read.xlsx(exp_path_list[['cs']]) %>% 
  rename(patient_id=1, gender=6) %>% 
  mutate(gender=ifelse(gender=='男', 1, 2))


delta_age <- read.xlsx(age_path) %>% 
  rename(samp_id=1) %>% 
  select(samp_id, age_gaps) %>% 
  rename(patient_id=1)
annot_df <- read.xlsx(dat_path_list[['bj']], sheet='q_fea') %>% 
  fill(name, group)
# process -----------------------------------------------------------------

res_list <- list()
for(i in group_annot$group){
  
  bj_id_use <- samp_info_dat_bj %>% 
    filter(group %in% i) %>% 
    pull(id)
  
  cs_id_use <- samp_info_dat_cs %>% 
    filter(group %in% i) %>% 
    pull(id)
  
  # merge pdata
  pdata_bj <- expdat_bj %>% 
    select(patient_id, all_of(bj_id_use), age, gender) %>% 
    rename(feature=2) %>% 
    mutate(sour='bj')
  
  pdata_cs <- expdat_cs %>% 
    select(patient_id, all_of(cs_id_use), `s3_A4.年龄(岁)`, gender) %>% 
    rename(feature=2, age=3) %>% 
    mutate(sour='cs')
  
  bj_cs_pdata <- rbind(pdata_bj, pdata_cs) %>% 
    merge(., delta_age, by='patient_id')

  
  
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
      fit_res <- lm(age_gaps ~ feature + age + gender + sour, data = pdata)
      
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
      fit_res <- lm(age_gaps ~ feature + age + gender + sour, data = pdata)
      
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

write.xlsx(
  res_df,
  paste0(outpath, 'questionary_features_cor_with_delta_age_all.xlsx')
)


sig_group <- res_df %>% 
  filter(pval < 0.05) %>% 
  filter(feature != '(Intercept)') %>% 
  pull(group) %>% unique()

sig_res <- res_df %>% 
  filter(group %in% sig_group)

write.xlsx(
  sig_res,
  paste0(outpath, 'questionary_features_cor_with_delta_age.xlsx')
)




















