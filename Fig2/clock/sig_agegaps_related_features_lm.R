# -------------------------------------
# Date: Mon May  5 09:15:41 2025
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
library(ppcor)

library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("select", "dplyr")
# detach(unload='conflicted')

# input -------------------------------------------------------------------

dat_path <- 'V20250427/output/use_dat/merge_data.xlsx'
age_gaps_path <- 'V20250427/output/Fig2/clock/age_gaps.xlsx'

outpath <- 'V20250427/output/Fig2/clock/'


dat_mtx <- read_excel(dat_path) %>% 
  select(-q_id, -ID_numb)

age_gaps <- read_excel(age_gaps_path) %>% 
  select(pt_id, age_gaps)

# process -----------------------------------------------------------------

cov_df <- dat_mtx %>% 
  select(pt_id, Gender, Age, sour, BMI)

feature_list <- dat_mtx %>% 
  select(-pt_id, -Gender, -Age, -sour, -BMI) %>% 
  colnames(.)

scale_dat <- dat_mtx %>% 
  select(pt_id, all_of(feature_list)) %>% 
  column_to_rownames('pt_id') %>% 
  scale(.) %>% 
  data.frame(., check.names = F) %>% 
  rownames_to_column('pt_id')

all_merge <- merge(scale_dat, age_gaps, by='pt_id') %>% 
  merge(., cov_df, by='pt_id') %>% 
  select(-pt_id)

res_list <- list()
for(f in feature_list){
  
  sub_df <- all_merge %>% 
    select(Age, age_gaps, sour, Gender, BMI, all_of(f)) %>% 
    rename(fea=6) %>% 
    filter(!is.na(fea))
  
  if(nrow(sub_df)==0){
    next
  }
  
  # 定义线性模型
  linear_model <- lm(age_gaps ~ fea + Age + sour + Gender, data = sub_df)
  
  # 输出模型摘要
  model_summary <- summary(linear_model)
  
  coefficients <- coef(linear_model)[['fea']]
  if(!is.na(coefficients)){
    coefficients_p_values <- model_summary$coefficients[, "Pr(>|t|)"][['fea']]
    # 多重检验校正
    adjusted_p_values <- p.adjust(summary(linear_model)$coefficients[, "Pr(>|t|)"], method = "BH")[['fea']]
  }else{
    coefficients_p_values <- 1
    adjusted_p_values <- 1
  }
  
  res_list[[f]] <- data.frame(
    feature=f,
    coefficients=coefficients,
    pvalue=coefficients_p_values,
    padj=adjusted_p_values
  )
  
}

res_df <- bind_rows(res_list) %>% 
  rename(id=1) %>% 
  arrange(coefficients)


sig_res_df <- res_df %>% 
  # filter(padj<0.05) %>% 
  filter(pvalue<0.05) %>% 
  rename(id=1)
write_xlsx(
  sig_res_df,
  paste0(outpath, 'sig_agegaps_related_features_lm.xlsx')
)
