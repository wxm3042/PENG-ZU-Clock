# -------------------------------------
# Date: Wed May 14 11:28:00 2025
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
library(survival)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------
death_dat_path <- '../00database/NHANES/All/6_mortality/mortality.xlsx'
disease_id_path <- '../00database/NHANES/All/6_mortality/death_id_name.xlsx'
group_path <- 'V20250427/output/Fig5/NHANES/predict_NHANES/age_gaps_with_disease.xls'
NHANES_dat_path <- 'V20250427/output/Fig5/NHANES/predict_NHANES/NHANES_data_clean.csv'

outpath <- 'V20250427/output/Fig5/NHANES/predict_NHANES/'

NHANES_dat <- read.csv(NHANES_dat_path) %>% 
  # rename(seqn=1, sex=2) %>% 
  select(seqn, Gender, BMI)
death_dat <- read_excel(death_dat_path) %>% 
  select(seqn, status, death_causes, time)
disease_id <- read_excel(disease_id_path) %>% 
  filter(show=='y')
group_df <- read.delim(
  file = group_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
) %>% select(pt_id, Age, type) %>% 
  rename(seqn=1)
# process -----------------------------------------------------------------


use_dat <- merge(group_df, death_dat, by='seqn') %>% 
  merge(., NHANES_dat, by='seqn') %>% 
  mutate(type=ifelse(type=='Decelerated', 'Normal', type))

res_list <- list()
for(d in disease_id$id){
  disease_name <- disease_id %>% 
    filter(id == d) %>% 
    pull(name)
  print(disease_name)
  
  sub_dat <- use_dat %>% 
    mutate(disease_status=ifelse(death_causes==d, 1, 0)) %>% 
    mutate(disease_status=ifelse(is.na(disease_status), 0, disease_status)) %>% 
    mutate(type=factor(type, c('Normal', 'Accelerated')))
  
  
  cox_model <- coxph(Surv(time, disease_status) ~ type+ Age + Gender + BMI, data = sub_dat)
  
  # 输出模型结果
  summary(cox_model)
  
  hr <- exp(coef(cox_model)) %>% 
    data.frame(HR=.) %>% 
    rownames_to_column('grp')
  
  ci <- exp(confint(cox_model)) %>% 
    data.frame() %>% 
    rename(low_ci=1, high_ci=2) %>% 
    rownames_to_column('grp')
  
  p_value <- coef(summary(cox_model))[, "Pr(>|z|)"] %>% 
    data.frame(pvalue=.) %>% 
    rownames_to_column('grp')
  
  res <- Reduce(function(x, y) merge(x, y, by='grp'), list(hr, ci, p_value))
  res_list[[d]] <- res %>% 
    mutate(disease=disease_name)
}
res_df <- bind_rows(res_list) %>% 
  mutate(grp=gsub('type', '', grp))

write.table(
  res_df,
  file = paste0(outpath, 'all_disease_survival_risk_data.xls'),
  sep = '\t',
  quote = F,
  row.names = F
)


