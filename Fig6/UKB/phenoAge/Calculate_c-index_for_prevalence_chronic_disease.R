# -------------------------------------
# Date: Wed Apr  9 15:22:40 2025
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
# 
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(readxl)
library(survcomp)
library(survival)
library(foreach)
# library(doParallel)
library(writexl)
library(parallel)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')

# function ----------------------------------------------------------------

C_index_Func <- function(use_dat, fea, covariate_use) {
  
  # 排除与 fea 重复的协变量（如 fea = "Age"，则排除 Age）
  covariates_final <- setdiff(covariate_use, fea)
  
  # 构建公式字符串
  covariate_formula <- paste(c("x", covariates_final), collapse = " + ")
  formula <- as.formula(paste("Surv(disease_time, disease_status) ~", covariate_formula))
  
  # 构建数据
  surv_dat <- use_dat %>% 
    select(disease_status, disease_time, all_of(fea), all_of(covariates_final)) %>%
    rename(x = all_of(fea))
  
  # 拟合 Cox 模型
  fit <- coxph(formula, data = surv_dat)
  sum.surv <- summary(fit)
  c_index <- sum.surv$concordance[[1]]
  
  # 返回结果
  c_index_df <- data.frame(
    concordance_indices = c_index,
    fea = fea
  )
  
  return(c_index_df)
}

# input -------------------------------------------------------------------

covariate_path = '..\\00database\\UKB\\output\\feature\\covariate_feature_data.csv'

fea_freq_path = 'V20250427\\output\\Fig5\\UKB\\predict_UKB\\elasticNet_coefficients.xlsx'
UKB_data_path = 'V20250427\\output\\Fig5\\UKB\\predict_UKB\\UKB_data_clean.csv'

phenoage_path = 'V20250427\\output\\Fig6\\UKB\\phenoAge\\age_gaps.xlsx'
# clinic_age_path = 'output\\3.clock\\changsha_clock\\elastic_net_scale\\UKB_clock_250121\\UKB\\age_gaps.xlsx'

clinic_age_path = 'V20250427\\output\\Fig5\\UKB\\predict_UKB\\age_gaps.xlsx'

disease_data_path = '..\\00database\\UKB\\output\\disease250417/disease_diagnosis_time250417.csv'
death_dat_path = '..\\00database\\UKB\\output\\disease250417/death_data_annot.csv'
show_class_path = 'V20250427\\data\\chronic_disease_id_long.xlsx'
low_Aascvd_path = 'output\\3.clock\\changsha_clock\\elastic_net_scale\\UKB_clock_250121\\UKB\\low_ascvd_dat.csv'

n_bootstrap <- 1

outpath <- 'V20250427/output/Fig6/UKB/phenoAge/'

covariate_dat <- read.csv(covariate_path) %>% 
  select(-Gender, -Age)
fea_freq <- read_xlsx(fea_freq_path) %>% pull(id)
UKB_data <- read.csv(UKB_data_path, check.names = F) %>% 
  select(eid, Age, Gender, all_of(fea_freq))

phenoage <- read_excel(phenoage_path) %>% 
  select(eid, age_gaps) %>% 
  rename(phenoAge=2)


clinic_age <- read_excel(clinic_age_path) %>% 
  select(pt_id, age_gaps) %>% 
  rename(eid=1, clinicAge=2)

disease_data <- read.csv(disease_data_path)
death_dat <- read.csv(death_dat_path) %>% select(eid, time)
show_class <- read_excel(show_class_path)


covariate_use <- c(
    'Age',
    'BMI',
    'Gender'
    
)
# covariate ---------------------------------------------------------------

covariate_dat$Income <- factor(covariate_dat$Income)
covariate_dat$Smoking_status <- factor(covariate_dat$Smoking_status)
covariate_dat$drinking_category <- factor(covariate_dat$drinking_category)
covariate_dat$Ethnicity <- factor(covariate_dat$Ethnicity)

# process -----------------------------------------------------------------

merge_dat <- Reduce(function(x,y) merge(x, y, by='eid'), list(UKB_data, phenoage, clinic_age)) %>% 
  select(-BMI) %>%
  merge(., covariate_dat, by='eid')


fea_list <- fea_freq %>% 
  c(., 'phenoAge', 'clinicAge', 'Age')


# subclass ----------------------------------------------------------------

sub_class_list <- unique(show_class$Subgroup)


sub_class_fea_res_list <- list()
for(sclas in sub_class_list){
  print(sclas)
  if(sclas=="Diabetes Mellitus"){
    sub_disease_id <- 'E11'
    
  }else{
    sub_disease_id <- show_class %>% 
      filter(Subgroup==sclas) %>% 
      pull(Code) %>% 
      unique()
    
  }
  
  rm_samp <- disease_data %>%
    filter((id_l2 %in% sub_disease_id) | (id_l3_type2 %in% sub_disease_id)) %>%
    filter(time<=0) %>%
    pull(eid) %>% unique()
  
  sub_prevalence <- disease_data %>% 
    filter((id_l2 %in% sub_disease_id) | (id_l3_type2 %in% sub_disease_id)) %>% 
    select(eid, time) %>%
    filter(time > 0) %>%
    rename(d_time = 2) %>%
    unique() %>%
    arrange(eid, d_time)
  sub_prevalence <- sub_prevalence[!duplicated(sub_prevalence$eid),]
  
  disease_merge <- merge(death_dat, sub_prevalence, by='eid', all.x = T) %>% 
    mutate(disease_status=ifelse(is.na(d_time), 0, 1)) %>% 
    mutate(disease_time=ifelse(disease_status==1, d_time, time)) %>% 
    filter(!(eid %in% rm_samp)) %>% 
    select(eid, disease_status, disease_time)
  
  # sex 1M 2F换为 1M 0F
  use_dat <- merge(merge_dat, disease_merge, by='eid') %>% 
    rename(sex=Gender) %>% 
    mutate(Gender=ifelse(sex==2, 0, sex))
  
  
  
  cl <- makeCluster(12)  # 使用所有可用核的一个少数
  clusterEvalQ(
    cl, 
    {
      library(survival)
      library(tidyverse)
    }
  )
  clusterExport(cl, list('use_dat', 'C_index_Func', 'covariate_use'))
  
  sub_class_fea_res_list[[sclas]] <- parLapply(cl, fea_list, function(fea) C_index_Func(use_dat, fea, covariate_use)) %>% 
    bind_rows() %>% 
    mutate(Class=sclas)
  # 关闭并行计算
  stopCluster(cl)
  
}

saveRDS(sub_class_fea_res_list, paste0(outpath, 'c-index_fea_res_list_level2_DPS_chronic_disease_class.rds'))



# prediabetes -------------------------------------------------------------


sub_disease_id <- 'E11' # 二型糖尿病

rm_samp <- disease_data %>% 
  filter(id_l2 %in% sub_disease_id) %>% 
  filter(time<=0) %>% 
  pull(eid) %>% unique()

sub_prevalence <- disease_data %>% 
  filter(id_l2 %in% sub_disease_id) %>% 
  select(eid, time) %>% 
  filter(time>0) %>%
  arrange(eid, time) %>% 
  rename(d_time=2)
sub_prevalence <- sub_prevalence[!duplicated(sub_prevalence$eid),]

disease_merge <- merge(death_dat, sub_prevalence, by='eid', all.x = T) %>% 
  mutate(disease_status=ifelse(is.na(d_time), 0, 1)) %>% 
  mutate(disease_time=ifelse(disease_status==1, d_time, time)) %>% 
  filter(!(eid %in% rm_samp)) %>% 
  select(eid, disease_status, disease_time)

use_dat <- merge(merge_dat, disease_merge, by='eid') %>%
  filter(FBG<7)




cl <- makeCluster(12)  # 使用所有可用核的一个少数
clusterEvalQ(
  cl, 
  {
    library(survival)
    library(tidyverse)
  }
)
clusterExport(cl, list('use_dat', 'C_index_Func', 'covariate_use'))

c_res <- parLapply(cl, fea_list, function(fea) C_index_Func(use_dat, fea, covariate_use)) %>% 
  bind_rows() %>% 
  arrange(desc(concordance_indices))
# 关闭并行计算
stopCluster(cl)

head(c_res)

write.table(
  c_res,
  file = paste0(outpath, 'prediabetes_c-index.txt'),
  sep = '\t',
  quote = F,
  row.names = F
)



# ASCVD -------------------------------------------------------------------

low_Aascvd_dat <- read.csv(low_Aascvd_path)

sub_disease_id = c(
  'I20',
  'I21',
  'I22',
  'I25',
  'I63',
  'I65',
  'I66',
  'I70',
  'I73'
)

rm_samp <- disease_data %>% 
  filter(id_l2 %in% sub_disease_id) %>% 
  filter(time<=0) %>% 
  pull(eid) %>% unique()

sub_prevalence <- disease_data %>% 
  filter(id_l2 %in% sub_disease_id) %>% 
  select(eid, time) %>% 
  filter(time>0) %>%
  arrange(eid, time) %>% 
  rename(d_time=2)
sub_prevalence <- sub_prevalence[!duplicated(sub_prevalence$eid),]


disease_merge <- merge(death_dat, sub_prevalence, by='eid', all.x = T) %>% 
  mutate(disease_status=ifelse(is.na(d_time), 0, 1)) %>% 
  mutate(disease_time=ifelse(disease_status==1, d_time, time)) %>% 
  filter(!(eid %in% rm_samp)) %>% 
  select(eid, disease_status, disease_time)

# sex 1M 2F换为 1M 0F
use_dat <- merge(merge_dat, disease_merge, by='eid') %>% 
  rename(sex=Gender) %>% 
  mutate(Gender=ifelse(sex==2, 0, sex))


cl <- makeCluster(12)  # 使用所有可用核的一个少数
clusterEvalQ(
  cl, 
  {
    library(survival)
    library(tidyverse)
  }
)
clusterExport(cl, list('use_dat', 'C_index_Func', 'covariate_use'))

c_res <- parLapply(cl, fea_list, function(fea) C_index_Func(use_dat, fea, covariate_use)) %>% 
  bind_rows()
# 关闭并行计算
stopCluster(cl)


write.table(
  c_res,
  file = paste0(outpath, 'low_ASCVD_c-index.txt'),
  sep = '\t',
  quote = F,
  row.names = F
)


