# -------------------------------------
# Date: Thu May 15 09:52:46 2025
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
library(ggtext)
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
  formula <- as.formula(paste("Surv(time, status) ~", covariate_formula))
  
  # 构建数据
  surv_dat <- use_dat %>% 
    select(status, time, all_of(fea), all_of(covariates_final)) %>%
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

# covariate_path = '../00database/UKB/output/feature/covariate_feature_data.csv'

fea_freq_path = 'V20250427/output/Fig5/NHANES/predict_NHANES/elasticNet_coefficients.xlsx'
UKB_data_path = 'V20250427/output/Fig5/NHANES/predict_NHANES/NHANES_data_clean.csv'
phenoage_path = 'V20250427/output/Fig5/NHANES/predict_NHANES/c-index/age_gaps_DNAm.xlsx'
clinic_age_path = 'V20250427/output/Fig5/NHANES/predict_NHANES/age_gaps.xlsx'
disease_id_path <- '../00database/NHANES/All/6_mortality/death_id_name.xlsx'

death_dat_path = '../00database/NHANES/All/6_mortality/mortality.xlsx'

n_bootstrap <- 1

outpath <- 'V20250427/output/Fig5/NHANES/predict_NHANES/c-index/'

disease_id <- read_excel(disease_id_path) %>% 
  filter(show=='y') %>% 
  select(-show)

fea_freq <- read_xlsx(fea_freq_path) %>% pull(id)
UKB_data <- read.csv(UKB_data_path, check.names = F) %>% 
  rename(eid=seqn) %>% 
  # rename(ALB='Alb', `Cys c`='Cystatin c', `Grip strength`='grip_strength') %>%
  select(eid, Age, all_of(fea_freq))

DNAm_age <- read_excel(phenoage_path) %>% 
  rename(eid=1)


clinic_age <- read_excel(clinic_age_path) %>% 
  # rename(pt_id=1) %>%
  select(pt_id, age_gaps) %>% 
  rename(eid=1, clinicAge=2)

death_dat <- read_excel(death_dat_path)

covariate_use <- c(
  'Age',
  'BMI',
  'Gender'
  
)

# process -----------------------------------------------------------------

DNAm_age_list <- colnames(DNAm_age)[-c(1:2)]


sub_DNAm_age <- DNAm_age %>% 
  select(-Age)

merge_dat <- Reduce(function(x,y) merge(x, y, by='eid'), list(UKB_data, sub_DNAm_age, clinic_age))

fea_list <- colnames(merge_dat)[-1]


# subclass ----------------------------------------------------------------

sub_class_list <- disease_id$id

sub_class_fea_res_list <- list()
for(sclas in sub_class_list){
  print(sclas)
  
  death_time_dat <- death_dat %>% 
    filter(status==0 | death_causes==sclas) %>% 
    select(seqn, status, time) %>% 
    rename(eid=1)

  # sex 1M 2F换为 1M 0F
  use_dat <- merge(merge_dat, death_time_dat, by='eid')
  
  
  
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
    mutate(Class=sclas) %>% 
    arrange(desc(concordance_indices))
  # 关闭并行计算
  stopCluster(cl)
  
  # view(sub_class_fea_res_list[[sclas]])
}

saveRDS(sub_class_fea_res_list, paste0(outpath, 'c-index_fea_res_chronic_disease_class.rds'))


# plot --------------------------------------------------------------------

pdata <- bind_rows(sub_class_fea_res_list) %>% 
  merge(., disease_id, by.x='Class', by.y='id') %>% 
  select(-Class) %>% 
  rename(Class=name) %>% 
  arrange(Class, desc(concordance_indices)) %>% 
  group_by(Class) %>% 
  mutate(sort=row_number()) %>% 
  mutate(col=ifelse(fea %in% fea_freq, 'black', 'blue')) %>% 
  mutate(col=ifelse(fea=='clinicAge', 'red', col))


class_list <- unique(pdata$Class)

pdf(paste0(outpath, 'c-index.pdf'))
for(c in class_list){
  
  sub_pdat <- pdata %>% 
    filter(Class==c) %>% 
    mutate(fea=factor(fea, rev(fea)))
  
  plt <- ggplot(sub_pdat, aes(fea, concordance_indices)) +
    geom_point()  +
    labs(x='', y='C-index', title=c) +
    theme_bw() +
    theme(
      legend.key.size = unit(.1, 'inches'),
      text = element_text(size = 12, color = 'black'),
      axis.text.x = element_text(size = 12, color = 'black', angle = 90, hjust = 1, vjust = .5),
      axis.text.y = element_text(size = 12, color = 'black'),
      axis.ticks = element_line(color = 'black'),
      panel.border = element_rect(color = 'black'),
      legend.position = 'top',
      panel.grid = element_blank()
    )
  plot(plt)
}
dev.off()


# other -------------------------------------------------------------------


