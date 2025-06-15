# -------------------------------------
# Date: Tue May 27 00:34:57 2025
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

dat_path_list <- list(
  ElasticNet = 'V20250427/output/Fig2/clock/algorithm_select/bj_test/ElasticNet_predict_age_in_test_data.xlsx',
  LASSO = 'V20250427/output/Fig2/clock/algorithm_select/bj_test/LASSO_predict_age_in_test_data.xlsx',
  LightGBM = 'V20250427/output/Fig2/clock/algorithm_select/bj_test/LightGBM_predict_age_in_test_data.xlsx',
  LinearRegression = 'V20250427/output/Fig2/clock/algorithm_select/bj_test/LinearRegression_predict_age_in_test_data.xlsx',
  RandomForest = 'V20250427/output/Fig2/clock/algorithm_select/bj_test/RandomForest_predict_age_in_test_data.xlsx',
  SVR = 'V20250427/output/Fig2/clock/algorithm_select/bj_test/SVR_predict_age_in_test_data.xlsx'
)

outpath <- 'V20250427/output/Fig2/clock/algorithm_select/bj_test/'


# process -----------------------------------------------------------------

res_list <- list()

for(g in names(dat_path_list)){
  
  dat <- read_excel(dat_path_list[[g]])
  cor_res <- cor(dat$predict_age, dat$age) %>% 
    round(., 2)
  
  mae <- dat %>% 
    mutate(aa=abs(age-predict_age)) %>% 
    pull(aa) %>% mean() %>% 
    round(., 2)
  res_list[[g]] <- data.frame(
    algorithm=g,
    cor=cor_res,
    mae=mae
  )
}
res_df <- bind_rows(res_list) %>% 
  arrange(desc(cor))
write_xlsx(
  res_df,
  paste0(outpath, 'res.xlsx')
)


