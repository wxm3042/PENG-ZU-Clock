# -------------------------------------
# Date: Wed Apr  9 13:46:32 2025
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

pred_path <- list(
  bj='V20250427/output/Fig2/clock/bj_test/predict_age_in_test_data.xlsx',
  cs1='V20250427/output/Fig2/clock/cs_normal_test/predict_age_in_test_data.xlsx',
  cs2='V20250427/output/Fig2/clock/cs_disease_test/predict_age_in_test_data.xlsx'
)

outpath <- 'V20250427/output/Fig2/clock/'


# process -----------------------------------------------------------------

res_list <- list()
for(g in names(pred_path)){
  
  dat <- read_excel(pred_path[[g]])
  
  
  res_list[[g]] <- dat
}

merge_res <- bind_rows(res_list)
write_xlsx(
  merge_res,
  paste0(outpath, 'predict_age_in_test_data.xlsx')
)


