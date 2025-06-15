# -------------------------------------
# Date: Mon Apr 21 17:00:09 2025
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
  cs_n='V20250427/output/Fig2/clock/cs_normal_test/predict_age_in_test_data.xlsx',
  cs_d='V20250427/output/Fig2/clock/cs_disease_test/predict_age_in_test_data.xlsx'
)

outpath <- 'V20250427/output/Fig2/clock/cs_merge/'


# process -----------------------------------------------------------------
res_list <- list()
for(g in names(dat_path_list)){
  res_list[[g]] <- read_excel(dat_path_list[[g]])
}

all_cs <- bind_rows(res_list)

write_xlsx(
  all_cs,
  paste0(outpath, 'predict_age.xlsx')
)


