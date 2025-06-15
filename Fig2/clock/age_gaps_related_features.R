# -------------------------------------
# Date: Fri Apr 18 17:08:24 2025
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


dat_mtx <- read_excel(dat_path)


age_gaps <- read_excel(age_gaps_path) %>% 
  select(pt_id, age_gaps)

all_merge <- merge(dat_mtx, age_gaps, by='pt_id') %>% 
  select(-pt_id)


# spearman cor ------------------------------------------------------------


analyse_fea <- setdiff(colnames(all_merge), c('Gender', 'BMI', 'Age', 'age_gaps', 'sour'))

cor_res <- lapply(analyse_fea, function(x){
  sub_dat_clean <- na.omit(all_merge[, c(x, "Age", "BMI", "Gender", 'age_gaps')])
  tmp_cor <- pcor.test(sub_dat_clean[[x]], sub_dat_clean$age_gaps, sub_dat_clean[, c("BMI", "Gender","Age")], method = 'spearman') %>% 
    mutate(fea=x)
  return(tmp_cor)
}) %>% bind_rows() %>% 
  select(fea, everything())

sig_cor <- cor_res %>% 
  filter(`p.value`<0.05)

write_xlsx(
  sig_cor,
  paste0(outpath, 'age_gaps_related_fea_sig_p0.05.xlsx')
)

