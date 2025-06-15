# -------------------------------------
# Date: Sun Apr 20 16:40:24 2025
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

pt_disease_path <- 'V20250427/data/bj_cs_disease_mtx.xlsx'
rm_path <- 'V20250427/data/remove_disease_list.xlsx'

outpath <- 'V20250427/output/Fig3/'

rm_disease <- read_excel(rm_path) %>% pull(1)

pt_disease <- read_excel(pt_disease_path) %>% 
  mutate(id=gsub('patient', 'pt', id))

rm_disease <- c(rm_disease, 'Gastrointestinal.diseases', 'Cervical.and.lumbar.diseases')
# process -----------------------------------------------------------------


pt_disease_filter <- pt_disease %>% 
  select(-all_of(rm_disease)) %>% 
  filter(`Heart.disease`<=1)


write_xlsx(
  pt_disease_filter,
  paste0(outpath, 'disease_dat.xlsx')
)

