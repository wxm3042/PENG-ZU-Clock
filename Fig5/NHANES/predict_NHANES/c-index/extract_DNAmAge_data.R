# -------------------------------------
# Date: Thu May 15 09:19:22 2025
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
conflict_prefer("select", "dplyr")

# detach(unload='conflicted')
# input -------------------------------------------------------------------

dnam_path <- '../00database/NHANES/All/4_Laboratory_Data/DNAm.rds'

outpath <- 'V20250427/output/Fig5/NHANES/predict_NHANES/c-index/'

dnam <- readRDS(dnam_path)
# process -----------------------------------------------------------------

dnm_age <- dnam %>% 
  select(SEQN, ends_with('Age'))
dnm_age_clean <- na.omit(dnm_age)
write_xlsx(
  dnm_age_clean,
  paste0(outpath,'DNAm_age.xlsx')
)




