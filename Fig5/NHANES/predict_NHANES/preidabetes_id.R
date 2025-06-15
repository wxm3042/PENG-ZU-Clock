# -------------------------------------
# Date: Wed May 14 15:56:04 2025
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

NHANES_dat_path <- 'V20250427/output/Fig5/NHANES/predict_NHANES/NHANES_data_clean.csv'


outpath <- 'V20250427/output/Fig5/NHANES/predict_NHANES/'

all_dat <- read.csv(NHANES_dat_path) %>% 
  rename(eid=1)


# process -----------------------------------------------------------------

pre_dia = all_dat %>% 
  filter(FBG<7) %>% 
  pull(eid)

write.table(
  pre_dia,
  file = paste0(outpath, 'preidabetes_id.txt'),
  sep = '\t',
  quote = F,
  row.names = F,
  col.names = 'id'
)


# process -----------------------------------------------------------------





