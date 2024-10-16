# -------------------------------------
# Date: Mon Sep 30 14:34:37 2024
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

dat_path <- 'output/3.clock/changsha_clock/elastic_net_scale/test_beijing/slowdown_vs_accelerate/common_id.xlsx'
protein_dat_path <- '../20231001_urine_proteome_metabolome/output/proteome/1.QC/all_sample/normalized_by_median_filtered.xlsx'
annot_path <- '../00database/UniProt/Release_2023_04/HUMAN_9606_id_annot.rds'

outpath <- 'output/0.figure/figure5/beijing_data_res/'

dat <- read_excel(dat_path) %>% 
  mutate(type=gsub('Slowdown','Decelerated',type))

protein_dat <- read_excel(protein_dat_path) %>% 
  column_to_rownames('id') %>% 
  t() %>% scale() %>% 
  data.frame(., check.names = F) %>% 
  rownames_to_column('samp_id') 

annotdf <- readRDS(annot_path)

# process -----------------------------------------------------------------


merge_dat <- merge(dat, protein_dat, by='samp_id') %>% 
  mutate(type=factor(type, c('Decelerated','Accelerated')))

res_list <- list()
for(f in colnames(protein_dat[,-1])){
  sub_pdata <- merge_dat %>% 
    select(samp_id, type, age, gender, all_of(f)) %>% 
    rename(fea=5)
  
  fit <-  lm(fea ~ type + age + gender, sub_pdata)
  res <- summary(fit)$coefficients %>%
    data.frame(., check.rows = F) %>%
    rownames_to_column('name') %>% 
    select(1,2, 5) %>%
    rename(beta = 2, pval = 3) %>%
    filter(name == 'typeAccelerated') %>% 
    mutate(id=f) %>% 
    select(id, everything(), -name)
  res_list[[f]] <- res
  
}

all_res <- bind_rows(res_list) %>% 
  mutate(id=gsub(';.*$','',id)) %>% 
  merge(., annotdf, by='id') %>% 
  select(id, gene, everything())

write_xlsx(all_res, paste0(outpath, 'all_deg_pt.xlsx'))

sig_res <- all_res %>% 
  filter(pval<0.05)

write_xlsx(sig_res, paste0(outpath, 'sig_deg_p0.05_pt.xlsx'))


