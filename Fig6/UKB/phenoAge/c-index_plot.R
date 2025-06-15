# -------------------------------------
# Date: Wed Apr  9 16:05:07 2025
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
library(patchwork)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

dat_path_list <- list(
  level1 = 'V20250427/output/Fig6/UKB/phenoAge/c-index_fea_res_list_level1_DPS_chronic_disease_class.rds',
  levle2 ='V20250427/output/Fig6/UKB/phenoAge/c-index_fea_res_list_level2_DPS_chronic_disease_class.rds'
)
single_dat_path <- c(
  prediabetes = 'V20250427/output/Fig6/UKB/phenoAge/prediabetes_c-index.txt',
  low_ascvd='V20250427/output/Fig6/UKB/phenoAge/low_ASCVD_c-index.txt'
)

covariate_path = '..\\00database\\UKB\\output\\feature\\covariate_feature_data.csv'

outpath <- 'V20250427/output/Fig6/UKB/phenoAge/'

covariate_dat_colname <- read.csv(covariate_path) %>% 
  select(-Gender, -Age) %>% 
  select(-eid) %>% 
  colnames(.)

# process -----------------------------------------------------------------

# for(g in names(dat_path_list)){
  g='levle2'
  fea_res_list <- readRDS(dat_path_list[[g]])
  
  
  pdata <- bind_rows(fea_res_list) %>% 
    arrange(Class, desc(concordance_indices)) %>% 
    group_by(Class) %>% 
    mutate(sort=row_number()) %>% 
    filter(sort<16)
  
  clinic_sort <- pdata %>% 
    filter(fea == 'clinicAge') %>% 
    select(Class, sort) %>% 
    rename(sort_c=2)
  
  pheno_sort <- pdata %>% 
    filter(fea == 'phenoAge') %>% 
    select(Class, sort) %>% 
    rename(sort_p=2)
  
  top_fea <-merge(clinic_sort, pheno_sort, by='Class') %>% 
    mutate(change=sort_c-sort_p)
  
  
  class_list <- unique(pdata$Class)
  
  plt_list <- list()
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
    plt_list[[c]] <- plt
  }
  
  plt_all <- wrap_plots(plt_list, ncol=4)
  
  ggsave(
    filename = paste0(outpath, 'c-index_plot_', g, '.pdf'),
    plt_all,
    width = 15,
    height = (ceiling(length(plt_list)/4)) * 4
  )
  
# }


# single dat --------------------------------------------------------------

for(g in names(single_dat_path)){
  tmp_dat <- read.delim(
    file = single_dat_path[[g]],
    sep = '\t',
    quote = '',
    header = T,
    check.names = F
  ) %>%
    filter(!(fea %in% covariate_dat_colname)) %>% 
    arrange(concordance_indices) %>%
    mutate(fea=factor(fea, fea))

  plt <- ggplot(tmp_dat, aes(fea, concordance_indices)) +
    geom_point() +
    labs(x='', y='C-index', title=g) +
    theme(
      legend.key.size = unit(.1, 'inches'),
      text = element_text(size = 12, color = 'black'),
      axis.text.x = element_text(size = 12, color = 'black', angle = 90, hjust = 1, vjust = .5),
      axis.text.y = element_text(size = 12, color = 'black'),
      axis.ticks = element_line(color = 'black'),
      panel.border = element_rect(colour = 'black', fill=NA),
      legend.position = 'top',
      panel.grid = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank()
    )
  ggsave(
    filename = paste0(outpath, g, '_c-index_plot.pdf'),
    plt,
    width = 5,
    height = 4
  )
}

