# -------------------------------------
# Date: Tue Jul 16 13:51:34 2024
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

annot_path <- 'data/beijing/副本北京医院2596例final原表_full-矫正.xlsx'
col_path <- 'output/0.figure/figure4/class_col.rds'
dat_path_list <- list(
  Y='output/3.clock/changsha_clock/elastic_net_scale/merge_bj_cs/young-vs-old_delta_age_related_features/Y_questionary_features_sig_cor_annot.xlsx',
  O='output/3.clock/changsha_clock/elastic_net_scale/merge_bj_cs/young-vs-old_delta_age_related_features/O_questionary_features_sig_cor_annot.xlsx'
)

outpath <- 'output/0.figure/figure4/merge/'

col_list <- readRDS(col_path)

# process -----------------------------------------------------------------

for(g in names(dat_path_list)){
  dat_df <- read_excel(dat_path_list[[g]])
  
  pdata <- dat_df %>% 
    filter(!(group %in% rm_grp[[g]])) %>%
    filter(!(choose_id %in% c('Not applicable', 'Not clear'))) %>% 
    # merge(., annot_df, by='group') %>% 
    mutate(id=ifelse(is.na(choose_id), short_name, paste0(short_name, '_', choose_id))) %>% 
    select(id, class, group, Estimate)
  
  grp_ord <- pdata %>% 
    select(group, Estimate) %>% 
    mutate(abs_Estimate=abs(Estimate)) %>% 
    group_by(group) %>%
    slice_max(n=1, order_by=abs_Estimate) %>% 
    arrange(Estimate)
  
  
  class_ord <- pdata %>% 
    pull(class) %>% 
    table() %>% 
    data.frame() %>% 
    rename(class=1) %>% 
    arrange(Freq)

  
  pdata_ord <- pdata  %>% 
    mutate(class=factor(class, rev(names(col_list)))) %>% 
    mutate(group=factor(group, grp_ord$group)) %>% 
    arrange(class, group, Estimate) %>% 
    mutate(id=factor(id, id))
  
  write_xlsx(
    pdata_ord,
    paste0(outpath, g, '_sig_questionary_features_barplot.xlsx')
  )
  
  
  plt <- ggplot(pdata_ord, aes(id, Estimate, fill=class)) +
    geom_bar(stat='identity') +
    scale_fill_manual(values = col_list) +
    labs(x='', y='') +
    coord_flip() +
    labs(x='', y='') +
    theme_bw() +
    theme(
      legend.key.size = unit(.1, 'inches'),
      text = element_text(size = 12, color = 'black'),
      axis.text.x = element_text(size = 12, color = 'black'),
      axis.text.y = element_text(size = 12, color = 'black'),
      axis.ticks = element_line(color = 'black'),
      panel.border = element_rect(color = 'black'),
      legend.position = 'top',
      panel.grid = element_blank()
    )
  if(g == 'O'){
    ggsave(
      filename = paste0(outpath, g, '_sig_uestionary_features_barplot.pdf'),
      plt,
      width = 7.2,
      height = 6
    )
  }else{
    ggsave(
      filename = paste0(outpath, g, '_sig_uestionary_features_barplot.pdf'),
      plt,
      width = 8,
      height = 10
    )
  }
}

