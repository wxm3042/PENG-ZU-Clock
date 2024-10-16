# -------------------------------------
# Date: Mon Sep 30 17:31:37 2024
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
library(Hmisc)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
source('../00script/RFunction/EnrichPlotFunc.R')

# detach(unload='conflicted')
# input -------------------------------------------------------------------

enrich_res_path <- 'output/0.figure/figure5/beijing_data_res/metascape/metascape_result.xlsx'
deg_res_path <- 'output/0.figure/figure5/beijing_data_res/sig_deg_p0.05_pt.xlsx'

outpath <- 'output/0.figure/figure5/beijing_data_res/'

enrich_res <- read_excel(enrich_res_path, sheet=2) %>% 
  filter(grepl('Summary$', GroupID)) %>% 
  slice_head(n=10) %>% 
  mutate(Description=gsub('HALLMARK ','',Description)) %>% 
  mutate(Description=capitalize(tolower(Description)))
deg_res <- read_excel(deg_res_path)

# barplot -----------------------------------------------------------------

bar_dat <- enrich_res %>% 
  select(Description, LogP, Symbols) %>% 
  mutate(LogP=abs(LogP)) %>% 
  rename(pathway=1, logp=2, hit=3)

hit_new <- lapply(bar_dat$hit, function(x){
  
  tmp_list <- strsplit(x, split = ',') %>% 
    unlist(.)
  
  new_list <- deg_res %>% 
    filter(gene %in% tmp_list) %>% 
    arrange(pval) %>% 
    slice_head(n=10) %>% 
    pull(gene) %>% 
    paste0(., collapse = ',')
  
  return(new_list)
}) %>% unlist()

bar_dat$hit <- hit_new

new_dat_list <- list()
for(i in 1:10){
  tmp_df <- data.frame(
    pathway=bar_dat[[i,'hit']],
    logp=0,
    hit=''
  )
  new_dat_list[[i]] <- rbind(bar_dat[i,],tmp_df)
}

new_dat <- bind_rows(new_dat_list) %>% 
  mutate(pathway=factor(pathway, rev(pathway)))


plt <- ggplot(new_dat, aes(pathway, logp, fill=logp)) +
  geom_bar(stat = 'identity') +
  scale_fill_gradient(low='#ced5ec', high = '#4c6fbb') +
  # geom_text(aes(label = pathway), hjust = 1, vjust = 0.5) +  # 在柱子上添加文字
  coord_flip() +
  labs(x='', y='-Log10(P value)') +
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

ggsave(
  filename = paste0(outpath, 'pt_enrich_bar.pdf'),
  plt,
  width = 14,
  height = 6
)




