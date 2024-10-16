# -------------------------------------
# Date: Thu Sep 26 17:25:55 2024
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(readxl)
library(ggpubr)
library(writexl)
library(conflicted)
library(patchwork)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

ic_path_list <- list(
  cs='data/changsha/changsha_intrinsic_capacity.xlsx',
  bj='data/beijing/beijing_intrinsic_capacity.xlsx'
)
age_gaps_path <- 'output/3.clock/changsha_clock/elastic_net_scale/merge_bj_cs/age_gaps_with_type.xls'

age_cutoff <- 60

outpath <- 'output/0.figure/figure3/merge/'

age_gaps <- read.delim(
  file = age_gaps_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
)


# ic ----------------------------------------------------------------------

ic_res_list <- list()
for(g in names(ic_path_list)){
  if(g=='bj'){
    tmp_ic <- read_excel(ic_path_list[[g]]) %>% 
      rename(pt_id=1, ic=2)
  }else{
    tmp_ic <- read_excel(ic_path_list[[g]]) %>% 
      select(-1) %>% 
      rename(pt_id=1, ic=2)
  }
  ic_res_list[[g]] <- tmp_ic
}
ic_res_all <- bind_rows(ic_res_list)

# ggline ------------------------------------------------------------------

pdata_rel <- age_gaps %>% 
  merge(., ic_res_all, by='pt_id') %>% 
  mutate(type=ifelse(type=='Slowdown', 'Decelerated', type)) %>% 
  mutate(type=factor(type, c('Decelerated','Normal', 'Accelerated'))) %>% 
  mutate(grp=ifelse(age<age_cutoff, 'y', 'o')) %>% 
  group_by(grp) %>%
  mutate(s_mean = mean(ic[type == "Decelerated"])) %>%
  ungroup() %>%
  mutate(adjusted_IC = ic / s_mean) %>% 
  mutate(grp=factor(grp, c('y', 'o'))) %>% 
  arrange(grp, type)



plt1 <- ggbarplot(
  pdata_rel,
  x = "type",
  y = "adjusted_IC",
  add = "mean_se",
  color = "grp",
  palette = "jco",
  position = position_dodge(0.8)
) +
  stat_compare_means(aes(group = grp), label = "p.signif", label.y = 1.1) +
  labs(x='', y='Relative IC score') +
  theme_bw() +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black'),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(color = 'black'),
    legend.position = '',
    panel.grid = element_blank()
  )


# abs line ----------------------------------------------------------------

pdata <- age_gaps %>% 
  merge(., ic_res_all, by='pt_id') %>% 
  mutate(type=ifelse(type=='Slowdown', 'Decelerated', type)) %>% 
  mutate(type=factor(type, c('Decelerated','Normal', 'Accelerated'))) %>% 
  mutate(grp=ifelse(age<age_cutoff, 'y', 'o')) %>% 
  mutate(grp=factor(grp, c('y', 'o')))

plt2 <- ggline(
  pdata,
  'type',
  'ic',
  add = "mean_se",
  color = "grp",
  palette = "jco"
) +
  labs(x='', y='IC score') +
  theme_bw() +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(color = 'black'),
    panel.border = element_rect(color = 'black'),
    legend.position = 'top',
    panel.grid = element_blank()
  )



plt <- wrap_plots(list(plt2, plt1), ncol = 1) +
  plot_layout(heights = c(1, 2))


ggsave(
  filename = paste0(outpath, 'IC_score_age_60.pdf'),
  plt,
  width = 4,
  height = 5
)

