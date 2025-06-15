# -------------------------------------
# Date: Sun Apr 20 22:45:42 2025
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
library(ggpubr)
library(patchwork)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------


ic_path_list <- list(
  cs='V20250427/data/changsha/changsha_intrinsic_capacity.xlsx',
  bj='V20250427/data/beijing/beijing_intrinsic_capacity.xlsx'
)
age_gaps_path <- 'V20250427/output/Fig3/age_gaps_with_disease.xls'

age_cutoff <- 60

outpath <- 'V20250427/output/Fig3/'

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
  # filter(type!='Normal') %>% 
  mutate(type=ifelse(type=='Slowdown', 'Decelerated', type)) %>% 
  mutate(type=factor(type, c('Decelerated','Normal', 'Accelerated'))) %>% 
  mutate(grp=ifelse(Age<age_cutoff, 'y', 'o')) %>% 
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
  fill = "grp",
  palette = c('#3773B6', '#B8281D'),
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
  # filter(type!='Normal') %>% 
  mutate(type=ifelse(type=='Slowdown', 'Decelerated', type)) %>% 
  mutate(type=factor(type, c('Decelerated','Normal', 'Accelerated'))) %>% 
  mutate(grp=ifelse(Age<age_cutoff, 'y', 'o')) %>% 
  mutate(grp=factor(grp, c('y', 'o')))

plt2 <- ggline(
  pdata,
  'type',
  'ic',
  add = "mean_se",
  color = "grp",
  palette = c('#3773B6', '#B8281D'),
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
  filename = paste0(outpath, '3F_IC_score_age_60.pdf'),
  plt,
  width = 4,
  height = 5
)


