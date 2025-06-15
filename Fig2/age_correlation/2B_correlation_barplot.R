# -------------------------------------
# Date: Thu Apr 17 18:17:38 2025
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
library(ggVennDiagram)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("select", "dplyr")

# detach(unload='conflicted')
# input -------------------------------------------------------------------

dat_path_list <- list(
  BJ='V20250427/output/use_dat/bj/correlation_dat_without_ic_score.xlsx',
  CS='V20250427/output/use_dat/cs/correlation_dat_without_ic_score.xlsx'
)

pt_id_path_list <- list(
  BJ='V20250427/output/use_dat/bj/clock_data.xlsx',
  CS='V20250427/output/use_dat/cs/clock_data.xlsx'
)

common_fea_path <- 'V20250427/output/Fig2/age_correlation/common_fea_for_correlation_pie.xlsx'

outpath <- 'V20250427/output/Fig2/age_correlation/'

common_fea <- read_excel(common_fea_path) %>% 
  rename(fea=1)

# process -----------------------------------------------------------------

fea_list <- list()
for(g in names(dat_path_list)){
  
  sub_dat <- read_excel(dat_path_list[[g]])
  fea_list[[g]] <- colnames(sub_dat)
}

common_fea <- intersect(fea_list[[1]], fea_list[[2]]) %>% 
  setdiff(., 'pt_id')

analyse_fea <- setdiff(common_fea, c('Gender', 'q_id', 'Age', 'ID_numb'))

res_list <- list()
sig_list <- list()
type_list <- list()
for(g in names(dat_path_list)){
  
  sub_pt_id <- read_excel(pt_id_path_list[[g]]) %>% 
    pull(pt_id)
  
  sub_dat <- read_excel(dat_path_list[[g]]) %>% 
    filter(pt_id %in% sub_pt_id) %>% 
    select(all_of(common_fea))
  
  cor_res <- lapply(analyse_fea, function(x){
    sub_dat_clean <- na.omit(sub_dat[, c(x, "Age", "Gender")])
    tmp_cor <- pcor.test(sub_dat_clean[[x]], sub_dat_clean$Age, sub_dat_clean[, c("Gender")], method = 'spearman') %>% 
      mutate(fea=x)
    return(tmp_cor)
  }) %>% bind_rows() %>% 
    select(fea, everything()) %>% 
    mutate(grp=g)
  res_list[[g]] <- cor_res
  sig_list[[g]] <- cor_res %>% 
    filter(`p.value`<0.05) %>% 
    select(fea, estimate)
  colnames(sig_list[[g]])[2] <- g
  
  
  #plot
  type_list[[g]] <- sig_list[[g]] %>% 
    rename(corr=2) %>% 
    mutate(type=ifelse(corr>0, 'Pos', 'Neg')) %>% 
    # mutate(grp=g) %>% 
    select(-corr) %>% 
    mutate(grp=paste0(type, '(', g, ')'))
  
  tmp_p <- sig_list[[g]] %>% 
    rename(corr=2) %>% 
    mutate(type=ifelse(corr>0, 'Pos', 'Neg')) %>% 
    pull(type) %>% 
    table(.) %>% 
    data.frame() %>% 
    rename(x=1, y=2)
  
  plt <- ggplot(tmp_p, aes(x, y, fill=x)) +
    geom_bar(stat='identity', color='black', width=.7) +
    scale_fill_manual(values=c(Neg='#3773B6',Pos = '#B8281D')) +
    geom_text(
      aes(label = y), 
      vjust = 0,            # 垂直位置调整
      hjust=0.5,
      color = "black",      # 文字颜色
      # size = 5,                 # 文字大小
      # fontface = "bold"         # 字体加粗
    ) +
    labs(x='', y='Number of features') +
    theme_bw() +
    theme(
      legend.key.size = unit(.1, 'inches'),
      text = element_text(size = 12, color = 'black'),
      axis.text.x = element_text(size = 12, color = 'black'),
      # axis.text.x = element_text(size = 12, color = 'black', angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 12, color = 'black'),
      axis.ticks = element_line(color = 'black'),
      panel.border = element_rect(color = 'black'),
      legend.position = 'nonw',
      panel.grid = element_blank()
    )
  ggsave(
    filename = paste0(outpath, g, '_2B_S_barplot.pdf'),
    plt,
    width = 3,
    height = 3
  )
  
  
}

# venn
ven_dat_raw <- type_list %>% 
  bind_rows()
ven_dat_list <- list()
for(tg in unique(ven_dat_raw$grp)){
  ven_dat_list[[tg]] <- ven_dat_raw %>% 
    filter(grp==tg) %>% 
    pull(fea)
}

plt <- ggVennDiagram(ven_dat_list, label = c('count')) +
  labs(x='', y='') +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_blank(),
    legend.position = 'none',
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
    )
ggsave(
  filename = paste0(outpath, 'S2C_venn.pdf'),
  plt,
  width = 3,
  height = 3
)

res_df <- bind_rows(res_list)

sig_fea <- merge(sig_list[[1]], sig_list[[2]], by='fea') %>% 
  mutate(check=BJ * CS) %>% 
  filter(check>0) %>% 
  mutate(ord=rowMeans(select(., BJ, CS))) %>% 
  arrange(ord)
write_xlsx(
  sig_fea,
  paste0(outpath, 'bj_cs_common_cor_feature.xlsx')
)


pdata <- res_df %>% 
  filter(fea %in% sig_fea$fea) %>% 
  mutate(fea=factor(fea, sig_fea$fea)) %>% 
  mutate(grp=gsub('BJ','BJ sub-cohort',grp)) %>% 
  mutate(grp=gsub('CS','CS sub-cohort',grp)) %>% 
  mutate(type=ifelse(estimate>0, 'Positive', 'Negative'))

plt <- ggplot(pdata, aes(fea, estimate, fill=type)) +
  geom_bar(stat='identity', color='black') +
  scale_fill_manual(values = c(Positive='#B8281D', Negative='#3773B6')) +
  labs(x='', y='Correlation coefficient') +
  facet_grid(rows = vars(grp)) +
  # coord_flip() +
  theme_bw() +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black', angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(colour = 'black', fill=NA),
    legend.position = 'top',
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
  )
ggsave(
  filename = paste0(outpath, '2B_cor_barplot.pdf'),
  plt,
  width = 10,
  height = 6
)


