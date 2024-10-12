# -------------------------------------
# Date: Fri Jul  5 01:04:54 2024
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
library(ggrepel)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

dat_path_list <- list(
  bj='output/2.deg/beijing/all_correlation_res.xlsx',
  cs='output/2.deg/changsha/all_correlation_res.xlsx'
)

outpath <- 'output/0.figure/figure2/'


# process -----------------------------------------------------------------

res_list <- list()
for(g in names(dat_path_list)){
  
  dat_raw <- read_excel(dat_path_list[[g]])
  
  non_zero_min <- dat_raw$padj %>% 
    setdiff(., 0) %>% min()
  
  dat <- dat_raw %>% 
    filter(id !='age') %>% 
    mutate(padj=ifelse(padj==0, non_zero_min, padj)) %>% 
    mutate(logp=-log10(padj)) %>% 
    mutate(grp=g)
  colnames(dat)[-1] <- paste0(colnames(dat)[-1], '_', g)
  res_list[[g]] <- dat
  
}

res_df <- merge(res_list[[1]], res_list[[2]], by='id') %>% 
  mutate(size=rowMeans(select(., starts_with('logp')))) %>% 
  mutate(col=rowMeans(select(., starts_with('correlation')))) %>% 
  arrange(col) %>% 
  mutate(show = case_when(
    row_number() <= 5 ~ id,
    row_number() > n() - 5 ~ id,
    TRUE ~ ''
  )) %>% 
  mutate(sig=ifelse(padj_bj<0.05 & padj_cs<0.05 & correlation_bj*correlation_cs>0, 'y','n')) %>% 
  mutate(change=ifelse(col>0 & sig=='y', 'up', 'none')) %>% 
  mutate(change=ifelse(col<0 & sig=='y', 'down', change))

write_xlsx(
  res_df,
  paste0(outpath, 'merge_correlation.xlsx')
)

plt <- ggplot(res_df, aes(correlation_bj, correlation_cs, color=col, size=size)) +
  geom_point(data=subset(res_df, change!='none'), alpha = 0.5) +
  scale_color_gradient2(low='darkblue', mid='white', high='darkred') +
  geom_point(data=subset(res_df, change=='none'), color='grey', alpha = 0.5) +
  # scale_color_manual(values = c(up='darkred', down='darkblue')) +
  geom_text_repel(data=subset(res_df, change=='up'), aes(label = show), max.overlaps = Inf, nudge_x=-0.3, direction="y", hjust=1, size=2,box.padding=0.1) +
  geom_text_repel(data=subset(res_df, change=='down'), aes(label = show), max.overlaps = Inf, nudge_x=0.3, direction="y", hjust=-0.3, size=2,box.padding=0.1) +
  labs(x='', y='') +
  # coord_flip() +
  theme_bw() +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 6, color = 'black'),
    axis.text.x = element_text(size = 6, color = 'black'),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(color = 'black'),
    legend.position = 'none',
    panel.grid = element_blank()
  )

ggsave(
  filename = paste0(outpath, 'age_cor_dotplot.pdf'),
  plt,
  width = 2.5,
  height = 2.5
)









