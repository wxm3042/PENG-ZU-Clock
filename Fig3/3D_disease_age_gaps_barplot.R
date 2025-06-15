# -------------------------------------
# Date: Sun Apr 20 17:47:12 2025
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



disease_path_list <- list(
  bj='V20250427/data/beijing/beijing_disease_mtx.xlsx',
  cs='V20250427/data/changsha/changsha_disease_dat_mtx.xlsx'
)
merge_dat_path <- 'V20250427/output/use_dat/merge_data.xlsx'
age_path <- 'V20250427/output/Fig2/clock/age_gaps.xlsx'

outpath <- 'V20250427/output/Fig3/'

delta_age <- read_excel(age_path) %>% 
  select(pt_id, age_gaps, Age)

show_disease_list <- read_excel(disease_path_list[['bj']]) %>% 
  select(-1) %>% 
  colnames(.)

show_disease_list <- c(
  'Hypertension',
  'Diabetes',
  'Heart disease',
  'Metabolic disorders',
  'Normal'
)

merge_dat <- read_excel(merge_dat_path) %>% 
  select(pt_id, Gender, sour)
# process -----------------------------------------------------------------

disease_df_list <- list()
# dat_res_list <- list()
for(g in names(disease_path_list)){
  disease_df_list[[g]] <- read_excel(disease_path_list[[g]]) %>%
    select(pt_id, all_of(show_disease_list))
  

}

disease_df <- bind_rows(disease_df_list) %>% 
  mutate(`Heart disease`=ifelse(`Heart disease`>1, 0, `Heart disease`)) %>% 
  mutate_at(., -1, ~as.factor(.))


pdata <- delta_age %>% 
  merge(., disease_df, by='pt_id') %>% 
  merge(., merge_dat, by='pt_id')

res_list <- list()

for(d in show_disease_list){
  
  if(d !='Normal'){
    sub_dat <- pdata %>% 
      select(age_gaps, Age, Gender, sour, all_of(d), Normal) %>% 
      rename(feature=5) %>% 
      filter(feature==1 | Normal==1) %>% 
      mutate(sour=factor(sour))
  }else{
    sub_dat <- pdata %>% 
      select(age_gaps, Age, Gender, sour, all_of(d)) %>% 
      rename(feature=5) %>% 
      mutate(sour=factor(sour))
    
  }
  
  
  fit_res <- lm(age_gaps ~ feature + Age + Gender + sour, data = sub_dat)
  
  coef_res <- summary(fit_res)$coef %>% 
    data.frame() %>% 
    rownames_to_column('feature') %>% 
    mutate(group=d) %>% 
    mutate(ratio=sum(sub_dat$feature %>% as.character() %>% as.numeric())/nrow(sub_dat))
  res_list[[d]] <- coef_res
}

res_df <- bind_rows(res_list) %>% 
  rename(pval=5) %>% 
  filter(feature=='feature1') %>% 
  arrange(pval)


bar_pdata <- res_df %>% 
  filter(group %in% show_disease_list) %>% 
  # filter(pval<0.05) %>% 
  arrange(Estimate) %>% 
  mutate(group=factor(group, group))

plt <- ggplot(bar_pdata, aes(group, Estimate, fill=Estimate)) +
  geom_bar(stat='identity', color='black', width=.7) +
  scale_fill_gradient2(low='#3773B6',mid = 'white',high = '#B8281D') +
  scale_y_continuous(limits = c(-2,7), breaks = seq(-2, 7, 1)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_text(
    aes(label = round(Estimate,2)), 
    vjust = .5,            # 垂直位置调整
    hjust=0,
    color = "black",      # 文字颜色
  ) +
  ylim(-3.5, 9) +
  labs(x='', y='coefficient') +
  coord_flip() +
  theme_bw() +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black'),
    # axis.text.x = element_text(size = 12, color = 'black', angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(color = 'black'),
    legend.position = 'top',
    panel.grid = element_blank()
  )
ggsave(
  filename = paste0(outpath, '3D_disease_age_gaps_barplot.pdf'),
  plt,
  width = 4.5,
  height = 3
)


