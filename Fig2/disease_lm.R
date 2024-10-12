# -------------------------------------
# Date: Thu Aug 15 12:27:48 2024
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
  bj='data/beijing/beijing_disease_mtx.xlsx',
  cs='data/changsha/changsha_disease_dat_mtx.xlsx'
)

dat_path_list <- list(
  bj='data/beijing/beijing_add_eGFR_corrected.xlsx',
  cs='data/changsha/changsha_add_eGFR_corrected.xlsx'
)

age_path <- 'output/3.clock/changsha_clock/elastic_net_scale/merge_bj_cs/age_gaps.xlsx'

outpath <- 'output/0.figure/figure3/'

delta_age <- read_excel(age_path) %>% 
  rename(samp_id=1) %>% 
  select(samp_id, age_gaps, age) %>% 
  rename(patient_id=1)

show_disease_list <- read_excel(disease_path_list[['bj']]) %>% 
  select(-1) %>% 
  colnames(.)

# show_disease_list <- c(
#   'Hypertension',
#   'Diabetes',
#   'Arthritis',
#   'Metabolic disorders',
#   'Heart disease',
#   'Cataracts'
# )

# process -----------------------------------------------------------------

disease_df_list <- list()
dat_res_list <- list()
for(g in names(disease_path_list)){
  disease_df_list[[g]] <- read_excel(disease_path_list[[g]]) %>%
    select(id, all_of(show_disease_list))
  if(g == 'cs'){
    dat_res_list[[g]] <- read_excel(dat_path_list[[g]]) %>% 
      select(pid, `s1_性别`) %>% 
      rename(patient_id=1, gender=2) %>% 
      mutate(sour='cs') %>% 
      mutate(gender=ifelse(gender=='男', 1, 2))
  }else{
    dat_res_list[[g]] <- read_excel(dat_path_list[[g]]) %>% 
      select(patient_id, gender) %>% 
      mutate(sour='bj')
  }
}

disease_df <- bind_rows(disease_df_list) %>% 
  rename(patient_id=1) %>% 
  mutate(`Heart disease`=ifelse(`Heart disease`>1, 0, `Heart disease`)) %>% 
  mutate_at(., -1, ~as.factor(.))
  
dat_res <- bind_rows(dat_res_list)

pdata <- delta_age %>% 
  rename(patient_id=1) %>% 
  merge(., disease_df, by='patient_id') %>% 
  merge(., dat_res, by='patient_id')

res_list <- list()

for(d in show_disease_list){
  
  sub_dat <- pdata %>% 
    select(age_gaps, age, gender, sour, all_of(d)) %>% 
    rename(feature=5)
  
  fit_res <- lm(age_gaps ~ feature + age + gender + sour, data = sub_dat)
  
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
write_xlsx(
  res_df,
  paste0(outpath, 'merge/disease_lm_all.xlsx')
)

bar_pdata <- res_df %>% 
  filter(pval<0.05) %>% 
  arrange(Estimate) %>% 
  mutate(group=factor(group, group))

plt <- ggplot(bar_pdata, aes(group, Estimate, fill=Estimate)) +
  geom_bar(stat='identity', color='black') +
  scale_fill_gradient2(low='darkblue',mid = 'white',high = 'darkred') +
  scale_y_continuous(limits = c(-2,7), breaks = seq(-2, 7, 1)) +
  labs(x='', y='coefficient') +
  coord_flip() +
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
  filename = paste0(outpath, 'merge/disease_age_gaps_barplot.pdf'),
  plt,
  width = 4,
  height = 4
)

# single sour -------------------------------------------------------------


for(g in names(disease_path_list)){
  
  spdata <- pdata %>% 
    filter(sour==g)
  
  res_list <- list()
  
  for(d in show_disease_list){
    
    sub_dat <- spdata %>% 
      select(age_gaps, age, gender, sour, all_of(d)) %>% 
      rename(feature=5)
    # if(d != 'Normal'){
    #   sub_dat <- spdata %>% 
    #     select(age_gaps, age, gender, sour, all_of(d), Normal) %>% 
    #     rename(feature=5) %>% 
    #     filter(!(feature==0 & Normal!=1))
    # }else{
    #   sub_dat <- spdata %>% 
    #     select(age_gaps, age, gender, sour, all_of(d)) %>% 
    #     rename(feature=5)
    #   
    # }
    
    fit_res <- lm(age_gaps ~ feature + age + gender, data = sub_dat)
    
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
  write_xlsx(
    res_df,
    paste0(outpath, g, '/disease_lm.xlsx')
  )
}









