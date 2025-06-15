# -------------------------------------
# Date: Thu May 22 08:53:18 2025
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
conflict_prefer("select", "dplyr")

# detach(unload='conflicted')
# input -------------------------------------------------------------------

merge_dat_path <- 'V20250427/output/use_dat/merge_data.xlsx'
show_fea_path <- 'V20250427/output/Fig1/show_fea.xlsx'
cs_health_dat_path <- 'V20250427/output/use_dat/cs/clock_data_normal.xlsx'
pt_id_path_list <- list(
  bj='V20250427/output/Fig2/clock/bj_test/age_gaps.xlsx',
  cs_train='V20250427/output/Fig2/clock/cs_normal_train/train_pt_id.txt',
  cs_test_disease = 'V20250427/output/Fig2/clock/cs_disease_test/predict_age_in_test_data.xlsx',
  cs_test_normal = 'V20250427/output/Fig2/clock/cs_normal_test/predict_age_in_test_data.xlsx'
)

data_path_list <- list(
  bj='V20250427/data/beijing/beijing_add_eGFR_corrected.xlsx',
  cs='V20250427/data/changsha/changsha_add_eGFR_corrected.xlsx'
)

fea_info_path_list <- list(
  bj='V20250427/data/beijing/副本北京医院2596例final原表_full-矫正.xlsx',
  cs='V20250427/data/changsha/changsha_colnamedf.xlsx'
)

disease_mtx_path <- 'V20250427/data/bj_cs_disease_mtx.xlsx'

activity_path <- 'V20250427/data/ADL与IADL数据.xlsx'

outpath <- 'V20250427/output/Fig1/'

show_fea <- read_excel(show_fea_path) %>% 
  filter(keep=='y')

activity_df <- read_excel(activity_path) %>% 
  select(1, 6, 7) %>% 
  rename(ADL=2) %>% 
  filter(!is.na(ADL) & !is.na(IADL)) %>% 
  mutate(IADL=gsub('好','Good',IADL)) %>% 
  mutate(IADL=gsub('能力减退','Reduced capacity',IADL)) %>% 
  mutate(IADL=gsub('能力缺失','Lack of capacity',IADL)) %>% 
  mutate(IADL=gsub('尚可','Acceptable',IADL)) %>% 
  rename(q_id=1) %>% 
  filter(!is.na(q_id))

disease_mtx <- read_excel(disease_mtx_path) %>% 
  rename(pt_id=1) %>% 
  select(pt_id, Hypertension, Diabetes)

sub_fea_annot <- read_excel(fea_info_path_list[['bj']], sheet='q_fea') %>% 
  fill(name, group) %>% 
  filter(group %in% show_fea$feature)

merge_dat <- read_excel(merge_dat_path) %>% 
  select(pt_id, sour)
cs_health_dat <- read_excel(cs_health_dat_path)



# id_use ------------------------------------------------------------------

cs_normal1 <- read.delim(
  file = pt_id_path_list[['cs_train']],
  sep = '\t',
  quote = '',
  header = F,
  check.names = F
) %>% rename(pt_id=1) %>%
  mutate(pt_sour='CS', pt_type='Healthy')
cs_normal2 <- read_excel(pt_id_path_list[['cs_test_normal']]) %>%
  mutate(pt_sour='CS', pt_type='Healthy') %>%
  select(pt_id, pt_sour, pt_type)
cs_disease <- read_excel(pt_id_path_list[['cs_test_disease']]) %>%
  mutate(pt_sour='CS', pt_type='Unhealthy') %>%
  select(pt_id, pt_sour, pt_type)
bj_all <- read_excel(pt_id_path_list[['bj']]) %>%
  mutate(pt_sour='BJ', pt_type='All') %>%
  select(pt_id, pt_sour, pt_type)

all_pt_df <- bind_rows(list(cs_normal1, cs_normal2, cs_disease, bj_all))


# process -----------------------------------------------------------------

all_dat_list <- list()

for(g in names(data_path_list)){
  
  if(g == 'bj'){
    
    dat_df <- read_excel(data_path_list[[g]]) %>% 
      rename(pt_id=1) %>% 
      merge(., activity_df, by='q_id', all.x=T) %>% 
      merge(., disease_mtx, by='pt_id', all.x=T)
    
    annot1 <- read_excel(fea_info_path_list[[g]], sheet=2) %>% 
      select(id, new_name) %>% 
      rename(feature=2)
    annot2 <- read_excel(fea_info_path_list[[g]], sheet=4) %>% 
      select(id, group) %>% 
      rename(feature=2)
    annot3 <- data.frame(
      id=c('ADL', 'IADL', 'Hypertension', 'Diabetes'),
      feature=c('ADL', 'IADL', 'Hypertension', 'Diabetes')
    )
    annot <- rbind(annot1, annot2) %>% 
      rbind(., annot3) %>% 
      filter(feature %in% show_fea$feature) %>% 
      unique() %>% 
      mutate(feature=factor(feature, show_fea$feature)) %>% 
      arrange(feature) %>% 
      mutate(id=ifelse(feature=='Age', 'age', id)) %>% 
      mutate(id=ifelse(feature=='Gender', 'gender', id))
    
    bj_datdf <- dat_df %>% 
      select(pt_id, all_of(annot$id))
    colnames(bj_datdf)[-1] <- annot$feature %>% as.character()
    all_dat_list[[g]] <- bj_datdf
    
  }else{
    
    dat_df <- read_excel(data_path_list[[g]]) %>% 
      rename(pt_id=1, q_id=36) %>% 
      merge(., activity_df, by='q_id', all.x=T) %>% 
      merge(., disease_mtx, by='pt_id', all.x=T)
    
    annot1 <- read_excel(fea_info_path_list[[g]], sheet=1) %>% 
      select(raw, new_name) %>% 
      rename(feature=2)
    annot2 <- read_excel(fea_info_path_list[[g]], sheet=2) %>% 
      select(id, group) %>% 
      rename(raw=1, feature=2)
    annot3 <- data.frame(
      raw=c('ADL', 'IADL', 'Hypertension', 'Diabetes'),
      feature=c('ADL', 'IADL', 'Hypertension', 'Diabetes')
    )
    annot <- rbind(annot1, annot2) %>% 
      rbind(., annot3) %>% 
      filter(feature %in% show_fea$feature) %>% 
      unique() %>% 
      mutate(feature=factor(feature, show_fea$feature)) %>% 
      arrange(feature)
    
    cs_datdf <- dat_df %>% 
      select(pt_id, all_of(annot$raw))
    colnames(cs_datdf)[-1] <- annot$feature %>% as.character()
    cs_datdf$Gender <- ifelse(cs_datdf$Gender=='男', 1, 2)
    
    all_dat_list[[g]] <- cs_datdf
  }
  
}


all_dat <- bind_rows(all_dat_list) %>% 
  rename(Sex=2) %>% 
  merge(., all_pt_df, by='pt_id')

all_dat_feautre <- show_fea %>% 
  mutate(feature=ifelse(feature=='Gender', 'Sex', feature))


all_dat_use <- all_dat %>% 
  mutate(Education=ifelse(Education %in% c(1:3),1, Education)) %>% 
  mutate(Education=ifelse(Education >5 ,5, Education)) %>% 
  # mutate(`Average Daily Interactions`=ifelse(`Average Daily Interactions` == 2, 1, `Average Daily Interactions`)) %>% 
  mutate(`Location of Current Residence`=ifelse(`Location of Current Residence` ==2, 1, `Location of Current Residence`)) %>% 
  mutate(`Location of Current Residence`=ifelse(`Location of Current Residence` %in% c(3,4), 5, `Location of Current Residence`)) %>% 
  mutate(`Location of Current Residence`=ifelse(`Location of Current Residence` %in% c(0,6,8), NA, `Location of Current Residence`)) %>% 
  mutate(Sex=factor(Sex, levels=c(1,2), labels=c('Male', 'Female'))) %>% 
  mutate(pt_type=factor(pt_type, c('Healthy','Unhealthy','All')))
# mutate(IADL=factor(IADL, c('Good', 'Acceptable', 'Reduced capacity', 'Lack of capacity'))) %>% 
# mutate(ADL=as.numeric(ADL))


for(tg in unique(sub_fea_annot$group)){
  
  use_sug_b <- sub_fea_annot %>% 
    filter(group==tg) %>% 
    pull(choose) %>% 
    intersect(., all_dat_use[[tg]] %>% unique())
  
  use_short_id <- sub_fea_annot %>% 
    filter(group==tg) %>% 
    filter(choose %in% use_sug_b) %>% 
    pull(short_id)
  
  all_dat_use[[tg]][!(all_dat_use[[tg]] %in% use_sug_b)] <- NA
  
  all_dat_use[[tg]] <- factor(all_dat_use[[tg]], levels = use_sug_b, labels = use_short_id)
  
}

res_list <- list()
pval_list <- list()
for(fe in unique(all_dat_feautre$feature)){
  print(fe)
  fe_type <- all_dat_feautre %>% 
    filter(feature == fe) %>% 
    pull(val_type)
  if(fe_type == "categorical"){
    
    all_stat_res <- all_dat_use %>% 
      mutate(new_type='Overall') %>% 
      select(all_of(fe), new_type) %>% 
      table() %>% 
      data.frame() %>%
      mutate(ratio=round(Freq/sum(Freq) *100, 1)) %>% 
      mutate(show_rt=paste0(Freq, ' (', ratio, '%)')) %>% 
      select(-Freq, -ratio) %>% 
      pivot_wider(., names_from = 'new_type', values_from = 'show_rt') %>% 
      rename(fea=1)
    
    stat_res <- all_dat_use %>% 
      select(all_of(fe), pt_type) %>% 
      table() %>% 
      data.frame() %>% 
      group_by(pt_type) %>% 
      mutate(ratio=round(Freq/sum(Freq) *100, 1)) %>% 
      mutate(show_rt=paste0(Freq, ' (', ratio, '%)')) %>% 
      select(-Freq, -ratio) %>% 
      pivot_wider(., names_from = 'pt_type', values_from = 'show_rt') %>% 
      rename(fea=1) %>% 
      merge(., all_stat_res, by='fea') %>% 
      mutate(class=fe)
    res_list[[fe]] <- stat_res
    
    # pval
    sub_dat <- all_dat_use %>% 
      select(pt_type, all_of(fe)) %>% 
      filter(pt_type!='All') %>% 
      mutate(pt_type= factor(pt_type, c('Healthy', 'Unhealthy'))) %>% 
      table(.)
    pval_list[[fe]] <- chisq.test(sub_dat)$p.value %>% 
      data.frame(feature=fe, pval=.)
    
  }else{
    
    shapiro_p <- all_dat_use %>% 
      pull(fe) %>% 
      shapiro.test(.) %>% 
      .$p.value
    if(shapiro_p<0.05){ # 非正态分布
      
      all_stat_res <- all_dat_use %>% 
        mutate(new_type='Overall') %>% 
        select(all_of(fe), new_type) %>% 
        rename(val=1) %>% 
        summarise(
          med=median(val, na.rm=T),
          Q1 = quantile(val, probs = 0.25, na.rm = TRUE),
          Q3 = quantile(val, probs = 0.75, na.rm = TRUE)
        ) %>% 
        mutate(Overall=paste0(med, ' (', Q1, ',', Q3, ')')) %>% 
        mutate(fea=fe) %>% 
        select(-med, -Q1, -Q3) %>%
        select(fea, everything())
      
      
      stat_res <- all_dat_use %>% 
        select(all_of(fe), pt_type) %>%
        group_by(pt_type) %>% 
        rename(val=1) %>% 
        summarise(
          med=median(val, na.rm=T),
          Q1 = quantile(val, probs = 0.25, na.rm = TRUE),
          Q3 = quantile(val, probs = 0.75, na.rm = TRUE)
          ) %>% 
        mutate(show_rt=paste0(med, ' (', Q1, ',', Q3, ')')) %>% 
        select(-med, -Q1, -Q3) %>% 
        pivot_wider(., names_from = 'pt_type', values_from = 'show_rt') %>% 
        mutate(fea=fe) %>% 
        merge(., all_stat_res, by='fea') %>% 
        mutate(class=fe) %>% 
        select(fea, everything())
      
    }else{
      
      all_stat_res <- all_dat_use %>% 
        mutate(new_type='Overall') %>% 
        select(all_of(fe), new_type) %>% 
        rename(val=1) %>% 
        summarise(
          mea=mean(val, na.rm=T) %>% round(., 1),
          sd_val = sd(val, na.rm = TRUE) %>% round(., 1)
        ) %>% 
        mutate(Overall=paste0(mea, '±', sd_val)) %>% 
        mutate(fea=fe) %>% 
        select(-mea, -sd_val) %>%
        select(fea, everything())
      
      
      stat_res <- all_dat_use %>% 
        select(all_of(fe), pt_type) %>%
        group_by(pt_type) %>% 
        rename(val=1) %>% 
        summarise(
          mea=mean(val, na.rm=T) %>% round(., 1),
          sd_val = sd(val, na.rm = TRUE) %>% round(., 1)
        ) %>% 
        mutate(show_rt=paste0(mea, '±', sd_val)) %>% 
        select(-mea, -sd_val) %>% 
        pivot_wider(., names_from = 'pt_type', values_from = 'show_rt') %>% 
        mutate(fea=fe) %>% 
        merge(., all_stat_res, by='fea') %>% 
        mutate(class=fe) %>% 
        select(fea, everything())

    }
    res_list[[fe]] <- stat_res
    
    
    # pval
    
    sub_dat <- all_dat_use %>% 
      select(pt_type, all_of(fe)) %>% 
      filter(pt_type!='All') %>% 
      mutate(pt_type= factor(pt_type, c('Healthy', 'Unhealthy'))) %>% 
      rename(fea_val=2)
    
    pval_list[[fe]] <- wilcox.test(fea_val~pt_type, data = sub_dat)$p.value%>% 
      data.frame(feature=fe, pval=.)
  }
  
}
res_df <- bind_rows(res_list)
write_xlsx(
  res_df,
  paste0(outpath, 'table_stat.xlsx')
)
pval_df <- bind_rows(pval_list)
write_xlsx(
  pval_df,
  paste0(outpath, 'table_stat_pval.xlsx')
)









