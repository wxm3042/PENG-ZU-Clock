# -------------------------------------
# Date: Fri Apr 25 08:36:51 2025
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
library(gamlss)
library(parallel)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)
cl <- makeCluster(8)  # 使用4个核心
clusterEvalQ(cl, {
  library(tidyverse)  # 例如，加载 ggplot2 包
  library(gamlss)    # 例如，加载 dplyr 包
})

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

dat_path_list <- list(
  male='V20250427/output/Fig8/male_centile_data.rds',
  female='V20250427/output/Fig8/female_centile_data.rds'
)
dat_path <- 'V20250427/output/Fig8/UKB_data_cor_feature.csv'
norml_range_path <- 'V20250427/data/feature_ref_range2.xlsx'
cut_range_path <- 'V20250427/data/ylim.xlsx'

outpath <- 'V20250427/output/Fig8/'

dat <- read.csv(dat_path, check.names = F)

cut_range <- read_excel(cut_range_path)
# process -----------------------------------------------------------------

pdata <- dat %>% 
  mutate(gender_use=ifelse(Gender==0, 'female', 'male')) %>% 
  select(eid, Age, Gender, gender_use, everything())


for(g in names(dat_path_list)){
  print(g)
  
  # g='all'
  
  model_dat <- readRDS(dat_path_list[[g]])
  
  if(g == 'all'){
    sub_dat <- pdata %>% 
      # filter(gender_use==g) %>%
      select(-Gender, -gender_use)
    
    norml_range <- read_excel(norml_range_path) %>% 
      select(1:6) %>% 
      filter_at(., -1, any_vars(!is.na(.))) %>% 
      mutate(keep=ifelse(age_low<30, 'n','y')) %>% 
      filter(keep=='y' | is.na(keep))
    
    # pt_age_df <- pdata
    
  }else if(g == 'male'){
    sub_dat <- pdata %>% 
      filter(gender_use==g) %>%
      select(-Gender, -gender_use)
    
    norml_range <- read_excel(norml_range_path, sheet='male') %>% 
      select(1:6) %>% 
      filter_at(., -1, any_vars(!is.na(.))) %>% 
      mutate(keep=ifelse(age_low<30, 'n','y')) %>% 
      filter(keep=='y' | is.na(keep))
    
    # pt_age_df <- pdata %>% 
    #   filter(gender_use=='M')
    
  }else{
    sub_dat <- pdata %>% 
      filter(gender_use==g) %>%
      select(-Gender, -gender_use)
    
    norml_range <- read_excel(norml_range_path, sheet='female') %>% 
      select(1:6) %>% 
      filter_at(., -1, any_vars(!is.na(.))) %>% 
      mutate(keep=ifelse(age_low<30, 'n','y')) %>% 
      filter(keep=='y' | is.na(keep)) %>% 
      filter(feature!=c('ALP', 'AST'))
    

  }
  
  res_list <- list()
  for(i in 1:length(model_dat)){
    
    print(i)
    s <- model_dat[[i]][['feature']]
    print(s)

    if(s %in% norml_range$feature){

      tpdata <- sub_dat %>% 
        select(Age, all_of(s)) %>% 
        rename(fea = 2) %>% 
        filter(!is.na(fea)) %>% 
        filter(fea > 0)
      age_list <- unique(tpdata$Age) %>% sort()
      
      gammodel <- model_dat[[i]][['model']]
      
      # 使用 parLapply 进行并行处理
      clusterExport(cl, varlist = c("gammodel", "i", "tpdata", "s"))
      
      age_res_list <- parLapply(cl, age_list, function(ag) {
        centiles_pred_25 <- centiles.pred(
          gammodel,
          xname = "Age",
          xvalues = ag,  # 单个年龄值
          cent = c(25, 75)  # 计算 25% 和 75% 百分位
        )
        tmp_res <- centiles_pred_25 %>% 
          rename(age=1) %>% 
          mutate(fea=s)
        
        return(tmp_res)
      }) %>% 
        bind_rows(.) %>% 
        mutate(fea = s)
      
      res_list[[i]] <- age_res_list
    }
    
    
  }
  res_df <- bind_rows(res_list) %>% 
    mutate(gender=g)
  
  write.table(
    res_df,
    file = paste0(outpath, g, '_25_75_threshold_criterial.txt'),
    sep = '\t',
    quote = F,
    row.names = F
  )
}

stopCluster(cl)


