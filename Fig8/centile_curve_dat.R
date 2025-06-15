# -------------------------------------
# Date Fri Apr 25 02:03:55 2025
# Script 
# Author WXM
# Purpose 
# Notes 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(gamlss)
library(Hmisc)
library(parallel)

library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)
cl <- makeCluster(6)  # 使用4个核心

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

dat_path <- 'V20250427/output/Fig8/UKB_data_cor_feature.csv'

outpath <- 'V20250427/output/Fig8/'

dat <- read.csv(dat_path, check.names = F)

# process -----------------------------------------------------------------

pdata <- dat %>% 
  mutate(gender_use=ifelse(Gender==0, 'F', 'M')) %>% 
  select(eid, Age, Gender, gender_use, everything())

# centile curves ----------------------------------------------------------

show_fea_list <- colnames(pdata)[-c(1:4)]
# 使用 parLapply 进行并行处理
clusterExport(cl, varlist = c("pdata"))


# all
result <- parLapply(cl, show_fea_list, function(s) {
  library(tidyverse)
  library(gamlss)
  
  tpdata <- pdata %>% 
    select(Age, all_of(s)) %>% 
    rename(fea = 2) %>% 
    filter(!is.na(fea)) %>% 
    filter(fea > 0)
  
  gammodel <- tryCatch({
    gamlss(fea ~ pb(Age), sigma.fo = ~ pb(Age), nu.fo = ~ pb(Age), family = BCT, data = tpdata)
  }, error = function(e) {
    NULL
  })
  
  # 返回每个特征的模型或 NULL
  return(list(feature = s, model = gammodel))
})

saveRDS(result, paste0(outpath, 'all_centile_data.rds'))

# male
result <- parLapply(cl, show_fea_list, function(s) {
  library(tidyverse)
  library(gamlss)
  
  tpdata <- pdata %>% 
    filter(gender_use=='M') %>% 
    select(Age, all_of(s)) %>% 
    rename(fea = 2) %>% 
    filter(!is.na(fea)) %>% 
    filter(fea > 0)
  
  gammodel <- tryCatch({
    gamlss(fea ~ pb(Age), sigma.fo = ~ pb(Age), nu.fo = ~ pb(Age), family = BCT, data = tpdata)
  }, error = function(e) {
    NULL
  })
  
  # 返回每个特征的模型或 NULL
  return(list(feature = s, model = gammodel))
})

saveRDS(result, paste0(outpath, 'male_centile_data.rds'))

# female
result <- parLapply(cl, show_fea_list, function(s) {
  library(tidyverse)
  library(gamlss)
  
  tpdata <- pdata %>% 
    filter(gender_use=='F') %>% 
    select(Age, all_of(s)) %>% 
    rename(fea = 2) %>% 
    filter(!is.na(fea)) %>% 
    filter(fea > 0)
  
  gammodel <- tryCatch({
    gamlss(fea ~ pb(Age), sigma.fo = ~ pb(Age), nu.fo = ~ pb(Age), family = BCT, data = tpdata)
  }, error = function(e) {
    NULL
  })
  
  # 返回每个特征的模型或 NULL
  return(list(feature = s, model = gammodel))
})

saveRDS(result, paste0(outpath, 'female_centile_data.rds'))

stopCluster(cl)


