# -------------------------------------
# Date: Sun Apr 20 23:37:29 2025
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


raw_dat_path_list <- list(
  bj='V20250427/output/use_dat/bj/correlation_dat_without_ic_score.xlsx',
  cs='V20250427/output/use_dat/cs/correlation_dat_without_ic_score.xlsx'
)
dat_path <- 'V20250427/output/Fig3/age_gaps_with_disease.xls'

follow_info_path_list <- list(
  bj='V20250427/data/beijing/北京医院随访.xlsx',
  cs='V20250427/data/changsha/3. 湘雅.xlsx'
)

outpath <- 'V20250427/output/Fig3/'


dat_df <- read.delim(
  file = dat_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
)


# merge dead --------------------------------------------------------------

annot_tmp_list <- list()

dead_id_list <- list()
for(g in names(follow_info_path_list)){
  
  if(g == 'cs'){
    
    annot_tmp_list[[g]] <- read_excel(raw_dat_path_list[[g]]) %>%
      select(pt_id,Gender) %>% 
      mutate(sour='cs')
    
    raw_dat <- read_excel(raw_dat_path_list[[g]]) %>% 
      select(pt_id, ID_numb) %>% 
      mutate(ID_numb=as.character(ID_numb))
    
    f_info <- read_excel(follow_info_path_list[[g]]) %>% 
      select(6, 12) %>% 
      rename(ID_numb=1, status=2) %>% 
      filter(status %in% c('良好','一般','已故')) %>% 
      mutate(status_use=ifelse(status=='已故', 'dead', ifelse(status=='一般', 'general','good'))) %>% 
      filter(status_use=='dead') %>% 
      pull(ID_numb)
    
    dead_id_list[[g]] <- raw_dat %>% 
      mutate(ID_numb=as.character(ID_numb)) %>% 
      filter(ID_numb %in% f_info) %>% 
      pull(pt_id)
    
    
  }else{
    
    annot_tmp_list[[g]] <- read_excel(raw_dat_path_list[[g]]) %>% 
      select(pt_id, Gender) %>% 
      mutate(sour='bj')
    
    raw_dat <- read_excel(raw_dat_path_list[[g]]) %>% 
      select(pt_id, q_id)
    
    f_info <- read_excel(follow_info_path_list[[g]]) %>% 
      pull(1)
    
    dead_id_list[[g]] <- raw_dat %>% 
      filter(q_id %in% f_info) %>% 
      pull(pt_id)
    
  }
  
  
}
dead_id_list <- unlist(dead_id_list)

annot_tmp <- bind_rows(annot_tmp_list)
# process -----------------------------------------------------------------


pdata <- dat_df %>% 
  select(pt_id, Age, type) %>% 
  mutate(status=ifelse(pt_id %in% dead_id_list, 1, 0)) %>% 
  filter(type!='Normal') %>% 
  filter(Age>=60) %>% 
  mutate(type=factor(type, c('Decelerated','Accelerated'))) %>% 
  merge(., annot_tmp, by='pt_id')

model <- glm(status ~ type + Age + Gender, data = pdata, family = binomial)
p_value <- summary(model)$coefficients["typeAccelerated", "Pr(>|z|)"]
esti <- summary(model)$coefficients["typeAccelerated", "Estimate"]

glm_res<- data.frame(
  estimate = esti,
  p_value = p_value
)


# 计算每组每种疾病的患病率和置信区间
prevalence <- pdata %>%
  group_by(type) %>%
  summarise(prevalence_rate = mean(status),
            ci_low = prevalence_rate - 1.96 * sqrt((prevalence_rate * (1 - prevalence_rate)) / n()),
            ci_high = prevalence_rate + 1.96 * sqrt((prevalence_rate * (1 - prevalence_rate)) / n()))

plt <- ggplot(prevalence, aes(type, prevalence_rate, fill=type)) +
  geom_bar(stat='identity', width = .5) +
  scale_fill_manual(values = c(Accelerated='#B8281D', Decelerated='#3773B6', Normal='grey')) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4, position = position_dodge(width = 0.5)) +
  labs(x='', y='Motality') +
  theme_bw() +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black'),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(color = 'black'),
    legend.position = 'none',
    panel.grid = element_blank()
  )
ggsave(
  filename = paste0(outpath, '3G_mortality_bar_stat.pdf'),
  plt,
  width = 3,
  height = 4
)


