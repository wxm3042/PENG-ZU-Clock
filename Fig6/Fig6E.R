# -------------------------------------
# Date: Mon Jul 22 02:05:26 2024
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
  bj='data/beijing/beijing_add_eGFR_corrected.xlsx',
  cs='data/changsha/changsha_add_eGFR_corrected.xlsx'
)
dat_path <- 'output/3.clock/changsha_clock/elastic_net_scale/simplify_clock/merge//age_gaps_with_disease.xls'

follow_info_path_list <- list(
  bj='data/beijing/北京医院随访.xlsx',
  cs='data/changsha/3. 湘雅.xlsx'
)

outpath <- 'output/0.figure/figure6/merge/'


dat_df <- read.delim(
  file = dat_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
)


# merge dead --------------------------------------------------------------

dead_id_list <- list()
gender_sour_list <- list()
for(g in names(follow_info_path_list)){
  
  if(g == 'cs'){
    
    raw_dat <- read_excel(raw_dat_path_list[[g]]) %>% 
      select(1,11,6) %>% 
      rename(id_numb=2, gender=3) %>% 
      mutate(gender=ifelse(gender=='男', 'male', 'female')) %>% 
      mutate(sour='cs')
    
    
    f_info <- read_excel(follow_info_path_list[[g]]) %>% 
      select(6, 12) %>% 
      rename(id_numb=1, status=2) %>% 
      filter(status %in% c('良好','一般','已故')) %>% 
      mutate(status_use=ifelse(status=='已故', 'dead', ifelse(status=='一般', 'general','good'))) %>% 
      filter(status_use=='dead') %>% 
      pull(id_numb)
    
    dead_id_list[[g]] <- raw_dat %>% 
      filter(id_numb %in% f_info) %>% 
      pull(pid)
    
    
  }else{
    
    raw_dat <- read_excel(raw_dat_path_list[[g]]) %>% 
      select(1,2,5) %>% 
      mutate(gender=ifelse(gender==1, 'male', 'female')) %>% 
      rename(pid=1, id_numb=2) %>% 
      mutate(sour='bj')
    
    
    f_info <- read_excel(follow_info_path_list[[g]]) %>% 
      pull(1)
    
    dead_id_list[[g]] <- raw_dat %>% 
      filter(id_numb %in% f_info) %>% 
      pull(pid)
    
  }
  
  gender_sour_list[[g]] <- raw_dat
  
}

gender_sourdf <- bind_rows(gender_sour_list) %>% 
  select(-2)

dead_id_list <- unlist(dead_id_list)

# process -----------------------------------------------------------------


pdata <- dat_df %>% 
  select(pt_id, age, type) %>% 
  mutate(status=ifelse(pt_id %in% dead_id_list, 1, 0)) %>% 
  filter(type!='Normal') %>% 
  filter(age>=60) %>% 
  rename(pid=1) %>% 
  merge(., gender_sourdf, by='pid') %>% 
  mutate(type=ifelse(type=='Slowdown', 'Decelerated', type)) %>% 
  mutate(type=factor(type, c('Decelerated','Accelerated')))

model <- glm(status ~ type + age +gender + sour, data = pdata, family = binomial)
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
  geom_bar(stat='identity') +
  scale_fill_manual(values = c(Accelerated='darkred', Slowdown='darkblue', Normal='grey')) +
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
    legend.position = 'top',
    panel.grid = element_blank()
  )
ggsave(
  filename = paste0(outpath, 'mortality_bar_stat.pdf'),
  plt,
  width = 4,
  height = 4
)


