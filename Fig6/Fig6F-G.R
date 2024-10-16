# -------------------------------------
# Date: Mon Jul 22 02:07:28 2024
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(readxl)
library(ggpubr)
library(reshape2)
library(writexl)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

adl_dat_path <- 'data/ADL与IADL数据.xlsx'
age_gap_path <- 'output/3.clock/changsha_clock/elastic_net_scale/simplify_clock/merge//age_gaps_with_disease.xls'
raw_Dat_path_list <- list(
  bj='data/beijing/beijing_add_eGFR_corrected.xlsx',
  cs='data/changsha/changsha_add_eGFR_corrected.xlsx'
)

outpath <- 'output/0.figure/figure6/merge/'

adl_dat <- read_excel(adl_dat_path) %>% 
  select(1, 6,7) %>% 
  rename(ADL=2) %>% 
  filter(!is.na(ADL) & !is.na(IADL)) %>% 
  mutate(IADL=gsub('好','Good',IADL)) %>% 
  mutate(IADL=gsub('能力减退','Reduced capacity',IADL)) %>% 
  mutate(IADL=gsub('能力缺失','Lack of capacity',IADL)) %>% 
  mutate(IADL=gsub('尚可','Acceptable',IADL)) %>% 
  rename(q_id=1)

age_gap <- read.delim(
  file = age_gap_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
) %>% rename(patient_id=1)

# process -----------------------------------------------------------------

dat_list <- list()
for(g in names(raw_Dat_path_list)){
  
  if(g == 'bj'){
    dat_list[[g]] <- read_excel(raw_Dat_path_list[[g]]) %>% 
      select(patient_id, q_id) %>% 
      filter(!is.na(q_id))
  }else{
    dat_list[[g]] <- read_excel(raw_Dat_path_list[[g]]) %>% 
      select(1, 36) %>% 
      rename(patient_id=1, q_id=2) %>% 
      filter(!is.na(q_id))
  }
  
}

raw_dat_mg <- bind_rows(dat_list)


pdata <- raw_dat_mg %>% 
  merge(., age_gap, by='patient_id') %>% 
  merge(., adl_dat, by='q_id') %>% 
  filter(type!='Normal', age>=60) %>% 
  mutate(type=factor(type, c('Slowdown','Accelerated'))) %>% 
  mutate(ADL=as.numeric(ADL))



plt <- ggbarplot(
  pdata,
  x = "type",
  y = "ADL",
  add = "mean_se",
  fill = "type",
  palette = c(Accelerated='darkred', Slowdown='darkblue', Normal='grey'),
  position = position_dodge(0.8)
) +
  stat_compare_means(label.y = 0.2) +
  labs(x='', y='ADL') +
  theme_bw() +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black'),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    # panel.border = element_rect(color = 'black'),
    legend.position = 'none',
    panel.grid = element_blank()
  )
ggsave(
  filename = paste0(outpath, 'ADL_barplot.pdf'),
  plt,
  width = 4,
  height = 3.5
)

# IADL

pdata2 <- pdata %>% 
  select(type, age, IADL) %>% 
  mutate(status=ifelse(IADL =='Lack of capacity', 1, 0)) %>% 
  filter(type!='Normal') %>% 
  mutate(type=factor(type, c('Slowdown','Accelerated')))

model <- glm(status ~ type + age + gender + sour, data = pdata2, family = binomial)
p_value <- summary(model)$coefficients["typeAccelerated", "Pr(>|z|)"]
esti <- summary(model)$coefficients["typeAccelerated", "Estimate"]

glm_res<- data.frame(
  estimate = esti,
  p_value = p_value
)


# 计算每组每种疾病的患病率和置信区间
prevalence <- pdata2 %>%
  group_by(type) %>%
  summarise(prevalence_rate = mean(status),
            ci_low = prevalence_rate - 1.96 * sqrt((prevalence_rate * (1 - prevalence_rate)) / n()),
            ci_high = prevalence_rate + 1.96 * sqrt((prevalence_rate * (1 - prevalence_rate)) / n()))

plt <- ggplot(prevalence, aes(type, prevalence_rate, fill=type)) +
  geom_bar(stat='identity') +
  scale_fill_manual(values = c(Accelerated='darkred', Slowdown='darkblue', Normal='grey')) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4, position = position_dodge(width = 0.5)) +
  labs(x='', y='Disability Ratio') +
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
  filename = paste0(outpath, 'Lack of capacity_bar_stat.pdf'),
  plt,
  width = 4,
  height = 4
)


