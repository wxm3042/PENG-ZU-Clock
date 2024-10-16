# -------------------------------------
# Date: Mon Jul 15 17:39:17 2024
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

rm_path <- 'data/remove_disease_list.xlsx'
pt_disease_path <- 'data/bj_cs_disease_mtx.xlsx'
age_gaps_path <- 'output/3.clock/changsha_clock/elastic_net_scale/merge_bj_cs/age_gaps_with_disease.xls'
annot_path <- 'output/2.deg/merge_bj_cs_feature_for_correlation.xlsx'

outpath <- 'output/0.figure/figure3/merge/240926_correct_gender_sour/'

rm_disease <- read_excel(rm_path) %>% 
  pull(1) %>% gsub('\\.',' ',.)

annot <- read_excel(annot_path) %>% 
  select(patient_id, gender, sour) %>% 
  mutate(gender=ifelse(gender==1, 'male', 'female')) %>% 
  rename(id=1)

# process -----------------------------------------------------------------


age_gaps <- read.delim(
  file = age_gaps_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
)

pt_disease <- read_excel(pt_disease_path)

disease_pdata <- age_gaps %>% 
  filter(type != 'Normal') %>% 
  select(pt_id, age, type) %>%
  rename(id=1) %>% 
  merge(., pt_disease, by='id') %>% 
  mutate(type=factor(type, c('Decelerated', 'Accelerated'))) %>% 
  merge(., annot, by='id')

disease_list <- setdiff(colnames(pt_disease)[-1], rm_disease)

res_list <- list()

for(d in disease_list){
  
  model <- glm(as.formula(paste0('\`', d, '\`', "~ type + age + gender + sour")), data = disease_pdata, family = binomial)
  p_value <- summary(model)$coefficients["typeAccelerated", "Pr(>|z|)"]
  esti <- summary(model)$coefficients["typeAccelerated", "Estimate"]
  
  res_list[[d]] <- data.frame(
    estimate = esti,
    disease = d,
    p_value = p_value
  )
  
}

stat_res <- bind_rows(res_list) %>% 
  arrange(p_value)


# 转换数据框为长格式
data_long <- disease_pdata %>% 
  select(-id, -age, -gender, -sour) %>% 
  pivot_longer(., -1, names_to = 'disease', values_to = 'presence')

# 计算每组每种疾病的患病率和置信区间
prevalence <- data_long %>%
  group_by(type, disease) %>%
  summarise(prevalence_rate = mean(presence),
            ci_low = prevalence_rate - 1.96 * sqrt((prevalence_rate * (1 - prevalence_rate)) / n()),
            ci_high = prevalence_rate + 1.96 * sqrt((prevalence_rate * (1 - prevalence_rate)) / n()))

print(prevalence)


# 合并患病率数据框和逻辑回归结果数据框
prevalence_filter <- merge(prevalence, stat_res, by = "disease")

ord <- prevalence_filter %>% 
  select(disease, prevalence_rate) %>% 
  arrange(desc(prevalence_rate)) %>% 
  pull(disease) %>% unique()

prevalence_filter_sort <- prevalence_filter %>% 
  mutate(disease=factor(disease, ord)) %>% 
  arrange(disease)



stat_res_sort <- stat_res %>% 
  mutate(disease=factor(disease, ord)) %>% 
  arrange(disease)

label_use <- ifelse(stat_res_sort$p_value < 0.001, "***", 
                    ifelse(stat_res_sort$p_value < 0.01, "**", 
                           ifelse(stat_res_sort$p_value < 0.05, "*", '')))



plt <- ggplot(prevalence_filter_sort, aes(disease, prevalence_rate, color=type)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, position = position_dodge(width = .5)) +
  scale_color_manual(values = c(Slowdown='darkblue', Accelerated='darkred')) +
  labs(x='', y='') +
  theme_bw() +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 6, color = 'black'),
    axis.text.x = element_text(size = 6, color = 'black', angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 6, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(color = 'black'),
    legend.position = 'top'
  ) +
  annotate("text", x = 1:length(label_use), y = 0.6, label = label_use, size = 3.5, color = "black")
ggsave(
  filename = paste0(outpath, 'disease_ratio_in_agegaps_group_sig.pdf'),
  plt,
  width = 3,
  height = 3
)

