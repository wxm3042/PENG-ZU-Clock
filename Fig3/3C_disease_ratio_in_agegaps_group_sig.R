# -------------------------------------
# Date: Sun Apr 20 16:11:43 2025
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
dat_path <- 'V20250427/output/use_dat/merge_data.xlsx'
pt_disease_path <- 'V20250427/output/Fig3/disease_dat.xlsx'
age_gaps_path <- 'V20250427/output/Fig3/age_gaps_with_disease.xls'

outpath <- 'V20250427/output/Fig3/'

dat_mtx <- read_excel(dat_path) %>% 
  select(pt_id, Gender, sour) %>% 
  mutate(pt_id=gsub('patient', 'pt', pt_id)) %>% 
  rename(id=1)

age_gaps <- read.delim(
  file = age_gaps_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
) %>% mutate(pt_id=gsub('patient', 'pt', pt_id))
pt_disease <- read_excel(pt_disease_path)

# process -----------------------------------------------------------------

disease_pdata <- age_gaps %>% 
  filter(type != 'Normal') %>% 
  select(pt_id, Age, type) %>%
  rename(id=1) %>% 
  merge(., pt_disease, by='id') %>% 
  # merge(., dat_mtx, by='id') %>% 
  mutate(type=factor(type, c('Decelerated', 'Accelerated')))

disease_list <- colnames(pt_disease)[-1]

res_list <- list()

for(d in disease_list){
  
  model <- glm(as.formula(paste0('\`', d, '\`', "~ type + Age")), data = disease_pdata, family = binomial)
  p_value <- summary(model)$coefficients["typeAccelerated", "Pr(>|z|)"]
  esti <- summary(model)$coefficients["typeAccelerated", "Estimate"]
  OR <- exp(esti)
  ci <- exp(confint(model))["typeAccelerated", ]
  
  res_list[[d]] <- data.frame(
    estimate = esti,
    OR = OR,
    or_ci_low = ci['2.5 %'],
    or_ci_high = ci['97.5 %'],
    disease = d,
    p_value = p_value
  )
  
}

stat_res <- bind_rows(res_list) %>% 
  arrange(p_value)

write_xlsx(
  stat_res,
  paste0(outpath, '3C_data.xlsx')
)

# 转换数据框为长格式
data_long <- disease_pdata %>% 
  select(-1, -2) %>% 
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

pdata <- prevalence_filter_sort %>% 
  mutate(disease=gsub('\\.', ' ', disease)) %>% 
  mutate(disease=factor(disease, unique(disease)))

plt <- ggplot(pdata, aes(disease, prevalence_rate, color=type)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, position = position_dodge(width = .5)) +
  scale_color_manual(values = c(Decelerated='#3773B6', Accelerated='#B8281D')) +
  labs(x='', y='Morbidity') +
  theme_bw() +
  theme(
      legend.key.size = unit(.1, 'inches'),
      text = element_text(size = 12, color = 'black'),
      axis.text.x = element_text(size = 12, color = 'black',angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 12, color = 'black'),
      axis.ticks = element_line(color = 'black'),
      panel.border = element_rect(color = 'black'),
      legend.position = 'top',
      panel.grid = element_blank()
  ) +
  annotate("text", x = 1:length(label_use), y = 0.6, label = label_use, size = 3.5, color = "black")
ggsave(
  filename = paste0(outpath, '3C_disease_ratio_in_agegaps_group_sig.pdf'),
  plt,
  width = 6,
  height = 5
)



