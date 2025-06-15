# -------------------------------------
# Date: Wed Apr  2 16:29:44 2025
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(readxl)
library(survival)
library(survminer)
library(writexl)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

age_gap_path <- 'V20250427/output/Fig5/NHANES/predict_NHANES/age_gaps.xlsx'
death_dat_path <- '../00database/NHANES/All/6_mortality/mortality.xlsx'

outpath <- 'V20250427/output/Fig5/NHANES/predict_NHANES/'

age_gap <- read_excel(age_gap_path)
death_dat <- read_excel(death_dat_path)
# sigma -------------------------------------------------------------------

sigma_val <- sd(age_gap$age_gaps)

dat2 <- age_gap %>% 
  mutate(type=ifelse(age_gaps>sigma_val, 'Accelerated', 'Normal')) %>% 
  mutate(type=ifelse(age_gaps< -sigma_val, 'Decelerated', type))
table(dat2$type)


write.table(
  dat2,
  file = paste0(outpath, 'age_gaps_with_disease.xls'),
  sep = '\t',
  quote = F,
  row.names = F
)


plt <- ggplot(dat2, aes(Age, predict_age, color=type)) +
  geom_point(size=1, alpha=0.3) +
  scale_color_manual(values = c(Accelerated='#B8281D', Decelerated='#3773B6', Normal='grey')) +
  labs(x='Chronological age (years)', y='Biological age (years)', color='') +
  ylim(0,200) +
  theme_bw() +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black'),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(colour = 'black', fill=NA),
    legend.position = 'top',
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
    )

ggsave(
  filename = paste0(outpath, 'S5B_cor_plot_sigma1.pdf'),
  plt,
  width = 4,
  height = 4
)



# survival ----------------------------------------------------------------

grp_dath <- merge(dat2, death_dat, by.x='pt_id', by.y='seqn')
grp_dath <- grp_dath %>% 
  mutate(type=factor(type, c('Accelerated', 'Normal', 'Decelerated'))) %>% 
  mutate(time=time/31)

# 创建生存对象
surv_object <- Surv(time = grp_dath$time, event = grp_dath$status)

# 拟合生存曲线
fit <- survfit(surv_object ~ type, data = grp_dath)
test_result <- survdiff(surv_object ~ type, data = grp_dath)

# 提取 p 值
p_value <- test_result$pvalue %>% 
  round(., 4)
# 绘制生存曲线
surv_plot <- ggsurvplot(
  fit,
  data = grp_dath,
  pval = TRUE,
  conf.int = TRUE,
  risk.table = F,
  palette = c("#B8281D", "grey", "#3773B6"),
)

p_plot <- surv_plot$plot

# 手动添加 p 值到图形上
# 这里的位置参数（x 和 y）需要根据你的具体图形进行调整
p_surv <- p_plot + 
  annotate("text", x = 1000, y = 0.75, label = paste("p =", format(p_value, digits = 3)), 
           hjust = 1, vjust = 0) +
  labs(x='Time (Months)', y='Overall survival probability') +
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
  filename = paste0(outpath, 'S5C_total_surv.pdf'),
  p_surv,
  width = 4,
  height = 3.5
)


