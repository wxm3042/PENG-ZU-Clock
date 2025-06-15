# -------------------------------------
# Date: Wed May 14 15:05:10 2025
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

dat_path <- 'V20250427/output/Fig5/NHANES/predict_NHANES/all_disease_survival_risk_data.xls'

outpath <- 'V20250427/output/Fig5/NHANES/predict_NHANES/'

dat <- read.delim(
  file = dat_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
)
# process -----------------------------------------------------------------

pdata <- dat %>% 
  filter(grp == 'Accelerated') %>% 
  arrange(HR) %>% 
  mutate(disease=factor(disease, disease))

plt <- ggplot(pdata, aes(x = disease, y = HR, ymin = low_ci, ymax = high_ci)) +
  # 添加点表示HR
  geom_point(size = 3, shape = 18, color = "#B8281D") +
  # 添加误差线表示置信区间
  geom_errorbar(width = 0, color = "#B8281D") +
  # 添加参考线（HR = 1）
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey") +
  # 添加标签显示p值
  # 坐标轴翻转（使变量在y轴）
  scale_y_log10() +
  coord_flip() +
  # 添加标题和标签
  labs(x='', y='Hazard Ratio (HR) with 95% CI', title = "Cause-specific mortality") +
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
  filename = paste0(outpath, 'S4C_Cause-specific mortality.pdf'),
  plt,
  width = 6,
  height = 3
)






