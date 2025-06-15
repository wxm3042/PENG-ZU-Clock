# -------------------------------------
# Date: Wed Apr  2 17:12:02 2025
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

dat_path <- 'V20250327/output/Fig5/UKB/predict_UKB/DSS_HR_df_sig_show.xlsx'

outpath <- 'V20250327/output/Fig5/UKB/predict_UKB/'

show_disease <- read_excel(dat_path, sheet='annot') %>% 
  filter(keep=='y') %>% 
  select(-keep, -Class)
dat <- read_excel(dat_path) %>% 
  merge(., show_disease, by.x='Disease', by.y='name_l1') %>% 
  filter(group=='Acc_type') %>% 
  arrange(Class)
rm_disease <- ''
# process -----------------------------------------------------------------


# show accelerate slowdown ------------------------------------------------


pdata <- dat %>% 
  mutate(Disease=factor(Disease, Disease)) %>% 
  mutate(new_name_l1=factor(new_name_l1, new_name_l1))


plt <- ggplot(pdata, aes(x = HR, y = new_name_l1, color = group)) +
  # 添加垂直线表示 HR = 1
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  # 添加置信区间（直线）
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0, size = 1, position = position_dodge(width = 0.3)) +
  # 添加 HR 点
  geom_point(size = 3, position = position_dodge(width = 0.3), shape = 17) +
  scale_shape_manual(values = c("y" = 16, "n" = 21)) +
  # facet_wrap(~ Class, ncol=1) +
  scale_x_log10() +
  scale_color_manual(values = c('Acc_type'='#EF7B20', 'Slowdown'='#329939')) +
  # 设置横轴和纵轴标签
  labs(
    x = "Hazard Ratio (HR) with 95% CI",
    y = "Group",
    color = "Subgroup"
  ) +
  # 设置主题
  labs(x='Hazard Ratio (HR) with 95% CI', y='') +
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
  filename = paste0(outpath, 'sig_DSS_risk.pdf'),
  plt,
  width = 8,
  height = 8
)


plt <- ggplot(pdata, aes(x = HR, y = new_name_l1, color = group)) +
  # 添加垂直线表示 HR = 1
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  # 添加置信区间（直线）
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0, size = 1, position = position_dodge(width = 0.3)) +
  # 添加 HR 点
  geom_point(size = 3, position = position_dodge(width = 0.3), shape = 17) +
  scale_shape_manual(values = c("y" = 16, "n" = 21)) +
  # 使用 facet_wrap 实现三线图布局
  facet_wrap(~ new_name_l1 + ICD10, ncol = 1, scales = "free_y", strip.position = "left") +
  scale_x_log10() +
  scale_color_manual(values = c('Acc_type'='#EF7B20', 'Slowdown'='#329939')) +
  # 设置横轴和纵轴标签
  labs(
    x = "Hazard Ratio (HR) with 95% CI",
    y = "",
    color = "Subgroup"
  ) +
  # 设置主题
  theme_bw() +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black'),
    axis.text.y = element_blank(),  # 隐藏 y 轴文本，因为已经在 facet 中显示
    axis.ticks = element_line(color = 'black'),
    axis.ticks.y = element_blank(),
    panel.border = element_rect(color = 'black'),
    legend.position = 'top',
    panel.grid = element_blank(),
    strip.text.y.left = element_text(angle = 0, size = 12, color = 'black')  # 调整 facet 标签的样式
  )

ggsave(
  filename = paste0(outpath, 'sig_DSS_risk.pdf'),
  plt,
  width = 8,
  height = 8
)

