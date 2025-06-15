# -------------------------------------
# Date: Sun May  4 17:42:39 2025
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
library(ggtext)

library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

dat_path <- 'V20250427/output/Fig5/UKB/disease_surv/DSS_HR_df_sig.xls'
rm_disease_path <- 'V20250427/output/Fig5/UKB/disease_surv/disease_rm.xlsx'
annot_path <- 'V20250427/output/Fig5/UKB/disease_surv/annot.xlsx'

outpath <- 'V20250427/output/Fig5/UKB/disease_surv/'

annot_res <- read_excel(annot_path) %>% 
  select(-Class)

dat <- read.delim(
  file = dat_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
) %>% 
  merge(., annot_res, by='name_l1') %>% 
  filter(group=='Acc_type') %>% 
  arrange(Class, HR)
rm_disease <- ''
# process -----------------------------------------------------------------

pdata <- dat %>%
  mutate(
    Class = factor(Class, unique(Class)),
    new_name_l1 = factor(new_name_l1, levels = unique(new_name_l1)) # 保持原始顺序
  )
# 


# 绘图
plt <- ggplot(pdata, aes(x = HR, y = new_name_l1, color = group)) +
  # 基础元素
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0, size = 1) +
  geom_point(size = 3, shape = 17) +
  scale_x_log10() +
  scale_color_manual(values = c('Acc_type'='#B8281D', 'Slowdown'='#329939')) +
  
  # 关键修改1：分面显示分类标题
  facet_grid(
    Class ~ .,
    scales = "free_y",
    space = "free_y",
    switch = "y",
  ) +
  
  # 关键修改2：用geom_text添加亚组名称
  geom_text(
    aes(label = new_name_l1, x = 0.2), # 将名称放在HR=0.2的位置
    hjust = 0,
    size = 4.2,
    color = "black",
    position = position_dodge(width = 0.3)
  ) +
  # 关键修改3：右侧添加ICD10编码
  geom_text(
    aes(label = ICD10, x = 0.7),  # 右侧位置（根据x轴范围调整）
    hjust = 1,
    size = 4.2,
    color = "black",
    position = position_dodge(width = 0.3)
  ) +
  # 主题调整

  theme_bw() +
  theme(
    strip.text.y.left = element_markdown(angle = 0, hjust = 0, size = 12), # 渲染Markdown
    axis.text.y = element_blank(),       # 隐藏原始y轴文本
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    panel.spacing = unit(0.5, "lines"),  # 增加分类区块间距
    legend.position = "top"
  ) +
  labs(x = "Hazard Ratio (HR) with 95% CI", y = "", color = "Subgroup")

ggsave(
  filename = paste0(outpath, 'sig_DSS_risk_part1.pdf'),
  plt,
  width = 22,
  height = 12
)



# 绘图
plt <- ggplot(pdata, aes(x = HR, y = new_name_l1, color = group)) +
  # 基础元素
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0, size = 1) +
  geom_point(size = 3, shape = 17) +
  scale_x_log10() +
  scale_color_manual(values = c('Acc_type'='#B8281D', 'Slowdown'='#329939')) +
  
  # 关键修改1：分面显示分类标题
  facet_grid(
    Class ~ .,
    scales = "free_y",
    space = "free_y",
    switch = "y",
    labeller = labeller(Class = function(x) paste0("<b>", x, "</b>")) # 加粗
  ) +
  labs(x = "Hazard Ratio (HR) with 95% CI", y = "", color = "Subgroup") +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black'),
    axis.text.y = element_blank(),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(colour = 'black', fill=NA),
    legend.position = 'top',
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank()
    )
ggsave(
  filename = paste0(outpath, 'sig_DSS_risk_part2.pdf'),
  plt,
  width = 3,
  height = 12
)
