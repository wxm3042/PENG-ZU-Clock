# -------------------------------------
# Date: Thu Jul  4 23:48:10 2024
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
library(writexl)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

common_fea_path <- 'output/2.deg/correlation_fea_common_type.xlsx'

outpath <- 'output/0.figure/figure2/'

common_fea <- read_excel(common_fea_path)
# process -----------------------------------------------------------------

type_ord <- common_fea %>% 
  filter(type!=1) %>% 
  mutate(type=ifelse(!is.na(merge), merge, type)) %>% 
  pull(type) %>% 
  table() %>% 
  data.frame() %>% 
  rename(type=1) %>% 
  arrange(Freq)

pdata <- type_ord %>%
  mutate(percentage = round((Freq / sum(Freq) * 100), 1)) %>% 
  mutate(type=factor(type, type_ord$type))

# 创建饼图
plt <- ggplot(pdata, aes(x = "", y = percentage, fill = type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() + # 清除背景和坐标轴
  labs(fill = "Category") 
ggsave(
  filename = paste0(outpath, 'fea_type_pie.pdf'),
  plt,
  width = 4,
  height = 4
)


# ggpubr ------------------------------------------------------------------


labs <- paste0(pdata$type, " (", pdata$percentage, "%)")
plt <- ggpie(
  pdata,
  "percentage",
  label = labs,
  fill = "type",
  color = "white"
) +
  theme(
      legend.key.size = unit(.1, 'inches'),
      text = element_text(size = 12, color = 'black'),
      axis.text.x = element_text(size = 12, color = 'black'),
      axis.text.y = element_text(size = 12, color = 'black'),
      axis.ticks = element_line(color = 'black'),
  )
ggsave(
  filename = paste0(outpath, 'fea_type_pie_label.pdf'),
  plt,
  width = 4,
  height = 4
)
