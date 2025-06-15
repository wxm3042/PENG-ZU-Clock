# -------------------------------------
# Date: Mon Apr 21 00:54:10 2025
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
#
# Copyright(c) Corporation Name
# -------------------------------------
source("V20250427/src/Fig3/function_bioage.R")
data_new <- read.csv("V20250427/output/Fig3/3E_data.csv")
library(ggplot2)
library(rms)
library(haven)
library(tidyr)
library(segmented)
library(labelled)
library(tidyverse)
library(cowplot)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("select", "dplyr")

## 截点分析

sample4_1 = seg_cpdetect_n("score", data_new%>%filter(type=="Normal"), "男", 1, type = 's')
sample4_2 = seg_cpdetect_n("score", data_new%>%filter(type=="Decelerated"), "男", 1, type = 's')
sample4_3 = seg_cpdetect_n("score", data_new%>%filter(type=="Accelerated"), "男", 1, type = 's')
sample4_4 = seg_cpdetect_n("score", data_new%>%filter(type=="Normal"), "女", 1, type = 's')
sample4_5 = seg_cpdetect_n("score", data_new%>%filter(type=="Decelerated"), "女", 1, type = 's')
sample4_6 = seg_cpdetect_n("score", data_new%>%filter(type=="Accelerated"), "女", 1, type = 's')

#图
outpath <- 'V20250427/output/Fig3/'

p2<-plot_cp("score",data_new%>%filter(type=="Normal"),1,1,ylab="Intrinsic Capacity Score",xlab = "Age",83,60,73,40) +
  scale_fill_manual(values = c(Accelerated='#B8281D', Decelerated='#3773B6', Normal='grey')) +
  labs(x='Chronological age(years)', y='Intrinsic Capacity Score') +
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
  filename = paste0(outpath, 'Fig3E_Normal.pdf'),
  p2,
  width = 3,
  height = 3
)
p1<-plot_cp("score",data_new%>%filter(type=="Decelerated"),1,1,ylab="Intrinsic Capacity Score",xlab = "Age",83,67,73,40) +
  scale_fill_manual(values = c(Accelerated='#B8281D', Decelerated='#3773B6', Normal='grey')) +
  labs(x='Chronological age(years)', y='Intrinsic Capacity Score') +
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
  filename = paste0(outpath, 'Fig3E_Decelerated.pdf'),
  p1,
  width = 3,
  height = 3
)


p3<-plot_cp("score",data_new%>%filter(type=="Accelerated"),1,1,ylab="Intrinsic Capacity Score",xlab = "Age",79,60,67,35) +
  scale_fill_manual(values = c(Accelerated='#B8281D', Decelerated='#3773B6', Normal='grey')) +
  labs(x='Chronological age(years)', y='Intrinsic Capacity Score') +
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
  filename = paste0(outpath, 'Fig3E_Acc.pdf'),
  p3,
  width = 3,
  height = 3
)


