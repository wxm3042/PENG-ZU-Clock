# -------------------------------------
# Date: Thu Mar 27 16:32:44 2025
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

common_fea_path <- 'V20250427/output/Fig2/age_correlation/common_fea_for_correlation_pie.xlsx'

outpath <- 'V20250427/output/Fig2/age_correlation/'

common_fea <- read_excel(common_fea_path)
# process -----------------------------------------------------------------

type_ord <- common_fea %>% 
  pull(group) %>% 
  table() %>% 
  data.frame() %>% 
  rename(type=1) %>% 
  arrange(Freq)

pdata <- type_ord %>%
  mutate(percentage = round((Freq / sum(Freq) * 100), 1)) %>% 
  mutate(type=factor(type, type_ord$type))


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
  filename = paste0(outpath, '2A_pie_fea_type.pdf'),
  plt,
  width = 4,
  height = 4
)


