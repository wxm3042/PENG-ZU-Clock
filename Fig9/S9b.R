# -------------------------------------
# Date: Tue May  6 00:00:02 2025
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

raw_res_path <- 'V20250427/output/Fig2/clock/predict_age_in_test_data.xlsx'
new_res_path <- 'V20250427/output/Fig9/all_predict_data.xlsx'

outpath <- 'V20250427/output/Fig9/'

raw_res <- read_excel(raw_res_path) %>% 
  select(-Age) %>% 
  rename(`Comprehensive Clock`=2)
new_res <- read_excel(new_res_path) %>% 
  select(-Age) %>% 
  rename(`Simplified Clock`=2)
# process -----------------------------------------------------------------

pdata <- merge(raw_res, new_res, by='pt_id')

cor_res <- cor(pdata$`Comprehensive Clock`, pdata$`Simplified Clock`) %>% 
  round(., 3)

plt <- ggplot(pdata, aes(`Comprehensive Clock`, `Simplified Clock`)) +
  geom_point() +
  annotate("text", x = 90, y = 25, 
           label = paste0('Pearson:', cor_res), color = "black") +
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
  filename = paste0(outpath, 'S9b.pdf'),
  plt,
  width = 4,
  height = 4
)



