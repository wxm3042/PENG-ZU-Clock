# -------------------------------------
# Date: Sun Apr 20 11:34:41 2025
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


dat_path <- 'V20250427/output/Fig2/clock/age_gaps.xlsx'

outpath <- 'V20250427/output/Fig3/'


dat <- read_excel(dat_path)

# sigma 1 plot ------------------------------------------------------------

sigma_val <- sd(dat$age_gaps)

dat2 <- dat %>% 
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
  geom_point(size=.5) +
  scale_color_manual(values = c(Accelerated='#B8281D', Decelerated='#3773B6', Normal='grey')) +
  labs(x='Chronological age(years)', y='Predicted age(years)') +
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
  filename = paste0(outpath, '3A_cor_plot_sigma1.pdf'),
  plt,
  width = 3,
  height = 3
)










