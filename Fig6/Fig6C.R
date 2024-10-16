# -------------------------------------
# Date: Sun Oct  6 19:40:02 2024
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(openxlsx)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

dat_path <- 'output/3.clock/changsha_clock/elastic_net_scale/simplify_clock/merge//age_gaps.xlsx'
pt_disease_path <- 'output/3.clock/changsha_clock/elastic_net_scale/merge_bj_cs/all_disease_pt.xls'
pt_disease_mtx_path <- 'data/bj_cs_disease_mtx.xlsx'
disease_numb_path <- 'output/3.clock/changsha_clock/elastic_net_scale/simplify_clock/merge/disease_numb.xls'

outpath <- 'output/0.figure/figure6/merge/'

pt_disease <- read.delim(
  file = pt_disease_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
) %>% pull(1)

dat <- read.xlsx(dat_path) %>% 
  mutate(disease=ifelse(pt_id %in% pt_disease, 'y', 'n'))
disease_numb <- read.delim(
  file = disease_numb_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
) %>% select(1,5) %>% 
  mutate(Freq=as.numeric(gsub('n','',Freq)))

pt_disease_mtx <- read.xlsx(pt_disease_mtx_path)
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

plt <- ggplot(dat2, aes(age, predict_age, color=type)) +
  geom_point(size=.5) +
  scale_color_manual(values = c(Accelerated='#b75388', Decelerated='#516db2', Normal='grey')) +
  labs(x='Chronological age(years)', y='Predicted age(years)') +
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
  filename = paste0(outpath, 'cor_plot_sigma1.pdf'),
  plt,
  width = 4,
  height = 4
)



