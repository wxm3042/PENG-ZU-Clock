# -------------------------------------
# Date: Sun Apr 20 14:35:42 2025
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
library(viridis)
# library(ggstatsplot)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

disease_numb_path <- 'output/3.clock/changsha_clock/elastic_net_scale/merge_bj_cs/disease_numb.xls'
dat_path <- 'V20250427/output/Fig3/age_gaps_with_disease.xls'

outpath <- 'V20250427/output/Fig3/'

dat <- read.delim(
  file = dat_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
)
disease_numb <- read.delim(
  file = disease_numb_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
) %>% select(pt_id, Freq, grp) %>% 
  rename(disease_num=2, disease_num_grp=3) %>% 
  mutate(pt_id=gsub('patient', 'pt', pt_id))


# process -----------------------------------------------------------------

pdata <- merge(dat,disease_numb, by='pt_id')

p_value <- kruskal.test(age_gaps ~ disease_num_grp, data = pdata)$p.value %>% 
  round(., digits = 4)

plt <- ggplot(pdata, aes(disease_num_grp, age_gaps, fill=disease_num_grp)) +
  geom_hline(yintercept = 0, color = "darkgrey", linewidth = 1) +
  geom_violin() +
  geom_boxplot(width=0.3, color="white", outlier.shape = NA, linewidth=.5) +
  annotate('text', x=5, y=-20, label='p<0.001') +
  ylim(-25,25) +
  scale_fill_viridis(discrete = TRUE) +
  labs(x='Number of diseases', y='ClinAge') +
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
  filename = paste0(outpath, '3B_disease_numb_violine.pdf'),
  plt,
  width = 3.5,
  height = 3
)







