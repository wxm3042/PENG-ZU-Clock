# -------------------------------------
# Date: Mon Jul 22 02:11:04 2024
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

dat_path <- 'output/3.clock/changsha_clock/elastic_net_scale/merge_bj_cs/all_agegaps_related_features.xlsx'

outpath <- 'output/0.figure/figure2/'

dat <- read_excel(dat_path)


# process -----------------------------------------------------------------

pdata <- dat %>% 
  filter(padj<0.05) %>% 
  arrange(coefficients) %>% 
  mutate(id=factor(id, id)) %>% 
  mutate(change=ifelse(coefficients>0, 'pos', 'neg'))

plt <- ggplot(pdata, aes(id, coefficients, fill = coefficients)) +
  geom_bar(stat='identity', color='black') +
  scale_fill_gradient2(low='darkblue', mid='white', high = 'darkred') +
  labs(x='', y='') +
  theme_bw() +
  theme(
      legend.key.size = unit(.1, 'inches'),
      text = element_text(size = 12, color = 'black'),
      axis.text.x = element_text(size = 12, color = 'black',angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 12, color = 'black'),
      axis.ticks = element_line(color = 'black'),
      panel.border = element_rect(color = 'black'),
      legend.position = 'top',
      panel.grid = element_blank()
  )

ggsave(
  filename = paste0(outpath, 'agegap_rel_fea.pdf'),
  plt,
  width = 10,
  height = 5
)

pdata <- pdata %>% 
  filter(!(id %in% c('WHR','Cystatin c')))

plt <- ggdotchart(
  pdata,
  x = "id",
  y = "coefficients",
  color = "change",
  # Color by groups
  palette = c("darkblue", "darkred"),
  # Custom color palette
  sorting = "descending",
  # Sort value in descending order
  rotate = TRUE,
  # Rotate vertically
  dot.size = 2,
  # Large dot size
  y.text.col = TRUE,
  # Color y text by groups
  ggtheme = theme_pubr()                        # ggplot2 theme
) +
  scale_y_continuous(
    limits = c(-3, 5),            # 设置横轴范围
    breaks = seq(-2.5, 5, by = 2.5) # 设置刻度间隔
  ) +
  theme_cleveland()                                      # Add dashed grids

ggsave(
  filename = paste0(outpath, 'agegap_rel_fea_dotplot_without_WHR_CystatinC.pdf'),
  plt,
  width = 5,
  height = 10
)


# sort by padj ------------------------------------------------------------

pdata <- dat %>% 
  filter(padj<0.05) %>% 
  mutate(logp=-log10(padj)) %>% 
  mutate(signed_logp=ifelse(coefficients>0, logp, -logp)) %>% 
  arrange(signed_logp) %>% 
  mutate(id=factor(id, id)) %>% 
  mutate(signed_logp=ifelse(signed_logp>=100, 100, signed_logp)) %>% 
  mutate(signed_logp=ifelse(signed_logp<= -100, -100, signed_logp)) %>% 
  mutate(change=ifelse(coefficients>0, 'pos', 'neg'))

# bar
plt <- ggplot(pdata, aes(id, signed_logp, fill = signed_logp)) +
  geom_bar(stat='identity', color='black') +
  scale_fill_gradient2(low='darkblue', mid='white', high = 'darkred') +
  labs(x='', y='') +
  theme_bw() +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black',angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(color = 'black'),
    legend.position = 'top',
    panel.grid = element_blank()
  )

# dot
plt <- ggdotchart(
  pdata,
  x = "id",
  y = "signed_logp",
  color = "change",
  # Color by groups
  palette = c("darkblue", "darkred"),
  # Custom color palette
  sorting = "descending",
  # Sort value in descending order
  rotate = TRUE,
  # Rotate vertically
  dot.size = 2,
  # Large dot size
  y.text.col = TRUE,
  # Color y text by groups
  ggtheme = theme_pubr()                      
) +
  theme_cleveland()                                     


ggsave(
  filename = paste0(outpath, 'agegap_rel_fea_dotplot_sort_by_padj.pdf'),
  plt,
  width = 6,
  height = 10
)




