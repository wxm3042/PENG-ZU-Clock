# -------------------------------------
# Date: Fri Jul  5 12:35:34 2024
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(readxl)
library(patchwork)
library(writexl)
library(ggrepel)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

dat_path <- 'output/0.figure/figure2/merge_correlation.xlsx'
type_path <- 'output/2.deg/correlation_fea_common_type.xlsx'
exp_dat_path_list <- list(
  bj='output/2.deg/beijing/feature_for_correlation.xlsx',
  cs='output/2.deg/changsha/feature_for_correlation.xlsx'
)

outpath <- 'output/0.figure/figure2/'

type_df <- read_excel(type_path)
dat <- read_excel(dat_path)


# process -----------------------------------------------------------------

show_fea <- dat %>% 
  filter(sig=='y') %>% 
  select(id, col) %>% 
  merge(., type_df, by='id') %>% 
  mutate(abs_col=abs(col)) %>% 
  arrange(type, desc(abs_col)) %>% 
  group_by(type) %>% 
  slice_head(n=3) %>% 
  filter(type !='Urine content')

show_type_list <- unique(show_fea$type)

raw_dat_list <- list()
for(g in names(exp_dat_path_list)){
  tdata <- read_excel(exp_dat_path_list[[g]]) %>% 
    select(age, all_of(show_fea$id)) %>% 
    mutate(sour=g)
  raw_dat_list[[g]] <- tdata
}

raw_dat_df <- bind_rows(raw_dat_list) %>% 
  mutate(CHE=CHE/1000)

plt_list <- list()
for (t in show_type_list) {
  use_fea <- show_fea %>%
    filter(type == t) %>%
    pull(id)
  
  for (fe in use_fea) {
    
      pdata <- raw_dat_df %>%
        select(age, all_of(fe), sour) %>%
        rename(ff = 2) %>%
        mutate(title = fe) %>%
        filter(!is.na(ff))

    
    plt <- ggplot(pdata, aes(age, ff, color = sour)) +
      # geom_point() +
      geom_smooth() +
      # facet_grid( ~ title) +
      labs(x = '', y = '', title=unique(pdata$title)) +
      theme_bw() +
      theme(
        legend.key.size = unit(.1, 'inches'),
        text = element_text(size = 24, color = 'black'),
        axis.text.x = element_text(size = 24, color = 'black'),
        axis.text.y = element_text(size = 24, color = 'black'),
        axis.ticks = element_line(color = 'black'),
        panel.border = element_rect(color = 'black'),
        legend.position = 'none',
        panel.grid = element_blank(),
        # plot.margin = unit(c(0, 0, 0, 0.6), "cm"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(size = 24, hjust = 0.5)
      )
    plt_list[[paste0(t, fe)]] <- plt
  }
  
}
out_plt <- wrap_plots(plt_list, ncol = 6)
ggsave(
  filename = paste0(outpath,  'all_fea_dotplot.pdf'),
  out_plt,
  width = 22,
  height = 17.5
)

# class order -------------------------------------------------------------

sort_dat <- dat %>% 
  select(id, col, sig) %>% 
  merge(., type_df, by='id') %>% 
  mutate(abs_col=abs(col)) %>% 
  arrange(type, desc(abs_col)) %>% 
  group_by(type)
  
sig_dat_cor <- sort_dat %>% 
  filter(sig=='y') %>% 
  select(type, abs_col) %>% 
  group_by(type) %>% 
  summarise(mean_cor=mean(abs_col)) %>% 
  arrange(desc(mean_cor)) %>% 
  mutate(rank_cor=min_rank(desc(mean_cor)))

sig_ratio <- sort_dat %>%
  select(type, sig) %>% 
  mutate(total_count = n()) %>%
  summarise(element_count = sum(sig == 'y'),
            proportion = element_count / total_count) %>%
  unique() %>% ungroup() %>%
  arrange(desc(proportion)) %>% 
  filter(element_count!=0) %>% 
  mutate(rank_prop=min_rank(desc(proportion)))

tmp_pdata <- merge(sig_ratio, sig_dat_cor, by='type') %>% 
  mutate(mean_rank=rowMeans(select(., starts_with('rank')))) %>% 
  arrange(mean_rank)

plt <- ggplot(tmp_pdata, aes(proportion, mean_cor, label=type, size=1/mean_rank, color=1/mean_rank)) +
  geom_point() +
  geom_text_repel(max.overlaps = Inf, size = 4) +
  # geom_text_repel(max.overlaps = Inf, nudge_x=-0.3, direction="y", hjust=1, size=2,box.padding=0.1) +
  scale_size_continuous(range = c(1, 10), limits = c(0, 1)) +
  # scale_color_continuous(limits = c(0, 1)) +
  coord_cartesian(xlim = c(0, max(tmp_pdata$proportion) + .1), ylim = c(0, max(tmp_pdata$mean_cor) + .1)) +
  labs(x='', y='') +
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
  filename = paste0(outpath, 'type_rank_dotplot.pdf'),
  plt,
  width = 4,
  height = 4
)

# v2 ----------------------------------------------------------------------


for(g in names(exp_dat_path_list)){
  tdata <- read_excel(exp_dat_path_list[[g]])

  raw_dat_df <- bind_rows(raw_dat_list)
  
  for(t in show_type_list){
    use_fea <- show_fea %>% 
      filter(type==t) %>% 
      pull(id)
    
    plt_list <- list()
    for(fe in use_fea){
      pdata <- tdata %>% 
        select(age, all_of(fe)) %>% 
        rename(ff=2) %>% 
        mutate(title=fe) %>% 
        filter(!is.na(ff))
      
      plt <- ggplot(pdata, aes(age, ff)) +
        geom_point() +
        geom_smooth() +
        facet_grid(~title) +
        labs(x='', y='') +
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
      plt_list[[fe]] <- plt
    }
    
    out_plt <- wrap_plots(plt_list, ncol=3)
    ggsave(
      filename = paste0(outpath,  g, '/',t,'_fea_dotplot.pdf'),
      out_plt,
      width = 7.5,
      height = 2.5
    )
  }
}








