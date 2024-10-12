# -------------------------------------
# Date: Tue Sep 10 00:39:32 2024
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
  # slice_head(n=3) %>% 
  filter(type !='Urine content')

show_type_list <- unique(show_fea$type)

raw_dat_list <- list()
for(g in names(exp_dat_path_list)){
  tdata <- read_excel(exp_dat_path_list[[g]]) %>% 
    select(age, gender, all_of(show_fea$id)) %>% 
    mutate(sour=g)
  raw_dat_list[[g]] <- tdata
}

raw_dat_df <- bind_rows(raw_dat_list) %>% 
  mutate(CHE=CHE/1000)
s
plt_list <- list()
for (t in show_type_list) {
  use_fea <- show_fea %>%
    filter(type == t) %>%
    pull(id)
  
  for (fe in use_fea) {
    
    if(fe=='Time_6_meters_FS'){
      pdata <- raw_dat_df %>%
        select(age, all_of(fe), gender) %>%
        mutate(gender=ifelse(gender==1, 'male', 'female')) %>% 
        rename(ff = 2) %>%
        mutate(title = 'Max walking speed') %>%
        filter(!is.na(ff)) %>% 
        mutate(ff=6/ff)
    }else if(fe=='Time_6_meters'){
      pdata <- raw_dat_df %>%
        select(age, all_of(fe), gender) %>%
        mutate(gender=ifelse(gender==1, 'male', 'female')) %>% 
        rename(ff = 2) %>%
        mutate(title = 'Normal Walking Speed') %>%
        filter(!is.na(ff)) %>% 
        mutate(ff=6/ff)
    }else{
      pdata <- raw_dat_df %>%
        select(age, all_of(fe), gender) %>%
        mutate(gender=ifelse(gender==1, 'male', 'female')) %>% 
        rename(ff = 2) %>%
        mutate(title = fe) %>%
        filter(!is.na(ff))
    }
    
    
    plt <- ggplot(pdata, aes(age, ff, color = gender)) +
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
out_plt <- wrap_plots(plt_list, ncol = 8)
ggsave(
  filename = paste0(outpath,  'all_fea_dotplot_34features_gender.pdf'),
  out_plt,
  width = 28,
  height = 17.5
)


