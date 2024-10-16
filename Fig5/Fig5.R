# -------------------------------------
# Date: Wed Jul 31 13:58:54 2024
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(readxl)
library(gamlss)
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
ranger_dat_path <- 'output/4.Reference_Range/merge_data.xlsx'
norml_range_path <- 'output/0.figure/figure7/feature_ref_range2.xlsx'
font_size <- 2.5
ylim_path <- 'output/0.figure/figure7/reset_ylim.xlsx'

outpath <- 'output/0.figure/figure7/all_features/'

dat <- read_excel(dat_path)
type_df <- read_excel(type_path)
range_dat <- read_excel(ranger_dat_path) %>% 
  mutate(gender_use=ifelse(gender==2, 'F', 'M')) %>% 
  select(1:3, gender_use, everything())

locomotion_add <- c(
  'grip_strength',
  'Time_6_meters',
  'Time_6_meters_FS',
  'Standing on Tiptoe',
  'Feet Together for 10 Seconds',
  'Feet Misaligned for 10 Seconds',
  'Heel-to-Toe Alignment for 10 Seconds'
)
ylim_df <- read_excel(ylim_path)

norml_range <- read_excel(norml_range_path)
# process -----------------------------------------------------------------

show_fea <- dat %>% 
  filter(padj_bj<0.05, padj_cs<0.05) %>% 
  select(id, col) %>% 
  merge(., type_df, by='id') %>% 
  mutate(abs_col=abs(col)) %>% 
  arrange(type, desc(abs_col)) %>% 
  group_by(type) %>% 
  # slice_head(n=3) %>% 
  filter(type !='Urine content')

show_type_list <- unique(show_fea$type)


for( c in show_type_list){
  
  if(c == "Action competence"){
    use_fea <- show_fea %>% 
      filter(type == c) %>% 
      pull(id) %>% 
      c(., locomotion_add) %>% 
      unique()
  }else{
    use_fea <- show_fea %>% 
      filter(type == c) %>% 
      pull(id)
  }
  pdf(paste0(outpath, c, '_all_centile_curves.pdf'))
  
  for(s in use_fea){
    print(s)
    
    if(s == 'Time_6_meters_FS'){
      tpdata <- range_dat %>% 
        select(age, all_of(s)) %>% 
        rename(fea=2) %>% 
        filter(!is.na(fea)) %>% 
        filter(fea>0) %>% 
        mutate(fea=6/fea)
      s = 'Max walking speed'
    }else if (s == 'Time_6_meters'){
      tpdata <- range_dat %>% 
        select(age, all_of(s)) %>% 
        rename(fea=2) %>% 
        filter(!is.na(fea)) %>% 
        filter(fea>0) %>% 
        mutate(fea=6/fea)
      s = 'Normal Walking Speed'
    }else if(s %in% c('Feet Together for 10 Seconds',
                      'Feet Misaligned for 10 Seconds',
                      'Heel-to-Toe Alignment for 10 Seconds')){
      
      tpdata <- range_dat %>% 
        select(age, all_of(s)) %>% 
        rename(fea=2) %>% 
        filter(!is.na(fea)) %>% 
        filter(fea>0) %>% 
        filter(age>60) %>% 
        mutate(fea=ifelse(fea>10, 10, fea))
      
    }else{
      tpdata <- range_dat %>% 
        select(age, all_of(s)) %>% 
        rename(fea=2) %>% 
        filter(!is.na(fea)) %>% 
        filter(fea>0)
      
    }
    
    gammodel <- tryCatch({
      gamlss(fea ~ pb(age), sigma.fo = ~ pb(age), nu.fo = ~ pb(age), family = BCT, data = tpdata)
    }, error = function(e) {
      NULL
    })
    
    tmp_normal_range <- norml_range %>% 
      filter(feature==s)
    
    if (!is.null(gammodel)) {
      # 处理正常结果
      #绘制分位数曲线
      # pdf(paste0(outpath, 'all_centile_pt.pdf'), width = 5, height = 5)
      par(cex.main = font_size,     # 标题字体大小
          cex.lab = font_size,      # 坐标轴标签字体大小
          cex.axis = font_size,     # 刻度标记字体大小
          cex.sub = font_size)      # 副标题字体大小
      
      if(s %in% ylim_df$feature){
        centiles(
          gammodel, 
          tpdata$age, 
          cent =  c(1, 2.5, 5, 10, 25, 50, 75, 90, 95, 97.5, 99),
          xlab = '',
          ylab = '',
          points = F,
          legend = F,
          ylim = ylim_df %>% filter(feature==s) %>% select(low, high) %>% as.numeric(),
          main = s,
          lty.centiles = c(2,2,2,1,1,2,1,1,2,2,2)
        )
        
      }else{
        centiles(
          gammodel, 
          tpdata$age, 
          cent =  c(1, 2.5, 5, 10, 25, 50, 75, 90, 95, 97.5, 99),
          xlab = '',
          ylab = '',
          points = F,
          legend = F,
          main = s,
          lty.centiles = c(2,2,2,1,1,2,1,1,2,2,2)
        )
      }
      
      if(nrow(tmp_normal_range)>0){
        
        rect(
          xleft = par("usr")[1],   # 获取当前绘图区域的左边界
          ybottom = tmp_normal_range[['low']],             # 矩形的底部 Y 坐标
          xright = par("usr")[2],  # 获取当前绘图区域的右边界
          ytop = tmp_normal_range[['high']],                # 矩形的顶部 Y 坐标
          col = adjustcolor("#dedac5", alpha.f = 0.5),  # 设置颜色为绿色，透明度 50%
          border = NA              # 边框设置为无
        )
      }
    } else {
      # 处理错误情况（已经在 tryCatch 中处理过错误消息）
      next
    }
    
  }
  dev.off()
}


