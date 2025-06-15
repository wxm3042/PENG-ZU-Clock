# -------------------------------------
# Date: Fri May  9 14:14:57 2025
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

age_gap_path <- 'V20250427/output/Fig3/age_gaps_with_disease.xls'
pt_id_path_list <- list(
  BJ='V20250427/output/use_dat/bj/clock_data.xlsx',
  CS='V20250427/output/use_dat/cs/clock_data.xlsx'
)
dat_path <- 'V20250427/output/use_dat/common_fea_for_correlation.xlsx'
type_path <- 'V20250427/output/Fig2/age_correlation/common_fea_for_correlation_pie.xlsx'
ranger_dat_path <- 'V20250427/output/use_dat/merge_data.xlsx'
norml_range_path <- 'V20250427/data/临床常用生化检验项目参考区间/临床常用生化检验项目参考区间.xlsx'
font_size <- 2.5
ylim_path <- 'V20250427/output/Fig7/reset_ylim.xlsx'

outpath <- 'V20250427/output/Fig7/gender/'

dat <- read_excel(dat_path)
type_df <- read_excel(type_path)

ylim_df <- read_excel(ylim_path)

col_list <- c(
  male='#3773B6',
  female='#B8281D'
)

age_gap <- read.delim(
  file = age_gap_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
)

keep_id <- age_gap %>% 
  filter(type != 'Accelerated') %>% 
  pull(pt_id)
# pt ----------------------------------------------------------------------

pt_id_list <- list()
for(g in names(pt_id_path_list)){
  pt_id_list[[g]] <- read_excel(pt_id_path_list[[g]]) %>% 
    pull(pt_id)
  
  }
pt_id_list <- unlist(pt_id_list)

range_dat_raw <- read_excel(ranger_dat_path) %>% 
  mutate(gender_use=ifelse(Gender==0, 'F', 'M')) %>% 
  select(pt_id, Gender, Age, gender_use, everything()) %>% 
  filter(pt_id %in% pt_id_list) %>% 
  filter(pt_id %in% keep_id)


# process -----------------------------------------------------------------


show_fea <- type_df %>% 
  filter(short_name %in% dat$fea) %>% 
  group_by(group)

show_type_list <- unique(show_fea$group)

for(gen in c('male', 'female')){
  
  norml_range <- read_excel(norml_range_path, sheet=gen) %>% 
    mutate(check=gsub('\\(.*$', '', feature))
  
  
  if(gen =='male'){
    range_dat <- range_dat_raw %>% 
      filter(gender_use=='M')
  }else{
    range_dat <- range_dat_raw %>% 
      filter(gender_use=='F')
  }
  
  for( c in show_type_list){
    use_fea <- show_fea %>% 
      filter(group == c) %>% 
      pull(short_name)
    
    pdf(paste0(outpath, gen, '_',c, '_all_centile_curves.pdf'))
    for(s in use_fea){
      print(s)
      tpdata <- range_dat %>%
        select(Age, all_of(s)) %>%
        rename(fea = 2) %>%
        filter(!is.na(fea)) %>%
        filter(fea > 0)
      
      
      gammodel <- tryCatch({
        gamlss(
          fea ~ pb(Age),
          sigma.fo = ~ pb(Age),
          nu.fo = ~ pb(Age),
          family = BCT,
          data = tpdata
        )
      }, error = function(e) {
        NULL
      })
      
      tmp_normal_range <- norml_range %>%
        filter(check == s)
      
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
            tpdata$Age, 
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
            tpdata$Age, 
            cent =  c(1, 2.5, 5, 10, 25, 50, 75, 90, 95, 97.5, 99),
            xlab = '',
            ylab = '',
            points = F,
            legend = F,
            main = s,
            lty.centiles = c(2,2,2,1,1,2,1,1,2,2,2)
          )
        }
        
        if(nrow(tmp_normal_range)>1){
          for (i in 1:nrow(tmp_normal_range)) {
            age_low <- tmp_normal_range[i, "age_low"]
            age_high <- tmp_normal_range[i, "age_high"]
            low_val <- tmp_normal_range[i, "low"]
            high_val <- tmp_normal_range[i, "high"]
            
            rect(
              xleft = max(age_low, min(tpdata$Age)),
              xright = min(age_high, max(tpdata$Age)),
              ybottom = low_val,
              ytop = high_val,
              col = adjustcolor(col_list[[gen]], alpha.f = 0.5),
              border = NA
            )
            }
        }else if(nrow(tmp_normal_range)==1){
          rect(
            xleft = par("usr")[1],   # 获取当前绘图区域的左边界
            ybottom = tmp_normal_range[['low']],             # 矩形的底部 Y 坐标
            xright = par("usr")[2],  # 获取当前绘图区域的右边界
            ytop = tmp_normal_range[['high']],                # 矩形的顶部 Y 坐标
            col = adjustcolor(col_list[[gen]], alpha.f = 0.5),  # 设置颜色为绿色，透明度 50%
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
  
}



