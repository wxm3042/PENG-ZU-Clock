# -------------------------------------
# Date Mon Apr 28 00:31:49 2025
# Script 
# Author WXM
# Purpose 
# Notes 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(survival)
library(survminer)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
# detach(unload='conflicted')
# input -------------------------------------------------------------------

grp_res_path <- 'V20250427/output/Fig8/all_fea_group.txt'
all_fea_dat_path <- 'V20250427/output/Fig8/UKB_data_cor_feature.csv'

death_dat_path <- '../00database/UKB/output/disease250417/death_data_annot.csv'

outpath <- 'V20250427/output/Fig8/'

death_dat <- read.csv(death_dat_path)
group_res <- read.delim(
  file = grp_res_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
)
all_fea <- read.delim(
  file = all_fea_dat_path,
  sep = ',',
  quote = '',
  header = T,
  check.names = F
)  
# process -----------------------------------------------------------------
show_fea <- group_res %>% 
  select(-eid, -ends_with('new')) %>% 
  colnames(.)
results_list <- list()
for(g in show_fea){
  print(g)
  grp_dat <- group_res %>% 
    select(eid, starts_with(g)) %>% 
    rename(Original_Range=2, New_Range=3) %>% 
    pivot_longer(., -1, names_to = 'group', values_to = 'val') %>% 
    mutate(val1=ifelse(val=='in', 1, 0)) %>% 
    select(-val) %>% 
    group_by(eid) %>% 
    slice_max(val1, n=1) %>% 
    mutate(group=ifelse(val1==0, 'out_of_range', group)) %>% 
    unique()
  
  
  use_dat <- grp_dat %>% 
    select(eid, group) %>% 
    merge(., all_fea, by='eid') %>% 
    select(-all_of(g)) %>% 
    merge(., death_dat, by='eid') %>% 
    # mutate(group=factor(group, c("out_of_range", "Original_Range", "New_Range")))
    mutate(group=factor(group, c("Original_Range", "out_of_range", "New_Range")))
  
  
  # 定义目标变量和协变量
  target_var <- "group"
  covariates <- all_fea %>% 
    select(-eid, -all_of(g), -Gender) %>% 
    colnames(.)
  
  # 识别需要反引号的变量名
  needs_backtick <- function(var) {
    grepl("[^a-zA-Z0-9._]", var)  # 匹配非字母、数字、点或下划线的字符
  }
  
  # 为需要反引号的变量名添加反引号
  add_backticks <- function(var) {
    if (needs_backtick(var)) {
      paste0("`", var, "`")
    } else {
      var
    }
  }
  
  # 处理所有变量名
  covariates <- sapply(covariates, add_backticks)
  
  
  # 动态生成公式
  formula <- as.formula(paste("Surv(time, status) ~", target_var, "+", paste(covariates, collapse = "+"), "+strata(Gender)"))
  
  # 拟合 Cox 模型
  cox_model <- coxph(formula, data = use_dat)
  
  # 提取 HR 和 P 值
  HR_dat <- summary(cox_model)$coefficients %>% 
    data.frame() %>% 
    rownames_to_column('id') %>% 
    filter(grepl('group', id)) %>% 
    mutate(id=gsub('^group', '', id)) %>% 
    select(1, 3, 6) %>% 
    rename(HR=2, p_value=3)
  conf_dat <- summary(cox_model)$conf.int %>% 
    data.frame() %>% 
    rownames_to_column('id') %>% 
    filter(grepl('group', id)) %>% 
    mutate(id=gsub('^group', '', id)) %>% 
    select(1, 4, 5) %>% 
    rename(HR_lower=2, HR_upper=3)
  
  # 存储结果
  results <- merge(HR_dat, conf_dat, by='id') %>%
    mutate(fea=g)
  results_list[[g]] <- results
}

results_df <- bind_rows(results_list)
# results_df <- results_df %>%
#   filter(fea != 'HbA1c')

write.table(
  results_df,
  file = paste0(outpath, 'all_fea_different_ref_forestPlot_dat.xls'),
  sep = '\t',
  quote = F,
  row.names = F
)



ord <- results_df %>% 
  group_by(fea) %>% 
  slice_max(n=1, order_by = HR) %>% 
  arrange(HR)
results_df_ord <- results_df %>% 
  mutate(fea=factor(fea, ord$fea))
# results_df_ord <- results_df_ord %>% 
#   filter(fea!='IGF', fea!='HbA1c')

keep_fea <- results_df_ord %>% 
  mutate(sig=ifelse(p_value<0.05, 'y', 'n')) %>% 
  filter(sig=='y') %>% 
  pull(fea) %>% 
  table(.) %>% 
  data.frame() %>% 
  filter(Freq>0)
results_df_ord_filter <- results_df_ord %>% 
  filter(fea %in% keep_fea$.) %>% 
  mutate(sig=ifelse(p_value<0.05, 'y', 'n'))


# 只画新区间的结果 ----------------------------------------------------------------

plot_dat2 <- results_df_ord_filter %>% 
  filter(id =='New_Range') %>% 
  arrange(HR) %>% 
  mutate(fea=factor(fea, fea))

plt <-
  ggplot(plot_dat2,
         aes(
           x = fea,
           y = HR,
           ymin = HR_lower,
           ymax = HR_upper,
           color = id
         )) +
  geom_point(
    data = subset(plot_dat2, sig == 'y'),
    position = position_dodge(width = 0.5),
    size = 3,
    shape = 16
  ) +  # 点表示 HR
  geom_errorbar(
    data = subset(plot_dat2, sig == 'y'),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +  # 误差线表示置信区间
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "black") +  # 参考线
  scale_color_manual(values = c("New_Range" = "black", "out_of_range" = "red")) +  # 自定义颜色
  geom_point(
    data = subset(plot_dat2, sig == 'n'),
    position = position_dodge(width = 0.5),
    color = 'grey',
    size = 3,
    shape = 16
  ) +  # 点表示 HR
  geom_errorbar(
    data = subset(plot_dat2, sig == 'n'),
    color = 'grey',
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +  # 误差线表示置信区间
  scale_y_log10() +
  labs(
    title = "Hazard Ratios (HR) by Feature and Group",
    x = "",
    y = "Hazard Ratio (HR)",
    color = "Group"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.key.size = unit(.1, 'inches'),
    text = element_text(size = 12, color = 'black'),
    axis.text.x = element_text(size = 12, color = 'black'),
    axis.text.y = element_text(size = 12, color = 'black'),
    axis.ticks = element_line(color = 'black'),
    panel.border = element_rect(color = 'black', fill=NA),
    legend.position = 'none',
    panel.grid = element_blank()
  )

ggsave(
  filename = paste0(outpath, 'all_fea_different_ref_forestPlot_new_range.pdf'),
  plt,
  width = 5,
  height = 5
)


