# -------------------------------------
# Date: Wed Sep 18 19:20:19 2024
# Script: 
# Author: WXM
# Purpose: 
# Notes: 
#
# Copyright(c) Corporation Name
# -------------------------------------
library(tidyverse)
library(readxl)
library(table1) 
library(boot)
library(flextable)
library(writexl)
library(officer)
library(conflicted)
set.seed(12345)
options(stringsAsFactors = F)

conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("label", "table1")

# detach(unload='conflicted')


# function ----------------------------------------------------------------

# a function to provide p-values that indicate differences among groups
pvalue <- function(x, ...) {
  x <- x[-c(length(x), length(x)-1)]  # Remove "overall" group
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform an ANOVA
    p <- summary(aov(y ~ g))[[1]][["Pr(>F)"]][1]
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value
  if (p < 0.001) {
    p = '< 0.001***'
  } else if (p < 0.01) {
    p = sprintf("%.3f**", p)
  } else if (p < 0.05) {
    p = sprintf("%.3f*", p)
  } else {
    p = sprintf("%.3f", p)
  }
  
  return(p)
}

# 自定义连续变量的渲染函数，仅展示均值（不展示缺失值）
render_continuous <- function(x) {
  stats <- stats.default(x)
  # 将 MEAN 和 SD 转换为数值并检查 NA
  mean_val <- as.numeric(stats$MEAN)
  sd_val <- as.numeric(stats$SD)
  
  # print(sprintf("%.2f (%.2f)", mean_val, sd_val))
  
  # 检查如果存在 NA 或非数值数据，返回 NA
  if (is.na(mean_val) || is.na(sd_val)) {
    return("NA")
  } else {
    return(sprintf("%.2f (%.2f)", mean_val, sd_val))
  }
}
render_categorical <- function(x) {
  render.categorical.default(x, na.is.category = F)
}

# input -------------------------------------------------------------------

show_fea_path <- 'output/0.figure/figure1/show_fea_omic.xlsx'
common_id_path <- 'output/3.clock/changsha_clock/elastic_net_scale/test_beijing/slowdown_vs_accelerate/common_id.xlsx'
pt_id_path_list <- list(
  bj='output/3.clock/changsha_clock/elastic_net_scale/test_beijing/age_gaps.xlsx',
  cs_train='output/3.clock/changsha_clock/elastic_net_scale/train_changsha_common_with_BJ/train_pt_id.txt',
  cs_test_disease = 'output/3.clock/changsha_clock/elastic_net_scale/test_changsha_disease/age_gaps.xlsx',
  cs_test_normal = 'output/3.clock/changsha_clock/elastic_net_scale/test_changsha_normal/age_gaps.xlsx'
)

data_path_list <- list(
  bj='data/beijing/beijing_add_eGFR_corrected.xlsx',
  cs='data/changsha/changsha_add_eGFR_corrected.xlsx'
)

fea_info_path_list <- list(
  bj='data/beijing/副本北京医院2596例final原表_full-矫正.xlsx',
  cs='data/changsha/changsha_colnamedf_for_fig1.xlsx'
)

disease_mtx_path <- 'data/bj_cs_disease_mtx.xlsx'

activity_path <- 'data/ADL与IADL数据.xlsx'

aging_group_path <- 'output/3.clock/changsha_clock/elastic_net_scale/test_beijing/age_gaps_with_disease.xls'

outpath <- 'output/0.figure/figure1/'

common_id <- read_excel(common_id_path)

show_fea <- read_excel(show_fea_path) %>% 
  filter(keep=='y')

activity_df <- read_excel(activity_path) %>% 
  select(1, 6,7) %>% 
  rename(ADL=2) %>% 
  filter(!is.na(ADL) & !is.na(IADL)) %>% 
  mutate(IADL=gsub('好','Good',IADL)) %>% 
  mutate(IADL=gsub('能力减退','Reduced capacity',IADL)) %>% 
  mutate(IADL=gsub('能力缺失','Lack of capacity',IADL)) %>% 
  mutate(IADL=gsub('尚可','Acceptable',IADL)) %>% 
  rename(q_id=1) %>% 
  filter(!is.na(q_id))

disease_mtx <- read_excel(disease_mtx_path) %>% 
  rename(pt_id=1) %>% 
  select(pt_id, Hypertension, Diabetes)

sub_fea_annot <- read_excel(fea_info_path_list[['bj']], sheet='q_fea') %>% 
  fill(name, group) %>% 
  filter(group %in% show_fea$feature)

aging_group <- read.delim(
  file = aging_group_path,
  sep = '\t',
  quote = '',
  header = T,
  check.names = F
) %>% 
  filter(type != 'Normal') %>% 
  select(pt_id, type) %>% 
  mutate(type=gsub('Slowdown','Decelerated',type))
# id_use ------------------------------------------------------------------

cs_normal1 <- read.delim(
  file = pt_id_path_list[['cs_train']],
  sep = '\t',
  quote = '',
  header = F,
  check.names = F
) %>% rename(pt_id=1) %>% 
  mutate(pt_sour='CS', pt_type='Healthy')
cs_normal2 <- read_excel(pt_id_path_list[['cs_test_normal']]) %>% 
  mutate(pt_sour='CS', pt_type='Healthy') %>% 
  select(pt_id, pt_sour, pt_type)
cs_disease <- read_excel(pt_id_path_list[['cs_test_disease']]) %>% 
  mutate(pt_sour='CS', pt_type='Unhealthy') %>% 
  select(pt_id, pt_sour, pt_type)
bj_all <- read_excel(pt_id_path_list[['bj']]) %>% 
  mutate(pt_sour='BJ', pt_type='All') %>% 
  select(pt_id, pt_sour, pt_type)

all_pt_df <- bind_rows(list(cs_normal1, cs_normal2, cs_disease, bj_all)) %>% 
  filter(pt_id %in% common_id$patient_id)


# process -----------------------------------------------------------------

all_dat_list <- list()

for(g in names(data_path_list)){
  
  if(g == 'bj'){
    
    dat_df <- read_excel(data_path_list[[g]]) %>% 
      rename(pt_id=1) %>% 
      merge(., activity_df, by='q_id', all.x=T) %>% 
      merge(., disease_mtx, by='pt_id', all.x=T)
    
    annot1 <- read_excel(fea_info_path_list[[g]], sheet=2) %>% 
      select(id, new_name) %>% 
      rename(feature=2)
    annot2 <- read_excel(fea_info_path_list[[g]], sheet=4) %>% 
      select(id, group) %>% 
      rename(feature=2)
    annot3 <- data.frame(
      id=c('ADL', 'IADL', 'Hypertension', 'Diabetes'),
      feature=c('ADL', 'IADL', 'Hypertension', 'Diabetes')
    )
    annot <- rbind(annot1, annot2) %>% 
      rbind(., annot3) %>% 
      filter(feature %in% show_fea$feature) %>% 
      unique() %>% 
      mutate(feature=factor(feature, show_fea$feature)) %>% 
      arrange(feature) %>% 
      mutate(id=ifelse(feature=='Age', 'age', id)) %>% 
      mutate(id=ifelse(feature=='Gender', 'gender', id))
    
    bj_datdf <- dat_df %>% 
      select(pt_id, all_of(annot$id))
    colnames(bj_datdf)[-1] <- annot$feature %>% as.character()
    all_dat_list[[g]] <- bj_datdf
    
  }else{
    
    dat_df <- read_excel(data_path_list[[g]]) %>% 
      rename(pt_id=1, q_id=36) %>% 
      merge(., activity_df, by='q_id', all.x=T) %>% 
      merge(., disease_mtx, by='pt_id', all.x=T)
    
    annot1 <- read_excel(fea_info_path_list[[g]], sheet=1) %>% 
      select(raw, new_name) %>% 
      rename(feature=2)
    annot2 <- read_excel(fea_info_path_list[[g]], sheet=2) %>% 
      select(id, group) %>% 
      rename(raw=1, feature=2)
    annot3 <- data.frame(
      raw=c('ADL', 'IADL', 'Hypertension', 'Diabetes'),
      feature=c('ADL', 'IADL', 'Hypertension', 'Diabetes')
    )
    annot <- rbind(annot1, annot2) %>% 
      rbind(., annot3) %>% 
      filter(feature %in% show_fea$feature) %>% 
      unique() %>% 
      mutate(feature=factor(feature, show_fea$feature)) %>% 
      arrange(feature)
    
    cs_datdf <- dat_df %>% 
      select(pt_id, all_of(annot$raw))
    colnames(cs_datdf)[-1] <- annot$feature %>% as.character()
    cs_datdf$Gender <- ifelse(cs_datdf$Gender=='男', 1, 2)
    
    all_dat_list[[g]] <- cs_datdf
  }
  
}


all_dat <- bind_rows(all_dat_list) %>% 
  rename(Sex=2) %>% 
  merge(., all_pt_df, by='pt_id')

all_dat_feautre <- show_fea %>% 
  mutate(feature=ifelse(feature=='Gender', 'Sex', feature))

# write.table(
#   all_dat,
#   file = paste0(outpath, 'table_dat.xlsx'),
#   sep = '\t',
#   quote = F,
#   row.names = F
# )

all_dat_use <- all_dat %>% 
  mutate(Education=ifelse(Education %in% c(1:3),1, Education)) %>% 
  mutate(Education=ifelse(Education >5 ,5, Education)) %>% 
  # mutate(`Average Daily Interactions`=ifelse(`Average Daily Interactions` == 2, 1, `Average Daily Interactions`)) %>% 
  mutate(`Location of Current Residence`=ifelse(`Location of Current Residence` ==2, 1, `Location of Current Residence`)) %>% 
  mutate(`Location of Current Residence`=ifelse(`Location of Current Residence` %in% c(3,4), 5, `Location of Current Residence`)) %>% 
  mutate(`Location of Current Residence`=ifelse(`Location of Current Residence` %in% c(0,6,8), NA, `Location of Current Residence`)) %>%  
  mutate(Sex=factor(Sex, levels=c(1,2), labels=c('Male', 'Female'))) %>% 
  merge(., aging_group, by='pt_id')
  # mutate(pt_type=factor(pt_type, c('Healthy','Unhealthy','All')))
# mutate(IADL=factor(IADL, c('Good', 'Acceptable', 'Reduced capacity', 'Lack of capacity'))) %>% 
# mutate(ADL=as.numeric(ADL))


for(tg in unique(sub_fea_annot$group)){
  
  use_sug_b <- sub_fea_annot %>% 
    filter(group==tg) %>% 
    pull(choose) %>% 
    intersect(., all_dat_use[[tg]] %>% unique())
  
  use_short_id <- sub_fea_annot %>% 
    filter(group==tg) %>% 
    filter(choose %in% use_sug_b) %>% 
    pull(short_id)
  
  all_dat_use[[tg]][!(all_dat_use[[tg]] %in% use_sug_b)] <- NA
  
  all_dat_use[[tg]] <- factor(all_dat_use[[tg]], levels = use_sug_b, labels = use_short_id)
  
}


selected_columns <- all_dat_use %>% 
  select(!starts_with('pt')) %>% 
  colnames(.) %>% setdiff('type')

# 包含空格的列名需要用反引号包围
selected_columns_quoted <- sapply(selected_columns, function(col) {
  if (grepl(" ", col)) {
    paste0("`", col, "`")
  } else {
    col
  }
})

# 动态生成公式
# formula <- as.formula(paste("~", paste(selected_columns_quoted, collapse = " + "), "| pt_type"))


# table1(formula, data = all_dat_use, render.continuous = render_continuous, render.missing = NULL)



# table -------------------------------------------------------------------



for(i in selected_columns){
  label(all_dat_use[[i]]) <- i
}


# 创建标签
create_labels <- function(vars) {
  labels <- list(variables = setNames(lapply(vars, function(var) render.varlabel(all_dat_use[[var]])), vars))
  return(labels)
}

# 应用标签
labels <- create_labels(selected_columns)
# labels$groups <- c("Male", "Female", "")

strata <- c(split(all_dat_use, all_dat_use$type), list(Overall=all_dat_use))


aa <- table1(
  strata, 
  labels, 
  # groupspan = c(2, 1, 1), 
  render.continuous = render_continuous, 
  render.categorical = render_categorical,
  # extra.col=list(`P-value`=pvalue),
  render.missing = NULL
)

t1flex(aa) %>% save_as_html(path=paste0(outpath, "description_table.html"))

ft <- t1flex(aa)

# Create a new Word document
doc <- read_docx()

# Add the table to the document
doc <- body_add_flextable(doc, value = ft)

# Save the document
print(doc, target = paste0(outpath, "table_output_omicdata.docx"))


