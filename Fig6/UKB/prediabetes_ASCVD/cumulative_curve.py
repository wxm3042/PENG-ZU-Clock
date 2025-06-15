# -*- coding: utf-8 -*-
"""
Created on Mon Apr  7 09:11:33 2025

@author: wxm
"""



import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from lifelines import KaplanMeierFitter
from lifelines import CoxPHFitter
from lifelines.statistics import multivariate_logrank_test

plt.rcParams['pdf.fonttype'] = 42
plt.rcParams['font.size'] = 12
plt.rcParams['ps.fonttype'] = 42
plt.rcParams['font.family'] = 'Times New Roman'


diabete_data_path = '..\\00database\\UKB\\output\\disease250417\\diabetes_diagnosis_time250417_T2D_E11.csv'
type_dat_path ='V20250427\\output\\Fig5\\UKB\\predict_UKB\\age_gaps_with_disease.xls'
death_dat_path = '..\\00database\\UKB\\output\\disease\\surv_data_clean_annot.csv'
UKB_data_path = 'V20250427\\output\\Fig5\\UKB\\predict_UKB\\UKB_data_clean.csv'

outpath = 'V20250427\\output\\Fig6\\UKB\\prediabetes_ASCVD\\'
col_dir = {
    'Accelerated': '#B8281D',
    'Decelerated': '#3773B6',
    'Normal': '#BFBEBE'
    }

UKB_dat = pd.read_csv(UKB_data_path)

#pre_diabete_pt = UKB_dat[(UKB_dat['FBG']>=6.1) & (UKB_dat['FBG']<7)]['eid']
pre_diabete_pt = UKB_dat[UKB_dat['FBG']<7]['eid']
pre_diabete_pt.to_csv(outpath + 'prediabetes_pt.csv', index=False)


death_dat = pd.read_csv(death_dat_path)[['eid', 'time']]
diabete_dat = pd.read_csv(diabete_data_path)[['eid', 'time']]
diabete_dat = diabete_dat.rename(columns={'time':'disease_time'})

type_dat = pd.read_csv(type_dat_path, sep='\t')

disease_merge = pd.merge(death_dat, diabete_dat, on='eid', how='left')
disease_merge['disease_status'] = 0
disease_merge.loc[~disease_merge['disease_time'].isna(), 'disease_status'] = 1
disease_merge['disease_time'] = disease_merge['disease_time'].fillna(disease_merge['time'])

merge_dat = pd.merge(disease_merge, type_dat, left_on='eid', right_on='pt_id')
merge_dat_filter = merge_dat[merge_dat['disease_time']>0]
merge_dat_filter['disease_time'] = merge_dat_filter['disease_time']/365
merge_dat_filter = merge_dat_filter[merge_dat_filter['eid'].isin(pre_diabete_pt)]

# 创建Kaplan-Meier生存模型
kmf = KaplanMeierFitter()

# 绘制累积患病率曲线
plt.figure(figsize=(4, 4))
for group in merge_dat_filter['type'].unique():
    group_data = merge_dat_filter[merge_dat_filter['type'] == group]
    kmf.fit(group_data['disease_time'], event_observed=group_data['disease_status'], label=group)
    kmf.plot_cumulative_density(color=col_dir[group])
results = multivariate_logrank_test(merge_dat_filter['disease_time'], merge_dat_filter['type'], merge_dat_filter['disease_status'])
p_value = results.p_value
plt.text(0.5, 0.1, f'Log-rank p-value: {p_value:.3f}', ha='center', va='center', transform=plt.gca().transAxes)
plt.title('Diabetes cumulative prevalence curve')
plt.xlabel('Time (year)')
plt.ylabel('Cumulative prevalence')
plt.savefig(outpath + 'Prediabetes cumulative prevalence curve.pdf')




# 定义时间段
time_intervals = [
    (0, 5),    # 0-5 年
    (5, 10),   # 5-10 年
    (10, 15)   # 10-15 年
]

# 创建 Kaplan-Meier 模型

# 绘制累积患病率曲线
plt.figure(figsize=(4,4))

for start_time, end_time in time_intervals:
    # 筛选当前时间段的数据
    interval_data = merge_dat_filter[(merge_dat_filter['disease_time'] >= start_time) & 
                                     (merge_dat_filter['disease_time'] <= end_time)]
    kmf = KaplanMeierFitter()

    # 对每个分组拟合 Kaplan-Meier 模型
    for group in interval_data['type'].unique():
        group_data = interval_data[interval_data['type'] == group]
        kmf.fit(group_data['disease_time'], event_observed=group_data['disease_status'], 
                label=f'{group} ({start_time}-{end_time} years)')
        kmf.plot_cumulative_density(color=col_dir[group])


    # 设置标题和标签
    plt.title('Diabetes Cumulative Prevalence Curve by Time Intervals')
    plt.xlabel('Time (years)')
    plt.ylabel('Cumulative Prevalence')


# 保存图表
plt.savefig(outpath + 'Prediabetes_Cumulative_Prevalence_Curve_Time_Intervals.pdf')



# 创建 Kaplan-Meier 模型
kmf = KaplanMeierFitter()

# 遍历每个时间段，绘制单独的图
for start_time, end_time in time_intervals:
    plt.figure(figsize=(4, 4))
    
    # 筛选当前时间段的数据
    interval_data = merge_dat_filter[(merge_dat_filter['disease_time'] >= start_time) & 
                                     (merge_dat_filter['disease_time'] <= end_time)]
    
    # 对每个分组拟合 Kaplan-Meier 模型
    for group in interval_data['type'].unique():
        group_data = interval_data[interval_data['type'] == group]
        kmf.fit(group_data['disease_time'], event_observed=group_data['disease_status'], 
                label=f'{group} ({start_time}-{end_time} years)')
        kmf.plot_cumulative_density(color=col_dir[group])
    results = multivariate_logrank_test(merge_dat_filter['disease_time'], merge_dat_filter['type'], merge_dat_filter['disease_status'])
    p_value = results.p_value
    plt.text(0.5, 0.1, f'Log-rank p-value: {p_value:.3f}', ha='center', va='center', transform=plt.gca().transAxes)

    # 设置标题和标签
    plt.title(f'Diabetes Cumulative Prevalence Curve ({start_time}-{end_time} years)')
    plt.xlabel('Time (years)')
    plt.ylabel('Cumulative Prevalence')
    
    # 调整横轴范围
    plt.xlim(start_time, end_time)
    
    # 保存图表
    plt.savefig(outpath + f'Prediabetes_Cumulative_Prevalence_Curve_{start_time}_{end_time}_years.pdf')
    
    # 显示图表
    plt.show()

