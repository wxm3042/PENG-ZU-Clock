# -*- coding: utf-8 -*-
"""
Created on Fri Apr 18 10:06:35 2025

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



diabete_data_path = '..\\00database\\UKB\\output\\disease250417\\diabetes_diagnosis_time250417.csv'
type_dat_path ='V20250327\\output\\Fig5\\UKB\\predict_UKB\\age_gaps_with_disease.xls'
death_dat_path = '..\\00database\\UKB\\output\\disease250417\\death_data_annot.csv'
UKB_data_path = 'V20250327\\output\\Fig5\\UKB\\predict_UKB\\UKB_data_clean.csv'


outpath = 'V20250327\\output\\Fig5\\UKB\\predict_UKB\\'
col_dir = {
    'Accelerated': '#b82122',
    'Slowdown': '#3773B6',
    'Normal': '#BEBEBE'
    }

UKB_dat = pd.read_csv(UKB_data_path)

pre_diabete_pt = UKB_dat[(UKB_dat['FBG']>=6.1) & (UKB_dat['FBG']<7)]['eid']
pre_diabete_pt.to_csv(outpath + 'prediabetes_pt.csv', index=False)


death_dat = pd.read_csv(death_dat_path)[['eid', 'time']]
diabete_dat = pd.read_csv(diabete_data_path)[['eid', 'time']]
diabete_dat.rename(columns={'time':'disease_time'}, inplace=True)

type_dat = pd.read_csv(type_dat_path, sep='\t')

disease_merge = pd.merge(death_dat, diabete_dat, on='eid', how='left')
disease_merge['disease_status'] = 0
disease_merge.loc[~disease_merge['disease_time'].isna(), 'disease_status'] = 1
disease_merge['disease_time'] = disease_merge['disease_time'].fillna(disease_merge['time'])

merge_dat = pd.merge(disease_merge, type_dat, left_on='eid', right_on='sample_id')
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
    kmf.plot_cumulative_density(color=col_dir.get(group, None))

results = multivariate_logrank_test(merge_dat_filter['time'], merge_dat_filter['type'], merge_dat_filter['disease_status'])
p_value = results.p_value
plt.text(0.5, 0.1, f'Log-rank p-value: {p_value:.3f}', ha='center', va='center', transform=plt.gca().transAxes)
plt.title('Diabetes cumulative prevalence curve')
plt.xlabel('Time (year)')
plt.ylabel('Cumulative prevalence')
plt.savefig(outpath + 'Prediabetes cumulative prevalence curve.pdf')

