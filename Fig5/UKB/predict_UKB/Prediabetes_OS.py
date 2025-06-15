# -*- coding: utf-8 -*-
"""
Created on Fri Apr 18 10:13:38 2025

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

type_dat_path ='V20250327\\output\\Fig5\\UKB\\predict_UKB\\age_gaps_with_disease.xls'
death_dat_path = '..\\00database\\UKB\\output\\disease250417\\death_data_annot.csv'
pre_diabete_dat_path = 'V20250327\\output\\Fig5\\UKB\\predict_UKB\\prediabetes_pt.csv'

outpath = 'V20250327\\output\\Fig5\\UKB\\predict_UKB\\'

col_dir = {
    'Accelerated': '#b82122',
    'Slowdown': '#3773B6',
    'Normal': '#BEBEBE'
    }
pre_diabete_pt = pd.read_csv(pre_diabete_dat_path)['eid']

death_dat = pd.read_csv(death_dat_path)[['eid', 'status', 'time', 'death_reason', 'name_l1', 'name_l2', 'name_l3']]

type_dat = pd.read_csv(type_dat_path, sep='\t')

merge_dat = pd.merge(death_dat, type_dat, left_on='eid', right_on='sample_id')
merge_dat_filter = merge_dat[merge_dat['eid'].isin(pre_diabete_pt)]
merge_dat_filter['time'] = merge_dat_filter['time']/365
# 创建Kaplan-Meier生存模型
kmf = KaplanMeierFitter()

# 绘制累积患病率曲线
plt.figure(figsize=(4, 4))
for group in merge_dat_filter['type'].unique():
    group_data = merge_dat_filter[merge_dat_filter['type'] == group]
    kmf.fit(group_data['time'], event_observed=group_data['status'], label=group)
    kmf.plot_survival_function(color=col_dir[group])

results = multivariate_logrank_test(merge_dat_filter['time'], merge_dat_filter['type'], merge_dat_filter['status'])
p_value = results.p_value
plt.text(0.5, 0.1, f'Log-rank p-value: {p_value:.3f}', ha='center', va='center', transform=plt.gca().transAxes)

plt.title('')
plt.xlabel('Time (years)')
plt.ylabel('Overall survival probability')
plt.savefig(outpath + 'Prediabetes surv plot.pdf')
