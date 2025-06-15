# -*- coding: utf-8 -*-
"""
Created on Thu Feb  6 15:19:58 2025

@author: wxm
"""


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from lifelines import KaplanMeierFitter
from lifelines import CoxPHFitter
from matplotlib.backends.backend_pdf import PdfPages
from lifelines.statistics import multivariate_logrank_test

# from googletrans import Translator

plt.rcParams['pdf.fonttype'] = 42
plt.rcParams['font.size'] = 12
plt.rcParams['ps.fonttype'] = 42
plt.rcParams['font.family'] = 'Times New Roman'


disease_data_path = '..\\00database\\UKB\\output\\disease250417\\disease_diagnosis_time250417.csv'
type_dat_path ='V20250427\\output\\Fig5\\UKB\\predict_UKB\\age_gaps_with_disease.xls'
death_dat_path = '..\\00database\\UKB\\output\\disease250417\\death_data_annot.csv'
ASCVD_path = '..\\00database\\UKB\\output\\disease\\ASCVD_score.csv'
ASCVD_ICD10_code = [
    'I20',
    'I21',
    'I22',
    'I25',
    'I63',
    'I65',
    'I66',
    'I70',
    'I73'
    ]
col_dir = {
    'Accelerated': '#B8281D',
    'Decelerated': '#3773B6',
    'Normal': '#BFBEBE'
    }


outpath = 'V20250427\\output\\Fig6\\UKB\\prediabetes_ASCVD\\'

ASCVD_type = pd.read_csv(ASCVD_path)
ASCVD_type['ASCVD_risk'] = 'mid'
ASCVD_type.loc[ASCVD_type['risk']<0.05, 'ASCVD_risk'] = 'low'
ASCVD_type.loc[ASCVD_type['risk']>=0.1, 'ASCVD_risk'] = 'high'
low_ascvd = ASCVD_type[ASCVD_type['ASCVD_risk']=='low']
low_ascvd.to_csv(outpath + 'low_ascvd_dat.csv')

type_dat = pd.read_csv(type_dat_path, sep='\t')

death_dat = pd.read_csv(death_dat_path)[['eid', 'time']]
disease_dat_raw = pd.read_csv(disease_data_path)

disease_dat = disease_dat_raw[disease_dat_raw['id_l2'].isin(ASCVD_ICD10_code)]
disease_dat = disease_dat[['eid', 'time']]
disease_dat = disease_dat.rename(columns={'time':'disease_time'})

disease_merge = pd.merge(death_dat, disease_dat, on='eid', how='left')
disease_merge['disease_status'] = 0
disease_merge.loc[~disease_merge['disease_time'].isna(), 'disease_status'] = 1
disease_merge['disease_time'] = disease_merge['disease_time'].fillna(disease_merge['time'])

merge_dat = pd.merge(disease_merge, type_dat, left_on='eid', right_on='pt_id')
merge_dat_filter = merge_dat[merge_dat['disease_time']>0]
merge_dat_filter['disease_time'] = merge_dat_filter['disease_time']/365
merge_dat_filter = merge_dat_filter[merge_dat_filter['eid'].isin(low_ascvd['eid'])]
# 创建Kaplan-Meier生存模型
kmf = KaplanMeierFitter()
# 绘制累积患病率曲线
plt.figure(figsize=(4,4))
for group in merge_dat_filter['type'].unique():
    group_data = merge_dat_filter[merge_dat_filter['type'] == group]
    kmf.fit(group_data['disease_time'], event_observed=group_data['disease_status'], label=group)
    kmf.plot_cumulative_density(color=col_dir[group])
results = multivariate_logrank_test(merge_dat_filter['disease_time'], merge_dat_filter['type'], merge_dat_filter['disease_status'])
p_value = results.p_value

plt.text(0.5, 0.1, f'Log-rank p-value: {p_value:.3f}', ha='center', va='center', transform=plt.gca().transAxes)
plt.title('ASCVD')
plt.xlabel('Time (year)')
plt.ylabel('Cumulative prevalence')
plt.savefig(outpath + 'ASCVD_low_cause-specific_cumulative_curv.pdf')

