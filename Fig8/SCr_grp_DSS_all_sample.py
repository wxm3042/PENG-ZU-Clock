# -*- coding: utf-8 -*-
"""
Created on Thu May  8 15:28:45 2025

@author: wxm
"""


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from lifelines.statistics import multivariate_logrank_test
import copy
from lifelines import KaplanMeierFitter
from lifelines import CoxPHFitter
plt.rcParams['pdf.fonttype'] = 42
plt.rcParams['font.size'] = 12
plt.rcParams['ps.fonttype'] = 42
plt.rcParams['font.family'] = 'Times New Roman'

renal_name_path = 'V20250427\\data\\renal_disease.xlsx'
disease_time_dat_path = '..\\00database\\UKB\\output\\disease250417\\disease_diagnosis_time250417.csv'
type_dat_path ='V20250427\\output\\Fig8\\FBG_SCr_RI_group.txt'
death_dat_path = '..\\00database\\UKB\\output\\disease250417\\death_data_annot.csv'
gender_list = ['male', 'female']
col_dir = {
    'Above_Range': '#B8281D',
    'Below_Range': '#3773B6',
    'Within_Range': '#BFBEBE'
    }

outpath = 'V20250427\\output\\Fig8\\'

renal_name = pd.read_excel(renal_name_path)
all_death_dat = pd.read_csv(death_dat_path)[['eid', 'time', 'death_reason', 'status']]
death_dat = all_death_dat[['eid', 'time']]
disease_dat = pd.read_csv(disease_time_dat_path)[['eid', 'id_l2', 'name_l2', 'time']]
disease_dat = disease_dat.rename(columns={'time':'disease_time'})

type_dat = pd.read_csv(type_dat_path, sep='\t')
type_dat =type_dat[type_dat['fea']=='SCr']

all_disease_time_list = []
for d in renal_name['id']:
    print(d)
    sub_disease_dat = disease_dat[disease_dat['id_l2']==d]
    sub_disease_dat = sub_disease_dat[['eid', 'disease_time']]
    all_disease_time_list.append(sub_disease_dat)
    
all_disease_time = pd.concat(all_disease_time_list)

disease_merge = pd.merge(death_dat, all_disease_time, on='eid', how='left')
disease_merge['disease_status'] = 0
disease_merge.loc[~disease_merge['disease_time'].isna(), 'disease_status'] = 1
disease_merge['disease_time'] = disease_merge['disease_time'].fillna(disease_merge['time'])

# cumulative incidence curve

sub_type_dat = type_dat[['eid', 'grp']]

merge_dat = pd.merge(disease_merge, sub_type_dat, on='eid')
merge_dat_filter = merge_dat[merge_dat['disease_time']>0]
merge_dat_filter['disease_time'] = merge_dat_filter['disease_time']/365

global_results = multivariate_logrank_test(
    event_durations=merge_dat_filter['disease_time'],
    groups=merge_dat_filter['grp'],
    event_observed=merge_dat_filter['disease_status']
)
p_value_cic = global_results.p_value


# 创建Kaplan-Meier生存模型
kmf = KaplanMeierFitter()

# 绘制累积患病率曲线
plt.figure(figsize=(4, 4))
for group in merge_dat_filter['grp'].unique():
    print(group)
    group_data = merge_dat_filter[merge_dat_filter['grp'] == group]
    kmf.fit(group_data['disease_time'], event_observed=group_data['disease_status'], label=group)
    kmf.plot_cumulative_density(color = col_dir[group])

plt.text(0.5, 0.1, f'p-value: {p_value_cic:.4f}', fontsize=12, transform=plt.gca().transAxes)    
plt.title('Diabetes cumulative prevalence curve')
plt.xlabel('Time (year)')
plt.ylabel('Cumulative prevalence')
plt.savefig(outpath + 'All_SCr_grp_renal_CIC.pdf')

# diabetes surv
surv_dat = pd.merge(all_death_dat, sub_type_dat, on='eid')
surv_dat['time'] = surv_dat['time']/365

surv_dat_renal = copy.deepcopy(surv_dat)
surv_dat_renal['spe_status'] = surv_dat_renal['death_reason'].apply(lambda x: 1 if pd.notna(x) and str(x).startswith(tuple(renal_name['id'])) else 0)

global_results = multivariate_logrank_test(
    event_durations=surv_dat_renal['time'],
    groups=surv_dat_renal['grp'],
    event_observed=surv_dat_renal['spe_status']
)
p_value_DSS = global_results.p_value

kmf = KaplanMeierFitter()
    # 绘制累积患病率曲线
plt.figure(figsize=(4, 4))
for group in surv_dat_renal['grp'].unique():
    group_data = surv_dat_renal[surv_dat_renal['grp'] == group]
    kmf.fit(group_data['time'], event_observed=group_data['spe_status'], label=group)
    kmf.plot_survival_function(color = col_dir[group])
    
plt.text(0.5, 0.1, f'p-value: {p_value_DSS:.4f}', fontsize=12, transform=plt.gca().transAxes)    
plt.title('OS')
plt.xlabel('Time (year)')
plt.ylabel('Survival Probability')
plt.savefig(outpath + 'All_renal_grp_DSS.pdf')
