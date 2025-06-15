# -*- coding: utf-8 -*-
"""
Created on Fri Jan 31 13:39:32 2025

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



diabete_data_path = '..\\00database\\UKB\\output\\disease250417\\diabetes_diagnosis_time250417_T2D_E11.csv'
type_dat_path ='V20250427\\output\\Fig8\\FBG_SCr_RI_group.txt'
death_dat_path = '..\\00database\\UKB\\output\\disease250417\\death_data_annot.csv'
gender_list = ['male', 'female']
col_dir = {
    'Above_Range': '#B8281D',
    'Below_Range': '#3773B6',
    'Within_Range': '#BFBEBE'
    }

outpath = 'V20250427\\output\\Fig8\\'

all_death_dat = pd.read_csv(death_dat_path)[['eid', 'time', 'death_reason', 'status']]
death_dat = all_death_dat[['eid', 'time']]
diabete_dat = pd.read_csv(diabete_data_path)[['eid', 'time']]
diabete_dat = diabete_dat.rename(columns={'time':'disease_time'})

type_dat = pd.read_csv(type_dat_path, sep='\t')
type_dat =type_dat[type_dat['fea']=='FBG']

disease_merge = pd.merge(death_dat, diabete_dat, on='eid', how='left')
disease_merge['disease_status'] = 0
disease_merge.loc[~disease_merge['disease_time'].isna(), 'disease_status'] = 1
disease_merge['disease_time'] = disease_merge['disease_time'].fillna(disease_merge['time'])


for g in gender_list:
    
    # cumulative incidence curve
    sub_type_dat = type_dat[type_dat['gender']==g][['eid', 'grp']]

    merge_dat = pd.merge(disease_merge, sub_type_dat, on='eid')
    merge_dat_filter = merge_dat[merge_dat['disease_time']>0]
    merge_dat_filter['disease_time'] = merge_dat_filter['disease_time']/365
    
    # 创建Kaplan-Meier生存模型
    kmf = KaplanMeierFitter()
    
    # 绘制累积患病率曲线
    plt.figure(figsize=(4, 4))
    for group in merge_dat_filter['grp'].unique():
        print(group)
        group_data = merge_dat_filter[merge_dat_filter['grp'] == group]
        kmf.fit(group_data['disease_time'], event_observed=group_data['disease_status'], label=group)
        kmf.plot_cumulative_density(color = col_dir[group])
    
    results = multivariate_logrank_test(merge_dat_filter['disease_time'], merge_dat_filter['grp'], merge_dat_filter['disease_status'])
    p_value = results.p_value
    plt.text(0.5, 0.1, f'Log-rank p-value: {p_value:.3f}', ha='center', va='center', transform=plt.gca().transAxes)
    plt.title('Diabetes cumulative prevalence curve')
    plt.xlabel('Time (year)')
    plt.ylabel('Cumulative prevalence')
    plt.savefig(outpath + g + '_FBG_grp_Diabetes_CIC.pdf')
    
    # total surv
    surv_dat = pd.merge(all_death_dat, sub_type_dat, on='eid')
    surv_dat['time'] = surv_dat['time']/365

    global_results = multivariate_logrank_test(
        event_durations=surv_dat['time'],
        groups=surv_dat['grp'],
        event_observed=surv_dat['status']
    )
    p_value = global_results.p_value
    
    kmf = KaplanMeierFitter()
        # 绘制累积患病率曲线
    plt.figure(figsize=(4, 4))
    for group in surv_dat['grp'].unique():
        group_data = surv_dat[surv_dat['grp'] == group]
        kmf.fit(group_data['time'], event_observed=group_data['status'], label=group)
        kmf.plot_survival_function(color = col_dir[group])
    
    results = multivariate_logrank_test(surv_dat['time'], surv_dat['grp'], surv_dat['status'])
    p_value = results.p_value
    plt.text(0.5, 0.1, f'Log-rank p-value: {p_value:.3f}', ha='center', va='center', transform=plt.gca().transAxes)

    plt.title('OS')
    plt.xlabel('Time (year)')
    plt.ylabel('Survival Probability')
    plt.savefig(outpath + g + '_FBG_grp_OS.pdf')
    
    # diabetes surv
    
    surv_dat_diabetes = copy.deepcopy(surv_dat)
    surv_dat_diabetes['spe_status'] = surv_dat_diabetes['death_reason'].apply(lambda x: 1 if pd.notna(x) and str(x).startswith(('E11')) else 0)
    
    kmf = KaplanMeierFitter()
        # 绘制累积患病率曲线
    plt.figure(figsize=(4, 4))
    for group in surv_dat_diabetes['grp'].unique():
        group_data = surv_dat_diabetes[surv_dat_diabetes['grp'] == group]
        kmf.fit(group_data['time'], event_observed=group_data['spe_status'], label=group)
        kmf.plot_survival_function(color = col_dir[group])

    results = multivariate_logrank_test(surv_dat_diabetes['time'], surv_dat_diabetes['grp'], surv_dat_diabetes['spe_status'])
    p_value = results.p_value
    plt.text(0.5, 0.1, f'Log-rank p-value: {p_value:.3f}', ha='center', va='center', transform=plt.gca().transAxes)

    plt.title('OS')
    plt.xlabel('Time (year)')
    plt.ylabel('Survival Probability')
    plt.savefig(outpath + g + '_FBG_grp_DSS.pdf')
    
    