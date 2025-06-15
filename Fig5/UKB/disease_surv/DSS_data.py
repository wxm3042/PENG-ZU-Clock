# -*- coding: utf-8 -*-
"""
Created on Sun May  4 17:45:55 2025

@author: wxm
"""


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from lifelines import CoxPHFitter
import time
import copy
from joblib import Parallel, delayed

plt.rcParams['pdf.fonttype'] = 42
plt.rcParams['font.size'] = 12
plt.rcParams['ps.fonttype'] = 42
plt.rcParams['font.family'] = 'Times New Roman'


type_dat_path ='V20250427\\output\\Fig5\\UKB\\predict_UKB\\age_gaps_with_disease.xls'
death_dat_path = '..\\00database\\UKB\\output\\disease250417\\death_data_annot.csv'
UKB_dat_path = 'V20250427\\output\\Fig5\\UKB\\predict_UKB\\UKB_data_clean.csv'

outpath = 'V20250427\\output\\Fig5\\UKB\\disease_surv\\'

# class_disease_dat = pd.read_csv(class_disease_dat_path)[['Class', 'name_l1', 'tmp3']].drop_duplicates()

UKB_dat = pd.read_csv(UKB_dat_path)[['eid', 'Gender']]

type_dat = pd.read_csv(type_dat_path, sep='\t')
type_dat = type_dat.rename(columns = {'pt_id': 'eid'})

death_dat = pd.read_csv(death_dat_path)[['eid', 'status', 'time', 'death_reason', 'name_l1', 'name_l2', 'name_l3', 'Class']]
merge_dat = pd.merge(type_dat, death_dat, on='eid')
merge_dat = pd.merge(merge_dat, UKB_dat, on='eid')
merge_dat['time'] = merge_dat['time']/365
merge_dat['Acc_type'] =  [1 if x == 'Accelerated' else 0 for x in merge_dat['type']]

disease_list = pd.DataFrame(death_dat['name_l1'].value_counts()).reset_index()
disease_list = disease_list[disease_list['count']>=10]
disease_list = disease_list.iloc[:, 0]

disease_count =pd.DataFrame(death_dat['name_l1'].value_counts()).reset_index()

class_disease_dat = death_dat[['name_l1', 'Class']].dropna().drop_duplicates()
# 初始化Cox模型

# 定义一个函数，用于处理每个疾病类型
def process_disease(d):
    
    if d in ['Diabetes mellitus', 'Renal failure', 'Glomerular diseases']:
        cph = CoxPHFitter(penalizer=0.001)
    else:
        cph = CoxPHFitter()

    sub_dat = copy.deepcopy(merge_dat)
    sub_dat['disease_status'] = 0
    sub_dat.loc[sub_dat['name_l1']==d, 'disease_status'] = 1
    # merge_dat_filter['group_binary'] = merge_dat_filter['type'].map({'Accelerated': 1, 'Slowdown': 0, 'Normal': 0})
    cph.fit(
        sub_dat[['time', 'disease_status', 'Acc_type', 'Age', 'Gender']], 
        duration_col='time', 
        event_col='disease_status'
        )
    summary_df = cph.summary
    p_values = pd.DataFrame(summary_df['p'][['Acc_type']])
    # 输出模型结果
    hr = pd.DataFrame(cph.hazard_ratios_[['Acc_type']])
    hr.columns = ['HR']

    ci_df = pd.DataFrame(np.exp(cph.confidence_intervals_))
   
    tmp_results = pd.merge(hr, p_values, on='covariate')
    tmp_results = pd.merge(tmp_results, ci_df, on='covariate')
    tmp_results['disease'] = d 
    tmp_results = tmp_results.reset_index()
    return tmp_results

# 并行执行
results = []
for d in disease_list[73:]:
    print(d)
    tmp_results = process_disease(d)
    results.append(tmp_results)
    

hr_results_df = pd.concat(results, ignore_index=True)
hr_results_df = pd.merge(hr_results_df, class_disease_dat, left_on='disease', right_on='name_l1')
hr_results_df.columns = ['group','HR', 'pvalue', 'CI_Lower', 'CI_Upper', 'Disease', 'level1', 'Class']

hr_results_df = hr_results_df[~hr_results_df['Disease'].isin(['Delivery'])]
hr_results_df_sort = hr_results_df.sort_values(['Class','HR'], ascending=[True, False])
hr_results_df_sort.to_csv(outpath + 'DSS_HR_df.xls', sep='\t', index=False)


hf_df_sig = hr_results_df_sort[hr_results_df_sort['pvalue']<0.05]
hf_df_sig = pd.merge(hf_df_sig, disease_count, left_on='Disease', right_on='name_l1')
hf_df_sig_sort = hf_df_sig.sort_values(['Class','HR'], ascending=[True, False])
hf_df_sig_sort.to_csv(outpath + 'DSS_HR_df_sig.xls', sep='\t', index=False)





