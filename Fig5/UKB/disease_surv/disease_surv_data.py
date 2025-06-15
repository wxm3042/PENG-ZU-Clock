# -*- coding: utf-8 -*-
"""
Created on Sun May  4 16:30:52 2025

@author: wxm
"""


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from lifelines import CoxPHFitter
import time
from joblib import Parallel, delayed

plt.rcParams['pdf.fonttype'] = 42
plt.rcParams['font.size'] = 12
plt.rcParams['ps.fonttype'] = 42
plt.rcParams['font.family'] = 'Times New Roman'


disease_data_path = '..\\00database\\UKB\\output\\disease250417\\disease_diagnosis_time250417.csv'
type_dat_path ='V20250427\\output\\Fig5\\UKB\\predict_UKB\\age_gaps_with_disease.xls'
death_dat_path = '..\\00database\\UKB\\output\\disease250417\\death_data_annot.csv'
disease_group_dat_path = '..\\00database\\UKB\\output\\disease\\disease_table_different_level.csv'
class_disease_dat_path = '..\\00database\\UKB\\notebook\\icd102019enMeta\\icd102019syst_chapters.txt'
#disease_count_path = '..\\00database\\UKB\\output\\disease\\disease_samp_numb_stat.csv'
UKB_dat_path = 'V20250427\\output\\Fig5\\UKB\\predict_UKB\\UKB_data_clean.csv'

outpath = 'V20250427\\output\\Fig5\\UKB\\disease_surv\\'

UKB_dat = pd.read_csv(UKB_dat_path)[['eid', 'Gender', 'BMI']]
disease_group_dat = pd.read_csv(disease_group_dat_path)[['tmp3', 'name_l1','name_l2', 'name_l3']]

class_disease_dat = pd.read_csv(class_disease_dat_path, sep=';',header=None)
class_disease_dat.columns = ['tmp3', 'Class']

disease_type_annot = pd.merge(class_disease_dat, disease_group_dat, on='tmp3')
disease_type_annot.to_csv(outpath + 'disease_type_annot.csv', index=False)

sub_annot = disease_type_annot[['Class', 'name_l1', 'tmp3']].drop_duplicates()

type_dat = pd.read_csv(type_dat_path, sep='\t')
type_dat = pd.merge(type_dat, UKB_dat, left_on='pt_id', right_on='eid', how='left')
type_dat['Acc_type'] =  [1 if x == 'Accelerated' else 0 for x in type_dat['type']]

death_dat = pd.read_csv(death_dat_path)[['eid', 'time']]
disease_dat_raw = pd.read_csv(disease_data_path)

disease_type_df = pd.DataFrame(disease_dat_raw['name_l1'].value_counts()).reset_index()
disease_type_df.to_csv(outpath + 'disease_samp_numb_stat.csv', index=False)
disease_type_df_filter = disease_type_df[disease_type_df['count']>10]
disease_type_list = disease_type_df_filter
disease_dat_filter = disease_dat_raw[disease_dat_raw['name_l1'].isin(disease_type_df_filter['name_l1'])]

disease_count = disease_type_df.copy()
# 初始化Cox模型
cph = CoxPHFitter()

# 定义一个函数，用于处理每个疾病类型
def process_disease(d):
    disease_dat = disease_dat_raw[disease_dat_raw['name_l1']==d]
    disease_dat = disease_dat[['eid', 'time', 'name_l1']]
    disease_dat = disease_dat.rename(columns={'time':'disease_time'})

    disease_merge = pd.merge(death_dat, disease_dat, on='eid', how='left')
    disease_merge['disease_status'] = 0
    disease_merge.loc[~disease_merge['disease_time'].isna(), 'disease_status'] = 1
    disease_merge['disease_time'] = disease_merge['disease_time'].fillna(disease_merge['time'])
    
    merge_dat = pd.merge(disease_merge, type_dat, left_on='eid', right_on='pt_id')
    merge_dat_filter = merge_dat[merge_dat['disease_time']>0]
    merge_dat_filter['disease_time'] = merge_dat_filter['disease_time']/365
    cph.fit(
        merge_dat_filter[['disease_time', 'disease_status', 'Acc_type', 'Age', 'Gender']], 
        duration_col='disease_time', 
        event_col='disease_status'
        )
    summary_df = cph.summary
    p_values = pd.DataFrame(summary_df['p'][['Acc_type']])
    # 输出模型结果
    hr = pd.DataFrame(cph.hazard_ratios_[['Acc_type']])
    hr.columns = ['HR']

    ci_df = pd.DataFrame(np.exp(cph.confidence_intervals_))
    [['Acc_type']]
    
    tmp_results = pd.merge(hr, p_values, on='covariate')
    tmp_results = pd.merge(tmp_results, ci_df, on='covariate')
    tmp_results['disease'] = d 
    tmp_results = tmp_results.reset_index()
    return tmp_results

# 并行执行
disease_list = disease_type_list.loc[:239, 'name_l1']
results = Parallel(n_jobs=16)(delayed(process_disease)(d) for d in disease_list)

hr_results_df = pd.concat(results, ignore_index=True)
hr_results_df = pd.merge(hr_results_df, sub_annot, left_on='disease', right_on='name_l1')
hr_results_df.columns = ['group','HR', 'pvalue', 'CI_Lower', 'CI_Upper', 'Disease', 'Class', 'level1','Class_id']

hr_results_df = hr_results_df[~hr_results_df['Disease'].isin(['Delivery'])]
hr_results_df_sort = hr_results_df.sort_values(['Class_id','HR'], ascending=[True, False])
hr_results_df_sort.to_csv(outpath + 'disease_HR_df.xls', sep='\t', index=False)


hf_df_sig = hr_results_df_sort[hr_results_df_sort['pvalue']<0.05]
hf_df_sig = pd.merge(hf_df_sig, disease_count, left_on='Disease', right_on='name_l1')
#hf_df_sig = hf_df_sig.sort_values('pvalue')
hf_df_sig_sort = hf_df_sig.sort_values(['Class_id','HR'], ascending=[True, False])
hf_df_sig_sort = hf_df_sig_sort[hf_df_sig_sort['Class_id']<=14]
hf_df_sig_sort.to_csv(outpath + 'disease_HR_df_sig.xls', sep='\t', index=False)
