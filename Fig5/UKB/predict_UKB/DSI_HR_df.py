# -*- coding: utf-8 -*-
"""
Created on Thu Apr  3 17:57:02 2025

@author: wxm
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from lifelines import CoxPHFitter
import time
from joblib import Parallel, delayed
from statsmodels.stats.multitest import multipletests

plt.rcParams['pdf.fonttype'] = 42
plt.rcParams['font.size'] = 12
plt.rcParams['ps.fonttype'] = 42
plt.rcParams['font.family'] = 'Times New Roman'

def process_disease_penalizer(d):
    disease_dat = disease_dat_filter[disease_dat_filter['disease_name']==d]
    disease_dat = disease_dat[['eid', 'time', 'disease_id']]
    disease_dat = disease_dat.rename(columns={'time':'disease_time'})
    
    disease_merge = pd.merge(death_dat, disease_dat, on='eid', how='left')
    disease_merge['disease_status'] = 0
    disease_merge.loc[~disease_merge['disease_time'].isna(), 'disease_status'] = 1
    disease_merge['disease_time'] = disease_merge['disease_time'].fillna(disease_merge['time'])
    
    merge_dat = pd.merge(disease_merge[['eid', 'disease_time', 'disease_status']], type_dat, on='eid')
    merge_dat_filter = merge_dat[merge_dat['disease_time']>0]
    merge_dat_filter['disease_time'] = merge_dat_filter['disease_time']/365

    sub_dat = merge_dat_filter.drop('eid', axis=1)
    
    penalizers = [0.0001, 0.001, 0.01, 0.1, 1]  # 预设的 penalizer 值
    best_penalizer = None
    best_aic = float('inf')  # 记录 AIC 最小值
    best_cph = None  # 记录最佳模型

    # 遍历不同的 penalizer 进行 Cox 回归
    for p in penalizers:
        try:
            cph = CoxPHFitter(penalizer=p)
            cph.fit(sub_dat, duration_col='disease_time', event_col='disease_status')
            aic = cph.AIC_partial_  # 计算 AIC

            # 选择 AIC 最小的 penalizer
            if aic < best_aic:
                best_aic = aic
                best_penalizer = p
                best_cph = cph  # 记录最佳模型

        except Exception as e:
            print(f"Penalizer {p} failed for {d}: {e}")
            continue

    # 提取最终模型的结果
    summary_df = best_cph.summary
    p_values = pd.DataFrame(summary_df['p'][['Acc_type']])
    hr = pd.DataFrame(best_cph.hazard_ratios_[['Acc_type']])
    hr.columns = ['HR']
    ci_df = pd.DataFrame(np.exp(best_cph.confidence_intervals_))

    # 合并结果
    tmp_results = pd.merge(hr, p_values, on='covariate')
    tmp_results = pd.merge(tmp_results, ci_df, on='covariate')
    tmp_results['disease'] = d
    tmp_results['best_penalizer'] = best_penalizer  # 记录最佳 penalizer 值
    tmp_results = tmp_results.reset_index()

    return tmp_results

disease_data_path = '..\\00database\\UKB\\output\\disease250417\\disease_diagnosis_time250417.csv'
type_dat_path ='V20250327\\output\\Fig5\\UKB\\predict_UKB\\age_gaps_with_disease.xls'
death_dat_path = '..\\00database\\UKB\\output\\disease250417\\death_data_annot.csv'
disease_group_dat_path = '..\\00database\\UKB\\output\\disease\\disease_table_different_level_add_class_level1.csv'
covariate_path = '..\\00database\\UKB\\output\\feature\\covariate_feature_data.csv'

outpath = 'V20250327\\output\\Fig5\\UKB\\predict_UKB\\'

covariate_df = pd.read_csv(covariate_path)
covariate_df = covariate_df.dropna(subset=['Smoking_status'])
covariate_df['Ethnicity'] = covariate_df['Ethnicity'].replace(5.0, 3.0)


sub_annot = pd.read_csv(disease_group_dat_path)[['Class', 'name_l1', 'tmp3']].drop_duplicates()

type_dat = pd.read_csv(type_dat_path, sep='\t')
#type_dat = pd.merge(type_dat, UKB_dat, left_on='sample_id', right_on='eid', how='left')
type_dat['Acc_type'] =  [1 if x == 'Accelerated' else 0 for x in type_dat['type']]
type_dat = type_dat.rename(columns={'sample_id': 'eid'})
type_dat =type_dat[['eid', 'Acc_type']]
type_dat = pd.merge(type_dat, covariate_df, on='eid')
type_dat = pd.get_dummies(type_dat, columns=['Income', 'drinking_category', 'Ethnicity'], drop_first=True)


death_dat = pd.read_csv(death_dat_path)[['eid', 'time']]
disease_dat_raw = pd.read_csv(disease_data_path)
disease_dat_raw.rename(columns={'name_l1': 'disease_name', 'id_l2': 'disease_id'}, inplace=True)
disease_dat_filter1 = disease_dat_raw[disease_dat_raw['time']>0]

disease_type_df = pd.DataFrame(disease_dat_filter1['disease_name'].value_counts()).reset_index()
disease_type_df_filter = disease_type_df[disease_type_df['count']>200]

disease_dat_filter = disease_dat_filter1[disease_dat_filter1['disease_name'].isin(disease_type_df_filter['disease_name'])]

# 并行执行
disease_list = disease_type_df_filter['disease_name']
results = Parallel(n_jobs=16)(delayed(process_disease_penalizer)(d) for d in disease_list)

hr_results_df = pd.concat(results, ignore_index=True)
reject, pvals_corrected, _, _ = multipletests(hr_results_df['p'], alpha=0.05, method='fdr_bh')

# 将结果添加到 DataFrame 中
hr_results_df['padj'] = pvals_corrected

hr_results_df = pd.merge(hr_results_df, sub_annot, left_on='disease', right_on='name_l1')
hr_results_df.columns = ['group','HR', 'pvalue', 'CI_Lower', 'CI_Upper', 'Disease', 'best_penalizer', 'padj', 'Class', 'level1','Class_id']

hr_results_df = hr_results_df[~hr_results_df['Disease'].isin(['Delivery'])]
hr_results_df_sort = hr_results_df.sort_values(['Class','HR'], ascending=[True, False])
hr_results_df_sort.to_csv(outpath + 'DSI_HR_df.xls.xls', sep='\t', index=False)


hf_df_sig = hr_results_df_sort[hr_results_df_sort['pvalue']<0.05]
hf_df_sig_sort = hf_df_sig.sort_values(['Class','HR'], ascending=[True, False])
# hf_df_sig_sort = hf_df_sig_sort[hf_df_sig_sort['Class_id']<=14]
hf_df_sig_sort.to_csv(outpath + 'DSI_HR_df_sig_pval.xls', sep='\t', index=False)


hf_df_sig = hr_results_df_sort[hr_results_df_sort['padj']<0.05]
hf_df_sig_sort = hf_df_sig.sort_values(['Class','HR'], ascending=[True, False])
# hf_df_sig_sort = hf_df_sig_sort[hf_df_sig_sort['Class_id']<=14]
hf_df_sig_sort.to_csv(outpath + 'DSI_HR_df_sig_padj.xls', sep='\t', index=False)






