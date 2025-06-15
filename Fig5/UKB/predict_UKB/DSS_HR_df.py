# -*- coding: utf-8 -*-
"""
Created on Wed Apr  2 17:13:34 2025

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

# function
# 定义一个函数，用于处理每个疾病类型

    
    sub_dat = copy.deepcopy(cox_dat)
    sub_dat['disease_status'] = 0
    sub_dat.loc[sub_dat['name_l1']==d, 'disease_status'] = 1
    sub_dat = sub_dat.drop(['name_l1'], axis=1)
    cph.fit(
        sub_dat, 
        duration_col='time', 
        event_col='disease_status'
        )
    summary_df = cph.summary
    p_values = pd.DataFrame(summary_df['p'][['type_Accelerated', 'type_Slowdown']])
    # 输出模型结果
    hr = pd.DataFrame(cph.hazard_ratios_[['type_Accelerated', 'type_Slowdown']])
    hr.columns = ['HR']

    ci_df = pd.DataFrame(np.exp(cph.confidence_intervals_))
   
    tmp_results = pd.merge(hr, p_values, on='covariate')
    tmp_results = pd.merge(tmp_results, ci_df, on='covariate')
    tmp_results['disease'] = d 
    tmp_results = tmp_results.reset_index()
    return tmp_results

def process_disease_penalizer(d):
    sub_dat = copy.deepcopy(cox_dat)
    sub_dat['disease_status'] = 0
    sub_dat.loc[sub_dat['name_l1'] == d, 'disease_status'] = 1
    sub_dat = sub_dat.drop(['name_l1'], axis=1)
    
    penalizers = [0.0001, 0.001, 0.01, 0.1, 1]  # 预设的 penalizer 值
    best_penalizer = None
    best_aic = float('inf')  # 记录 AIC 最小值
    best_cph = None  # 记录最佳模型

    # 遍历不同的 penalizer 进行 Cox 回归
    for p in penalizers:
        try:
            cph = CoxPHFitter(penalizer=p)
            cph.fit(sub_dat, duration_col='time', event_col='disease_status')
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

covariate_path = '..\\00database\\UKB\\output\\feature\\covariate_feature_data.csv'
type_dat_path ='V20250327\\output\\Fig5\\UKB\\predict_UKB\\age_gaps_with_disease.xls'
death_dat_path = '..\\00database\\UKB\\output\\disease\\surv_data_clean_annot.csv'
UKB_dat_path = 'V20250327\\output\\Fig5\\UKB\\predict_UKB\\UKB_data_clean.csv'
class_disease_dat_path = 'output\\3.clock\\changsha_clock\\elastic_net_scale\\UKB_clock_250121\\UKB\\disease_type_annot.csv'

outpath = 'V20250327\\output\\Fig5\\UKB\\predict_UKB\\'

covariate_df = pd.read_csv(covariate_path)
covariate_df = covariate_df.dropna(subset=['Smoking_status'])
covariate_df['Ethnicity'] = covariate_df['Ethnicity'].replace(5.0, 3.0)

class_disease_dat = pd.read_csv(class_disease_dat_path)[['Class', 'name_l1', 'tmp3']].drop_duplicates()

type_dat = pd.read_csv(type_dat_path, sep='\t')[['sample_id', 'type']]
type_dat = type_dat.rename(columns = {'sample_id': 'eid'})

death_dat = pd.read_csv(death_dat_path)[['eid', 'status', 'time', 'death_reason', 'name_l1', 'name_l2', 'name_l3']]
merge_dat = pd.merge(type_dat, death_dat, on='eid')
merge_dat['time'] = merge_dat['time']/365
merge_dat['Acc_type'] =  [1 if x == 'Accelerated' else 0 for x in merge_dat['type']]
merge_dat_cov = pd.merge(merge_dat, covariate_df, on='eid')
merge_dat_cov = pd.get_dummies(merge_dat_cov, columns=['Income', 'drinking_category', 'Ethnicity'], drop_first=True)

cox_dat = merge_dat_cov[[
    'time', 
    'Acc_type', 
    'Age', 
    'Gender', 
    'name_l1',
    'BMI', 
    'Smoking_status',
    'Income_high_mid_income', 
    'Income_low_income', 
    'Income_low_mid_income',
    'Income_mid_income', 
    'drinking_category_drink_monthly',
    'drinking_category_drink_special', 
    'drinking_category_drink_weekly_1_2',
    'drinking_category_drink_weekly_3_4', 
    'drinking_category_never_drink',
    'Ethnicity_2.0', 
    'Ethnicity_3.0', 
    'Ethnicity_4.0', 
    'Ethnicity_6.0']]

disease_list = pd.DataFrame(death_dat['name_l1'].value_counts()).reset_index()
disease_list = disease_list[disease_list['count']>=30]
disease_list = disease_list.iloc[:, 0]

disease_count =pd.DataFrame(death_dat['name_l1'].value_counts()).reset_index()

# 并行执行
results = Parallel(n_jobs=16)(delayed(process_disease_penalizer)(d) for d in disease_list)

# 顺序执行

hr_results_df = pd.concat(results, ignore_index=True)
hr_results_df = pd.merge(hr_results_df, class_disease_dat, left_on='disease', right_on='name_l1')
hr_results_df.columns = ['group','HR', 'pvalue', 'CI_Lower', 'CI_Upper', 'Disease', 'best_penalizer', 'Class', 'level1', 'Class_id']

hr_results_df = hr_results_df[~hr_results_df['Disease'].isin(['Delivery'])]
hr_results_df_sort = hr_results_df.sort_values(['Class','Disease'], ascending=[True, False])
hr_results_df_sort.to_csv(outpath + 'DSS_HR_df.xls', sep='\t', index=False)


hf_df_sig = hr_results_df_sort[hr_results_df_sort['pvalue']<0.05]
hf_df_sig = pd.merge(hf_df_sig, disease_count, left_on='Disease', right_on='name_l1')
hf_df_sig_sort = hf_df_sig.sort_values(['Class','Disease'], ascending=[True, False])
hf_df_sig_sort.to_csv(outpath + 'DSS_HR_df_sig.xls', sep='\t', index=False)

sig_diseases = hf_df_sig_sort['Disease'].unique()

pdata = hr_results_df_sort[hr_results_df_sort['Disease'].isin(sig_diseases)]
pdata.to_csv(outpath + 'DSS_HR_sig_pdata.xls', sep='\t', index=False)

