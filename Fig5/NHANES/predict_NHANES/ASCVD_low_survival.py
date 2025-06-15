# -*- coding: utf-8 -*-
"""
Created on Wed May 14 17:15:35 2025

@author: wxm
"""


from lifelines import KaplanMeierFitter
from lifelines.statistics import multivariate_logrank_test
import pandas as pd
import matplotlib.pyplot as plt
plt.rcParams['pdf.fonttype'] = 42
plt.rcParams['font.size'] = 12
plt.rcParams['ps.fonttype'] = 42
plt.rcParams['font.family'] = 'Times New Roman'

type_dat_path ='V20250427\\output\\Fig5\\NHANES\\predict_NHANES\\age_gaps_with_disease.xls'
disease_path = '..\\00database\\NHANES\\All_250516\\8.ASCVD\\ASCVD_score.csv'
death_dat_path = '..\\00database\\NHANES\\All\\6_mortality\\mortality.xlsx'
col_dir = {
    'Accelerated': '#B8281D',
    'Decelerated': '#3773B6',
    'Normal': '#BFBEBE'
    }
outpath = 'V20250427\\output\\Fig5\\NHANES\\predict_NHANES\\'

type_dat = pd.read_csv(type_dat_path, sep='\t')[['pt_id', 'age_gaps', 'type']]
type_dat.columns = ['eid', *type_dat.columns[1:]]
disease_dat = pd.read_csv(disease_path)[['pt_id', 'risk']]
disease_dat=disease_dat.rename(columns={'pt_id':'eid'})
death_dat = pd.read_excel(death_dat_path)
death_dat = death_dat.rename(columns={'seqn':'eid'})
death_dat['time'] = death_dat['time']/12

surv_dat = pd.merge(death_dat, type_dat, on='eid')
surv_dat = pd.merge(surv_dat, disease_dat, on='eid')
surv_dat['ASCVD_risk'] = 'mid'
surv_dat.loc[surv_dat['risk']<0.05, 'ASCVD_risk'] = 'low'
surv_dat.loc[surv_dat['risk']>=0.2, 'ASCVD_risk'] = 'high'
surv_dat.to_csv(outpath + 'ASCVD_score_data.csv')

plt.figure(figsize=(4,4))
kmf = KaplanMeierFitter()


for name, grouped_df in surv_dat.groupby('ASCVD_risk'):
    print(name)
    kmf.fit(grouped_df["time"], grouped_df["status"], label=name)
    kmf.plot_survival_function()

# 设置 x 轴和 y 轴标题
plt.xlabel('Time (Months)')  # 设置 x 轴标题
plt.ylabel('Survival Probability')  # 设置 y 轴标题

plt.savefig(outpath + 'ASCVD_survival.pdf')


# low ascvd

low_ascvd = surv_dat[surv_dat['ASCVD_risk']=='low']

plt.figure(figsize=(4,4))
kmf = KaplanMeierFitter()

for name, grouped_df in low_ascvd.groupby('type'):
    print(name)
    kmf.fit(grouped_df["time"], grouped_df["status"], label=name)
    kmf.plot_survival_function(color = col_dir[name])
results = multivariate_logrank_test(low_ascvd['time'], low_ascvd['type'], low_ascvd['status'])
p_value = results.p_value
plt.text(0.5, 0.1, f'Log-rank p-value: {p_value:.3f}', ha='center', va='center', transform=plt.gca().transAxes)
plt.xlabel('Time (years)')
plt.ylabel('OS')
plt.title('NHANES')

plt.savefig(outpath + 'ASCVD_low_survival.pdf')
plt.close()
