# -*- coding: utf-8 -*-
"""
Created on Tue Jan 21 10:26:15 2025

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

disease_path = '..\\00database\\UKB\\output\\disease\\ASCVD_score.csv'
type_dat_path ='V20250427\\output\\Fig5\\UKB\\predict_UKB\\age_gaps_with_disease.xls'
death_dat_path = '..\\00database\\UKB\\output\\disease250417\\death_data_annot.csv'
col_dir = {
    'Accelerated': '#B8281D',
    'Decelerated': '#3773B6',
    'Normal': '#BFBEBE'
    }
col_dir2 = {
    'high': '#B8281D',
    'low': '#3773B6',
    'mid': '#BFBEBE'
    }
outpath = 'V20250427\\output\\Fig6\\UKB\\prediabetes_ASCVD\\'

type_dat = pd.read_csv(type_dat_path, sep='\t')[['pt_id', 'age_gaps', 'type']]
type_dat = type_dat.rename(columns = {'type': 'type_raw', 'pt_id': 'eid'})
disease_dat = pd.read_csv(disease_path)[['eid', 'risk']]
death_dat = pd.read_csv(death_dat_path)

surv_dat = pd.merge(death_dat, type_dat, on='eid')
surv_dat = pd.merge(surv_dat, disease_dat, on='eid')
surv_dat['ASCVD_risk'] = 'mid'
surv_dat.loc[surv_dat['risk']<0.05, 'ASCVD_risk'] = 'low'
surv_dat.loc[surv_dat['risk']>=0.1, 'ASCVD_risk'] = 'high'

top_5_percent = surv_dat['age_gaps'].quantile(0.95)
bottom_5_percent = surv_dat['age_gaps'].quantile(0.05)
surv_dat['type'] = 'None'
surv_dat.loc[surv_dat['age_gaps']<=bottom_5_percent, 'type'] = 'slowdown'
surv_dat.loc[surv_dat['age_gaps']>=top_5_percent, 'type'] = 'accelerate'
surv_dat['time'] = surv_dat['time']/365

plt.figure(figsize=(4,4))
kmf = KaplanMeierFitter()

# kmf.fit(surv_dat["time"], surv_dat["status"], label='All')
# kmf.plot_survival_function()

for name, grouped_df in surv_dat.groupby('ASCVD_risk'):
    print(name)
    kmf.fit(grouped_df["time"], grouped_df["status"], label=name)
    kmf.plot_survival_function(color = col_dir2[name])
results = multivariate_logrank_test(surv_dat['time'], surv_dat['ASCVD_risk'], surv_dat['status'])
p_value = results.p_value
plt.text(0.5, 0.1, f'Log-rank p-value: {p_value:.3f}', ha='center', va='center', transform=plt.gca().transAxes)
plt.xlabel('Time (years)')
plt.ylabel('OS')
plt.title('UKB')
plt.savefig(outpath + 'ASCVD_survival.pdf')


low_ascvd = surv_dat[surv_dat['ASCVD_risk']=='low']


plt.figure(figsize=(4,4))
kmf = KaplanMeierFitter()

for name, grouped_df in low_ascvd.groupby('type_raw'):
    print(name)
    kmf.fit(grouped_df["time"], grouped_df["status"], label=name)
    kmf.plot_survival_function(color = col_dir[name])
results = multivariate_logrank_test(low_ascvd['time'], low_ascvd['type_raw'], low_ascvd['status'])
p_value = results.p_value
plt.text(0.5, 0.1, f'Log-rank p-value: {p_value:.3f}', ha='center', va='center', transform=plt.gca().transAxes)
plt.xlabel('Time (years)')
plt.ylabel('OS')
plt.title('UKB')
plt.savefig(outpath + 'ASCVD_low_survival.pdf')
plt.close()
