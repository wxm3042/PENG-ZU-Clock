# -*- coding: utf-8 -*-
"""
Created on Fri May 16 15:58:41 2025

@author: wxm
"""

import numpy as np
import statsmodels.api as sm
import pandas as pd

# Assuming you have a list of predicted ages for each sample (predicted_ages),
# a list of chronological ages for each sample (chronological_ages),
# and a list of cohort labels for each sample (cohorts)

age_data_path = 'V20250427\\output\\Fig5\\NHANES\\predict_NHANES\\c-index\\DNAm_age.xlsx'
NHANES_dat_path = 'V20250427\\output\\Fig5\\NHANES\\predict_NHANES\\NHANES_data_clean.csv'

outpath = 'V20250427\\output\\Fig5\\NHANES\\predict_NHANES\\c-index\\'

NHANES_dat = pd.read_csv(NHANES_dat_path)[['seqn', 'Age']]

# Step 1: Initialize arrays to store age gaps and z-scored age gaps
age_df_raw = pd.read_excel(age_data_path)
age_df_raw.rename(columns={'SEQN':'seqn'}, inplace=True)
age_df_raw_add_age = pd.merge(age_df_raw, NHANES_dat, on='seqn')
age_df = age_df_raw_add_age.sort_values(by='Age')

clock_list = age_df_raw.drop('seqn', axis=1).columns

age_gap_res = age_df[['seqn', 'Age']]
for c in clock_list:
    print(c)
    predicted_ages = age_df[c].values
    chronological_ages = age_df['Age'].values
    
    lowess_smoothing_fraction = 2 / 3
    lowess_fit = sm.nonparametric.lowess(predicted_ages, chronological_ages, frac=lowess_smoothing_fraction)
    
    lowess_val = lowess_fit[:, 1]
    
    age_gaps = predicted_ages - lowess_val
    z_scored_age_gaps = (age_gaps - np.mean(age_gaps)) / np.std(age_gaps)
    
    age_gap_res[c] = age_gaps
    
age_gap_res.to_excel(outpath + 'age_gaps_DNAm.xlsx', index=False)
    


