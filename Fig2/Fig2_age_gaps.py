# -*- coding: utf-8 -*-
"""
Created on Fri Jul  5 02:02:55 2024

@author: wxm
"""


import numpy as np
import statsmodels.api as sm
import pandas as pd

# Assuming you have a list of predicted ages for each sample (predicted_ages),
# a list of chronological ages for each sample (chronological_ages),
# and a list of cohort labels for each sample (cohorts)

age_data_path = 'output\\3.clock\\changsha_clock\\elastic_net_scale\\merge_bj_cs\\predict_age_in_test_data.xlsx'

outpath = 'output\\3.clock\\changsha_clock\\elastic_net_scale\\merge_bj_cs\\'


# Step 1: Initialize arrays to store age gaps and z-scored age gaps
age_df_raw = pd.read_excel(age_data_path)
age_df = age_df_raw.sort_values(by='age')

predicted_ages = age_df['predict_age'].values
chronological_ages = age_df['age'].values

lowess_smoothing_fraction = 2 / 3
lowess_fit = sm.nonparametric.lowess(predicted_ages, chronological_ages, frac=lowess_smoothing_fraction)

lowess_val = lowess_fit[:, 1]

age_gaps = predicted_ages - lowess_val
z_scored_age_gaps = (age_gaps - np.mean(age_gaps)) / np.std(age_gaps)

age_df['lowess_val'] = lowess_val
age_df['age_gaps'] = age_gaps
age_df['z_scored_age_gaps'] = z_scored_age_gaps

age_df.to_excel(outpath + 'age_gaps.xlsx', index=False)