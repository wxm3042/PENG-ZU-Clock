# -*- coding: utf-8 -*-
"""
Created on Mon May 12 10:08:29 2025

@author: wxm
"""

import pandas as pd

NHANES_data_path = '..\\00database\\NHANES\\All_250516\\adult_data_from_single_year_merge.xlsx'
common_id_path = 'V20250427\\output\\Fig5\\NHANES\\predict_NHANES\\keep_fea.xlsx'

outpath = 'V20250427\\output\\Fig5\\NHANES\\predict_NHANES\\'

common_fea_df = pd.read_excel(common_id_path)
common_fea = common_fea_df[common_fea_df['keep']=='y']['fea'].to_list()

dat = pd.read_excel(NHANES_data_path)[['seqn'] + common_fea]
dat_clean = dat.dropna()

dat_clean.to_csv(outpath + 'NHANES_data_clean.csv', index=False)



