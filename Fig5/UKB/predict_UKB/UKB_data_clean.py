# -*- coding: utf-8 -*-
"""
Created on Wed Apr  2 14:31:51 2025

@author: wxm
"""
import pandas as pd

dat_path = '..\\00database\\UKB\\output\\feature\\feature_value_add_eGFR.csv'
common_id_path = 'V20250427\\data\\UKB_feature_id.csv'

outpath = 'V20250427\\output\\Fig5\\UKB\\predict_UKB\\'

common_fea = pd.read_csv(common_id_path).dropna()['new_name'].to_list()

dat = pd.read_csv(dat_path)[['eid'] + common_fea]
dat_clean = dat.dropna()

dat_clean.to_csv(outpath + 'UKB_data_clean.csv', index=False)

