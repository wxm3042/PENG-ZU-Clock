# -*- coding: utf-8 -*-
"""
Created on Wed May  7 09:59:50 2025

@author: wxm
"""

import pandas as pd

dat_path = '..\\00database\\UKB\\output\\feature\\feature_value_add_eGFR.csv'
common_id_path = 'V20250427\\output\\use_dat\\common_fea_for_correlation.xlsx'

outpath = 'V20250427\\output\\Fig8\\'

dat_raw = pd.read_csv(dat_path)

common_fea_raw = pd.read_excel(common_id_path)['fea'].to_list()
common_fea = set(common_fea_raw) & set(dat_raw.columns)
dat = dat_raw[['eid'] + list(common_fea)]
dat_clean = dat.dropna()

dat_clean.to_csv(outpath + 'UKB_data_cor_feature.csv', index=False)



