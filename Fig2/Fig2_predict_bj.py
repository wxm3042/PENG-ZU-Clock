# -*- coding: utf-8 -*-
"""
Created on Wed May 22 08:57:11 2024

@author: wxm
"""

import numpy as np
import pandas as pd
import joblib
from sklearn.impute import KNNImputer

np.random.seed(42)

common_id_path = 'output\\2.deg\\bj_cs_feature_common.xls'
model_path = 'output\\3.clock\\changsha_clock\\elastic_net_scale\\train_changsha_common_with_BJ\\best_model.joblib'
train_dat_path = 'output\\3.clock\\changsha_clock\\elastic_net_scale\\train_changsha_common_with_BJ\\train_pt_id.txt'
bj_data_path = 'output\\2.deg\\beijing\\beijing_data_for_clock_part1.xlsx'

scale_param_path = 'output\\3.clock\\changsha_clock\\elastic_net_scale\\train_changsha_common_with_BJ\\scale_param.npz'

outpath = 'output\\3.clock\\changsha_clock\\elastic_net_scale\\test_beijing\\'

scale_param = np.load(scale_param_path)

common_fea = pd.read_csv(common_id_path, decimal='\t')['fea']

used_pt = pd.read_csv(train_dat_path, header=None, delimiter='\t').iloc[:,0]
bj_data = pd.read_excel(bj_data_path)[common_fea]
bj_data = bj_data[~bj_data['patient_id'].isin(used_pt)]

distribution_summary_cs = bj_data.describe()
distribution_summary_cs.to_excel(outpath + 'distribution_summary_bj.xlsx')


best_model = joblib.load(model_path)

X_all_raw = bj_data.drop(['patient_id', 'gender', 'age'], axis=1)
X_all_scale = (X_all_raw - scale_param['out_mean']) / scale_param['out_scale']
imputer_knn = KNNImputer(n_neighbors=20)
X_all = imputer_knn.fit_transform(X_all_scale)


y_all = bj_data['age'].values
pid_all = bj_data['patient_id'].values

# predict
y_pred_valid = best_model.predict(X_all)
pred_df = pd.DataFrame({'pt_id': pid_all, 'age': y_all, 'predict_age': y_pred_valid})
pred_df.to_excel(outpath + 'predict_age_in_test_data.xlsx', index=False)
