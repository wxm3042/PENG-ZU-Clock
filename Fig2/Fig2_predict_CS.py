# -*- coding: utf-8 -*-
"""
Created on Tue May 21 15:01:43 2024

@author: wxm
"""

import numpy as np
import pandas as pd
import joblib

from sklearn.impute import KNNImputer

np.random.seed(42)

common_id_path = 'output\\2.deg\\bj_cs_feature_common.xls'
model_path = 'output\\3.clock\\changsha_clock\\elastic_net_scale\\train_changsha_common_with_BJ\\best_model.joblib'
normal_data_path = 'output\\2.deg\\changsha\\changsha_normal_data_for_clock.xlsx'
changsha_data_path = 'output\\2.deg\\changsha\\changsha_all_data_for_clock.xlsx'
scale_param_path = 'output\\3.clock\\changsha_clock\\elastic_net_scale\\train_changsha_common_with_BJ\\scale_param.npz'

outpath = 'output\\3.clock\\changsha_clock\\elastic_net_scale\\test_changsha_disease\\'

scale_param = np.load(scale_param_path)

common_fea = pd.read_csv(common_id_path, delimiter = '\t')['fea']

used_pt = pd.read_excel(normal_data_path)['patient_id']
changsha_data = pd.read_excel(changsha_data_path)[common_fea]
changsha_data = changsha_data[~changsha_data['patient_id'].isin(used_pt)]

best_model = joblib.load(model_path)

X_all_raw = changsha_data.drop(['patient_id', 'gender', 'age'], axis=1)
X_all_scale = (X_all_raw - scale_param['out_mean']) / scale_param['out_scale']
imputer_knn = KNNImputer(n_neighbors=20)
X_all = imputer_knn.fit_transform(X_all_scale)


y_all = changsha_data['age'].values
pid_all = changsha_data['patient_id'].values

# predict
y_pred_valid = best_model.predict(X_all)
pred_df = pd.DataFrame({'pt_id': pid_all, 'age': y_all, 'predict_age': y_pred_valid})
pred_df.to_excel(outpath + 'predict_age_in_test_data.xlsx', index=False)






