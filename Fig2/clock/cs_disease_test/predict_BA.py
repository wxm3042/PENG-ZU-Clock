# -*- coding: utf-8 -*-
"""
Created on Thu Mar 27 14:03:23 2025

@author: wxm
"""

import numpy as np
import pandas as pd
import joblib

from sklearn.impute import KNNImputer

np.random.seed(42)

model_path = 'V20250427\\output\\Fig2\\clock\\cs_normal_train\\best_model.joblib'
normal_data_path = 'V20250427\\output\\use_dat\\cs\\clock_data_normal_common_feature.xlsx'
changsha_data_path = 'V20250427\\output\\use_dat\\cs\\clock_data_common_feature.xlsx'
scale_param_path = 'V20250427\\output\\Fig2\\clock\\cs_normal_train\\scale_param.npz'
fea_ord_path = 'V20250427\\output\\Fig2\\clock\\cs_normal_train\\train_feature_order.csv'

outpath = 'V20250427\\output\\Fig2\\clock\\cs_disease_test\\'

train_fea= pd.read_csv(fea_ord_path)['feature']

scale_param = np.load(scale_param_path)

#common_fea = pd.read_csv(common_id_path, delimiter = '\t')['fea']

used_pt = pd.read_excel(normal_data_path)['pt_id']
changsha_data = pd.read_excel(changsha_data_path)
changsha_data = changsha_data[~changsha_data['pt_id'].isin(used_pt)]
#changsha_data.fillna(0, inplace=True)

best_model = joblib.load(model_path)

X_all_raw = changsha_data[train_fea]
X_mtx_gender = X_all_raw[['Gender']]
X_mtx_to_scale = X_all_raw.drop(columns=['Gender'])


X_all_scale = (X_mtx_to_scale - scale_param['out_mean']) / scale_param['out_scale']
X_all_scale['Gender'] = X_mtx_gender

imputer_knn = KNNImputer(n_neighbors=20)
X_all = imputer_knn.fit_transform(X_all_scale)


y_all = changsha_data['Age'].values
pid_all = changsha_data['pt_id'].values

# predict
y_pred_valid = best_model.predict(X_all)
pred_df = pd.DataFrame({'pt_id': pid_all, 'Age': y_all, 'predict_age': y_pred_valid})
pred_df.to_excel(outpath + 'predict_age_in_test_data.xlsx', index=False)





