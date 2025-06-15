# -*- coding: utf-8 -*-
"""
Created on Tue May 27 00:31:18 2025

@author: wxm
"""


import numpy as np
from sklearn.model_selection import train_test_split, GridSearchCV, KFold
from sklearn.model_selection import train_test_split, GridSearchCV, KFold
from sklearn.linear_model import LinearRegression, Lasso, ElasticNet
from sklearn.ensemble import RandomForestRegressor
from sklearn.svm import SVR
from lightgbm import LGBMRegressor
import pandas as pd
import joblib
from sklearn.preprocessing import StandardScaler
from sklearn.impute import KNNImputer

np.random.seed(42)

model_path_dir = {
    'ElasticNet' : 'V20250427\\output\\Fig2\\clock\\algorithm_select\\cs_normal_train\\best_ElasticNet_model.joblib',
    'LASSO' : 'V20250427\\output\\Fig2\\clock\\algorithm_select\\cs_normal_train\\best_LASSO_model.joblib',
    'LightGBM' : 'V20250427\\output\\Fig2\\clock\\algorithm_select\\cs_normal_train\\best_LightGBM_model.joblib',
    'LinearRegression' : 'V20250427\\output\\Fig2\\clock\\algorithm_select\\cs_normal_train\\best_LinearRegression_model.joblib',
    'RandomForest' : 'V20250427\\output\\Fig2\\clock\\algorithm_select\\cs_normal_train\\best_RandomForest_model.joblib',
    'SVR' : 'V20250427\\output\\Fig2\\clock\\algorithm_select\\cs_normal_train\\best_SVR_model.joblib'
    }
bj_data_path = 'V20250427\\output\\use_dat\\bj\\clock_data_common_feature.xlsx'

scale_param_path = 'V20250427\\output\\Fig2\\clock\\algorithm_select\\cs_normal_train\\scale_param.npz'

outpath = 'v20250427\\output\\Fig2\\clock\\algorithm_select\\bj_test\\'

scale_param = np.load(scale_param_path)

bj_data = pd.read_excel(bj_data_path)

for n,m in model_path_dir.items():
    print(n)
    print(m)
    
    best_model = joblib.load(m)
    
    X_all_raw = bj_data.drop(['pt_id', 'Age'], axis=1)
    X_mtx_gender = X_all_raw[['Gender']]
    X_mtx_to_scale = X_all_raw.drop(columns=['Gender'])

    X_all_scale = (X_mtx_to_scale - scale_param['out_mean']) / scale_param['out_scale']
    X_all_scale['Gender'] = X_mtx_gender
    imputer_knn = KNNImputer(n_neighbors=20)
    X_all = imputer_knn.fit_transform(X_all_scale)
    
    
    y_all = bj_data['Age'].values
    pid_all = bj_data['pt_id'].values
    
    # predict
    y_pred_valid = best_model.predict(X_all)
    pred_df = pd.DataFrame({'pt_id': pid_all, 'age': y_all, 'predict_age': y_pred_valid})
    pred_df.to_excel(outpath + n +'_predict_age_in_test_data.xlsx', index=False)
