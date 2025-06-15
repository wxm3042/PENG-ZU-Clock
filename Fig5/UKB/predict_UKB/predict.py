# -*- coding: utf-8 -*-
"""
Created on Wed Apr  2 13:53:54 2025

@author: wxm
"""

import numpy as np
from sklearn.linear_model import ElasticNet
import pandas as pd
import joblib
from sklearn.impute import KNNImputer
import warnings
from sklearn.exceptions import ConvergenceWarning
import pickle
from sklearn.metrics import mean_absolute_error
from sklearn.model_selection import train_test_split, GridSearchCV, KFold
from sklearn.preprocessing import StandardScaler


np.random.seed(42)

common_id_path = 'V20250427\\data\\UKB_feature_id.csv'
UKB_data_path = 'V20250427\\output\\Fig5\\UKB\\predict_UKB\\UKB_data_clean.csv'
train_dat_path = 'V20250427\\output\\use_dat\\cs\\clock_data_normal.xlsx'

outpath = 'V20250427\\output\\Fig5\\UKB\\predict_UKB\\'

common_fea = pd.read_csv(common_id_path).dropna()['new_name']

# train data
cs_data = pd.read_excel(train_dat_path)
train_age = cs_data['Age']

train_data = cs_data[common_fea].drop(columns=['Age'])

imputer_knn = KNNImputer(n_neighbors=20)
train_data_knn = imputer_knn.fit_transform(train_data)

# 初始化StandardScaler
scaler = StandardScaler()
# 对数据进行标准化
train_data_scale = scaler.fit_transform(train_data_knn)

# 保存标准化参数
out_mean = scaler.mean_
out_scale = scaler.scale_

# test data
UKB_data = pd.read_csv(UKB_data_path)
test_age = UKB_data['Age']
test_data = UKB_data[train_data.columns]
test_data_scale = (test_data - out_mean) / out_scale


# 定义ElasticNet模型
elastic_net = ElasticNet()

# 设置 ElasticNet 参数候选值
param_grid = {
    #'alpha': [i * 0.1 for i in range(1, 10)],
    'alpha': [0.001, 0.005, 0.01, 0.05] + [i * 0.1 for i in range(1, 10)],
    #'l1_ratio': [0.5],
    'l1_ratio': [0.0, 0.5, 1.0]
}

# 设置交叉验证
kf = KFold(n_splits=5, shuffle=True, random_state=42)

# 使用GridSearchCV进行超参数搜索
grid_search = GridSearchCV(estimator=elastic_net, param_grid=param_grid, scoring='neg_mean_absolute_error', cv=kf)
grid_search.fit(train_data_scale, train_age)

# Get the best model and best hyperparameters
best_model = grid_search.best_estimator_
joblib.dump(best_model, outpath + 'best_model.joblib')

best_params = grid_search.best_params_

coefficients = best_model.coef_
coefficients_df = pd.DataFrame({'id': train_data.columns, 'Coefficient': coefficients})
coefficients_df_out = coefficients_df.sort_values(by='Coefficient', ascending=True)
coefficients_df_out.to_excel(outpath + 'elasticNet_coefficients.xlsx', index=False)

# predict
y_pred_valid = best_model.predict(test_data_scale)
pred_df = pd.DataFrame({'pt_id': UKB_data['eid'], 'Age': test_age, 'predict_age': y_pred_valid})
pred_df.to_excel(outpath + 'predict_age_in_test_data.xlsx', index=False)


