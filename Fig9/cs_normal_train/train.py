# -*- coding: utf-8 -*-
"""
Created on Fri Apr 25 12:27:24 2025

@author: wxm
"""


import numpy as np
from sklearn.model_selection import train_test_split, GridSearchCV, KFold
from sklearn.linear_model import ElasticNet
import pandas as pd
import joblib
from sklearn.preprocessing import StandardScaler
from sklearn.impute import KNNImputer

np.random.seed(42)

changsha_data_path = 'V20250327\\output\\use_dat\\cs\\clock_normal_data_common_feature.xlsx'
rank_path = 'V20250327\\output\\Fig9\\cs_normal_train\\iter1000_fea_freq.xlsx'

outpath = 'V20250327\\output\\Fig9\\cs_normal_train\\'

fea_use = pd.read_excel(rank_path)
fea_list = fea_use['id'][fea_use['Count']>950]

fea_list.to_csv(outpath + 'train_fea.csv')

changsha_data = pd.read_excel(changsha_data_path)

X_mtx = changsha_data[fea_list]

# 填充
imputer_knn = KNNImputer(n_neighbors=20)
X_filled = imputer_knn.fit_transform(X_mtx) 

# 标准化
# 初始化StandardScaler
scaler = StandardScaler()
# 对数据进行标准化
X_all_scale = pd.DataFrame(scaler.fit_transform(X_filled), columns=X_mtx.columns)

# 保存标准化参数
out_mean = scaler.mean_
out_scale = scaler.scale_

# 保存多个数组到文件
np.savez(outpath + 'scale_param.npz', out_mean=out_mean, out_scale=out_scale)
y_all = changsha_data['Age'].values
pid_all = changsha_data['pt_id'].values

# 定义ElasticNet模型
elastic_net = ElasticNet()

# 划分数据集为训练集和测试集
X_train, X_test, y_train, y_test, pid_train, pid_test = train_test_split(X_all_scale, y_all, pid_all, test_size=0.5, random_state=42)

np.savetxt(outpath + 'train_pt_id.txt', pid_train, delimiter='\t', fmt='%s')

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
grid_search.fit(X_train, y_train)

# Get the best model and best hyperparameters
best_model = grid_search.best_estimator_
joblib.dump(best_model, outpath + 'best_model.joblib')

best_params = grid_search.best_params_

coefficients = best_model.coef_
coefficients_df = pd.DataFrame({'id': X_mtx.columns, 'Coefficient': coefficients})
coefficients_df_sorted = coefficients_df.sort_values(by='Coefficient')
coefficients_df_sorted.to_excel(outpath + 'elasticNet_coefficients.xlsx', index=False)


# predict
y_pred_valid = best_model.predict(X_test)
pred_df = pd.DataFrame({'pt_id': pid_test, 'age': y_test, 'predict_age': y_pred_valid})
pred_df.to_excel(outpath + '..//cs_normal_test//predict_age_in_test_data.xlsx', index=False)


