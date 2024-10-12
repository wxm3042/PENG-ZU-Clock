# -*- coding: utf-8 -*-
"""
Created on Tue Jun 25 00:21:21 2024

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

annot_path = 'data\\beijing\\副本北京医院2596例final原表_full-矫正.xlsx'
common_id_path = 'output\\2.deg\\bj_cs_feature_common.xls'
changsha_data_path = 'output\\2.deg\\changsha\\changsha_normal_data_for_clock.xlsx'
outpath = 'output\\3.clock\\changsha_clock\\elastic_net_scale\\'

annot = pd.read_excel(annot_path, sheet_name=1).loc[:,['id','name']]

common_fea = pd.read_csv(common_id_path, decimal='\t')['fea']

changsha_data = pd.read_excel(changsha_data_path)[common_fea]
#changsha_data.drop(columns=['m43-526','m20-503'], inplace=True)

# changsha_data.fillna(0, inplace=True)

distribution_summary_cs = changsha_data.describe()
distribution_summary_cs.to_excel(outpath + 'train_changsha_common_with_BJ//distribution_summary_cs.xlsx')

X_mtx = changsha_data.drop(['patient_id', 'gender', 'age'], axis=1)
# 初始化StandardScaler
scaler = StandardScaler()
# 对数据进行标准化
X_all_scale = pd.DataFrame(scaler.fit_transform(X_mtx), columns=X_mtx.columns)

imputer_knn = KNNImputer(n_neighbors=20)
X_all = imputer_knn.fit_transform(X_all_scale)

#X_all = changsha_data.drop(['patient_id', 'gender', 'age'], axis=1).values

# 保存标准化参数
out_mean = scaler.mean_
out_scale = scaler.scale_

# 保存多个数组到文件
np.savez(outpath + 'train_changsha_common_with_BJ//scale_param.npz', out_mean=out_mean, out_scale=out_scale)
y_all = changsha_data['age'].values
pid_all = changsha_data['patient_id'].values

# 定义ElasticNet模型
elastic_net = ElasticNet()

# 划分数据集为训练集和测试集
X_train, X_test, y_train, y_test, pid_train, pid_test = train_test_split(X_all, y_all, pid_all, test_size=0.5, random_state=42)

np.savetxt(outpath + 'train_changsha_common_with_BJ//train_pt_id.txt', pid_train, delimiter='\t', fmt='%s')

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
joblib.dump(best_model, outpath + 'train_changsha_common_with_BJ//best_model.joblib')

best_params = grid_search.best_params_

coefficients = best_model.coef_
coefficients_df = pd.DataFrame({'id': changsha_data.drop(['patient_id', 'gender', 'age'], axis=1).columns, 'Coefficient': coefficients})
coefficients_df_out = pd.merge(coefficients_df, annot, on='id').sort_values(by='Coefficient', ascending=True)
coefficients_df_out.to_excel(outpath + 'train_changsha_common_with_BJ//elasticNet_coefficients.xlsx', index=False)


# predict
y_pred_valid = best_model.predict(X_test)
pred_df = pd.DataFrame({'pt_id': pid_test, 'age': y_test, 'predict_age': y_pred_valid})
pred_df.to_excel(outpath + 'test_changsha_normal//predict_age_in_test_data.xlsx', index=False)

