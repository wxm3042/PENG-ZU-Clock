# -*- coding: utf-8 -*-
"""
Created on Tue Apr 29 15:40:44 2025

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

fea_freq_path = 'V20250427\\output\\Fig9\\cs_normal_train\\feature_rank.xlsx'
cs_data_path = 'V20250427\\output\\use_dat\\cs\\clock_data_common_feature.xlsx'
bj_data_path = 'V20250427\\output\\use_dat\\bj\\clock_data_common_feature.xlsx'
train_dat_path = 'V20250427\\output\\use_dat\\cs\\clock_data_normal_common_feature.xlsx'

outpath = 'V20250427\\output\\Fig9\\cs_normal_train\\select_model\\'

fea_freq_raw = pd.read_excel(fea_freq_path)

# train data
changsha_data = pd.read_excel(train_dat_path)
X_mtx = changsha_data[fea_freq_raw['fea']]

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

# 划分数据集为训练集和测试集
X_train, X_test, y_train, y_test, pid_train, pid_test = train_test_split(X_all_scale, y_all, pid_all, test_size=0.5, random_state=42)

np.savetxt(outpath + 'train_pt_id.txt', pid_train, delimiter='\t', fmt='%s')

# test data
# bj
bj_data = pd.read_excel(bj_data_path)
bj_pt_id = bj_data['pt_id'].to_list()
bj_age = bj_data['Age'].to_list()
bj_test_data = bj_data[fea_freq_raw['fea']]
# 填充
imputer_knn = KNNImputer(n_neighbors=20)
bj_filled = imputer_knn.fit_transform(bj_test_data) 
bj_filled_df = pd.DataFrame(bj_filled, columns=bj_test_data.columns)
bj_test_data_scale = (bj_filled_df - out_mean) / out_scale

# cs
cs_data = pd.read_excel(cs_data_path)
cs_data_filter = cs_data[~cs_data['pt_id'].isin(pid_train)]
cs_pt_id = cs_data_filter['pt_id'].to_list()
cs_age = cs_data_filter['Age'].to_list()
cs_test_data = cs_data_filter[fea_freq_raw['fea']]
# 填充
imputer_knn = KNNImputer(n_neighbors=20)
cs_filled = imputer_knn.fit_transform(cs_test_data) 
cs_filled_df = pd.DataFrame(cs_filled, columns=cs_test_data.columns)
cs_test_data_scale = (cs_filled_df - out_mean) / out_scale



# 设置交叉验证
kf = KFold(n_splits=10, shuffle=True, random_state=42)
# 设置 ElasticNet 参数候选值
param_grid = {
    'alpha': [0.001, 0.005, 0.01, 0.05] + [i * 0.1 for i in range(1, 11)],
    'l1_ratio': [i * 0.1 for i in range(1, 11)]
}

# 定义ElasticNet模型
elastic_net = ElasticNet()
warnings.filterwarnings("ignore", category=ConvergenceWarning)

# 使用GridSearchCV进行超参数搜索
grid_search = GridSearchCV(estimator=elastic_net, param_grid=param_grid, scoring='neg_mean_absolute_error', cv=kf)

use_fea_dir = {}
#cs_predict_age_dir = {}
#bj_predict_age_dir = {}
predict_age_dir = {}
cor_dir = {}
mae_dir = {}
predict_age_dir['sample_id'] = cs_pt_id + bj_pt_id
predict_age_dir['Age'] = cs_age + bj_age
'''
cs_predict_age_dir['sample_id'] = cs_pt_id
cs_predict_age_dir['Age'] = cs_age

bj_predict_age_dir['sample_id'] = bj_pt_id
bj_predict_age_dir['Age'] = bj_age
'''
for i in range(1, fea_freq_raw.shape[0]+1):
    print(i)
    sl_train = fea_freq_raw.head(i)['fea'].tolist()
    # 训练集 子集
    X_train_subset = X_train[sl_train]

    # 使用GridSearchCV进行超参数搜索
    grid_search = GridSearchCV(estimator=elastic_net, param_grid=param_grid, scoring='neg_mean_absolute_error',
                               cv=kf)
    grid_search.fit(X_train_subset, y_train)

    # Get the best model and best hyperparameters
    best_model = grid_search.best_estimator_

    joblib.dump(best_model, outpath + 'fea_num' + str(i) +  '_best_model.joblib')

    best_params = grid_search.best_params_

    use_fea_dir['fea_num' + str(i)] = sl_train
    
    # predict cs
    cs_test_subset = cs_test_data_scale[sl_train]
    cs_pred_valid = best_model.predict(cs_test_subset)
    #cs_predict_age_dir['num'+str(i)] = cs_pred_valid

    # predict bj
    bj_test_subset = bj_test_data_scale[sl_train]
    bj_pred_valid = best_model.predict(bj_test_subset)
    #bj_predict_age_dir['num'+str(i)] = bj_pred_valid
    
    predict_age_dir['num'+str(i)] = np.concatenate([cs_pred_valid, bj_pred_valid])

    correlation_matrix = np.corrcoef(predict_age_dir['Age'], predict_age_dir['num'+str(i)])
    pearson_corr = correlation_matrix[0, 1]
    cor_dir['num'+str(i)] = pearson_corr
    mae_dir['num'+str(i)] = mean_absolute_error(predict_age_dir['Age'], predict_age_dir['num'+str(i)])


# 指定保存文件的路径
file_path = outpath + 'use_fea_dir.pkl'

# 使用 pickle 序列化并保存字典到文件
with open(file_path, 'wb') as f:
    pickle.dump(use_fea_dir, f)

mae_res = pd.DataFrame(list(mae_dir.items()), columns=['model_name', 'MAE'])

predict_age = pd.DataFrame(predict_age_dir)
predict_age.to_excel(outpath + 'predict_age.xlsx', index=False)

cor_res  = pd.DataFrame(list(cor_dir.items()), columns=['model_name', 'correlation'])
cor_res['fea'] = fea_freq_raw['fea']

cor_res_mae = pd.merge(cor_res, mae_res, on='model_name')
cor_res_mae.to_excel(outpath + 'cor_res.xlsx', index=False)
