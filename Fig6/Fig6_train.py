# -*- coding: utf-8 -*-
"""
Created on Sun Jul 14 17:43:35 2024

@author: wxm
"""
import numpy as np
from sklearn.model_selection import train_test_split, GridSearchCV, KFold
from sklearn.linear_model import ElasticNet
import pandas as pd
import joblib
from sklearn.preprocessing import StandardScaler
from sklearn.impute import KNNImputer
from collections import Counter
from joblib import Parallel, delayed
from tqdm import tqdm
import time
import warnings
from sklearn.exceptions import ConvergenceWarning
import itertools
import pickle
np.random.seed(42)

common_id_path = 'output\\2.deg\\bj_cs_feature_common.xls'
changsha_data_path = 'output\\2.deg\\changsha\\changsha_normal_data_for_clock.xlsx'
fea_freq_path = 'output/3.clock/changsha_clock/elastic_net_scale/simplify_clock/iter1000_fea_freq.xlsx'

outpath = 'output/3.clock/changsha_clock/elastic_net_scale/simplify_clock/cs_train/'

common_fea = pd.read_csv(common_id_path, decimal='\t')['fea']
changsha_data = pd.read_excel(changsha_data_path)[common_fea]
fea_freq = pd.read_excel(fea_freq_path)

# feature list#######################
# 1:7
# 假设列表
items = fea_freq.head(7)['Indicator']

# 生成所有可能的排列组合，从包含一个元素到包含所有元素的组合，且元素不重复
all_combinations = []
for r in range(1, len(items) + 1):
    print(r)
    combinations = [list(comb) for comb in itertools.combinations(items, r)]
    all_combinations.extend(combinations)

# 8-9
items2 = fea_freq.iloc[[7, 8], :]['Indicator']

# 生成所有可能的排列组合，从包含一个元素到包含所有元素的组合，且元素不重复
all_combinations2 = []
for r in range(1, len(items2) + 1):
    print(r)
    combinations = [list(comb) + all_combinations[-1] for comb in itertools.combinations(items2, r)]
    all_combinations2.extend(combinations)

comb1_9 = all_combinations + all_combinations2

#######################


X_mtx = changsha_data.drop(['patient_id', 'gender', 'age'], axis=1)
# 初始化StandardScaler
scaler = StandardScaler()
# 对数据进行标准化
X_all_scale = pd.DataFrame(scaler.fit_transform(X_mtx), columns=X_mtx.columns)

imputer_knn = KNNImputer(n_neighbors=20)
X_all = imputer_knn.fit_transform(X_all_scale)
# 保存标准化参数
out_mean = scaler.mean_
out_scale = scaler.scale_

# 保存多个数组到文件
np.savez(outpath + 'scale_param.npz', out_mean=out_mean, out_scale=out_scale)

y_all = changsha_data['age'].values
pid_all = changsha_data['patient_id'].values

# 设置 ElasticNet 参数候选值
param_grid = {
    'alpha': [0.001, 0.005, 0.01, 0.05] + [i * 0.1 for i in range(1, 10)],
    'l1_ratio': [0.0, 0.5, 1.0]
}

# 设置交叉验证
kf = KFold(n_splits=5, shuffle=True, random_state=42)

X_train, X_test, y_train, y_test, pid_train, pid_test = train_test_split(X_all, y_all, pid_all, test_size=0.5, random_state=12345)

np.savetxt(outpath + 'train_pt_id.txt', pid_train, delimiter='\t', fmt='%s')

# 定义ElasticNet模型
elastic_net = ElasticNet()
warnings.filterwarnings("ignore", category=ConvergenceWarning)

# 使用GridSearchCV进行超参数搜索
grid_search = GridSearchCV(estimator=elastic_net, param_grid=param_grid, scoring='neg_mean_absolute_error', cv=kf)


#######################

use_fea_dir = {}

for i in range(fea_freq.shape[0] + 1):
    n=1
    print(i)
    if(i <10):
        sub_list = [ss for ss in comb1_9 if len(ss) == i]
        for sl in sub_list:
            print (sl)
            column_indices = [X_all_scale.columns.get_loc(column) for column in sl]

            # 训练集 子集
            X_train_subset = X_train[:, column_indices]

            # 使用GridSearchCV进行超参数搜索
            grid_search = GridSearchCV(estimator=elastic_net, param_grid=param_grid, scoring='neg_mean_absolute_error', cv=kf)
            grid_search.fit(X_train_subset, y_train)

            # Get the best model and best hyperparameters
            best_model = grid_search.best_estimator_

            joblib.dump(best_model, outpath + 'fea_num' + str(i) + '_' + str(n) + '_best_model.joblib')

            best_params = grid_search.best_params_

            use_fea_dir['fea_num' + str(i) + '_' + str(n)] = sl
            n = n + 1
    else:
        sl = fea_freq.head(i)['Indicator'].tolist()

        print(sl)
        column_indices = [X_all_scale.columns.get_loc(column) for column in sl]

        # 训练集 子集
        X_train_subset = X_train[:, column_indices]

        # 使用GridSearchCV进行超参数搜索
        grid_search = GridSearchCV(estimator=elastic_net, param_grid=param_grid, scoring='neg_mean_absolute_error',
                                   cv=kf)
        grid_search.fit(X_train_subset, y_train)

        # Get the best model and best hyperparameters
        best_model = grid_search.best_estimator_

        joblib.dump(best_model, outpath + 'fea_num' + str(i) + '_' + str(n) + '_best_model.joblib')

        best_params = grid_search.best_params_

        use_fea_dir['fea_num' + str(i) + '_' + str(n)] = sl

# 指定保存文件的路径
file_path = outpath + 'use_fea_dir.pkl'

# 使用 pickle 序列化并保存字典到文件
with open(file_path, 'wb') as f:
    pickle.dump(use_fea_dir, f)


