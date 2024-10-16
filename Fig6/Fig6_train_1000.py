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
np.random.seed(42)

##########
def run_iteration(i):
    # 划分数据集为训练集和测试集
    X_train, X_test, y_train, y_test, pid_train, pid_test = train_test_split(X_all, y_all, pid_all, test_size=0.5, random_state=i)

    # 定义ElasticNet模型
    elastic_net = ElasticNet()
    warnings.filterwarnings("ignore", category=ConvergenceWarning)

    # 使用GridSearchCV进行超参数搜索
    grid_search = GridSearchCV(estimator=elastic_net, param_grid=param_grid, scoring='neg_mean_absolute_error', cv=kf)
    grid_search.fit(X_train, y_train)

    # 获取最佳模型
    best_model = grid_search.best_estimator_

    # 获取系数
    coefficients = best_model.coef_
    return coefficients
###########

annot_path = 'data\\beijing\\副本北京医院2596例final原表_full-矫正.xlsx'
common_id_path = 'output\\2.deg\\bj_cs_feature_common.xls'
changsha_data_path = 'output\\2.deg\\changsha\\changsha_normal_data_for_clock.xlsx'
outpath = 'output/3.clock/changsha_clock/elastic_net_scale/simplify_clock/'

annot = pd.read_excel(annot_path, sheet_name=1).loc[:,['id','name']]

common_fea = pd.read_csv(common_id_path, decimal='\t')['fea']

changsha_data = pd.read_excel(changsha_data_path)[common_fea]

X_mtx = changsha_data.drop(['patient_id', 'gender', 'age'], axis=1)
# 初始化StandardScaler
scaler = StandardScaler()
# 对数据进行标准化
X_all_scale = pd.DataFrame(scaler.fit_transform(X_mtx), columns=X_mtx.columns)

imputer_knn = KNNImputer(n_neighbors=20)
X_all = imputer_knn.fit_transform(X_all_scale)
y_all = changsha_data['age'].values
pid_all = changsha_data['patient_id'].values

# 设置 ElasticNet 参数候选值
param_grid = {
    'alpha': [0.001, 0.005, 0.01, 0.05] + [i * 0.1 for i in range(1, 10)],
    'l1_ratio': [0.0, 0.5, 1.0]
}

# 设置交叉验证
kf = KFold(n_splits=5, shuffle=True, random_state=42)

# 并行执行1000次迭代，分批处理
batch_size = 100
num_batches = 1000 // batch_size
coefficients_list = []

for batch in range(num_batches):
    start_time = time.time()
    batch_results = Parallel(n_jobs=16)(delayed(run_iteration)(i + batch * batch_size) for i in tqdm(range(batch_size), desc=f"Batch {batch + 1}/{num_batches}"))
    coefficients_list.extend(batch_results)
    print(f"Batch {batch + 1}/{num_batches} completed in {time.time() - start_time:.2f} seconds")

# 将系数列表转换为DataFrame
coefficients_matrix = pd.DataFrame(coefficients_list, columns=changsha_data.drop(['patient_id', 'gender', 'age'], axis=1).columns)

# 将系数矩阵转换为布尔矩阵
nonzero_matrix = (coefficients_matrix != 0).astype(int)

# 统计每个指标在1000次迭代中出现非零系数的次数
indicator_counts = nonzero_matrix.sum(axis=0).sort_values(ascending=False)

# 将结果转换为DataFrame
result_df = pd.DataFrame(indicator_counts).reset_index()
result_df.columns = ['Indicator', 'Count']

result_df.to_excel(outpath + 'iter1000_fea_freq.xlsx', index=False)
