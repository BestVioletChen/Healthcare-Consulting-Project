import numpy as np
import pandas as pd
import os
from scipy.io import loadmat
import re
from scipy import stats
import random
import itertools
import torch
from torch import nn

def read_data(path_list):
    b={};z={}
    for i in path_list:
        Id=re.findall('\d{6}_\d{3}', i)[0]
        Type= re.findall('[a-zA-Z_]*(?=.mat)',i)[0]
        if Type=='binned_behavior':
            b[Id]=loadmat(i)[Type]
        if Type=='binned_zscore':
            z[Id]=loadmat(i)[Type]
    return b,z

def convert_y(y):
    if y.shape[1]!=2:
        y=y.T
    y_new=np.zeros(y.shape[0])
    condlist=[(y[:,0]==0) & (y[:,1]==0),(y[:,0]==1) & (y[:,1]==0),(y[:,0]==0) & (y[:,1]==1),(y[:,0]==1) & (y[:,1]==1)]
    y_new[condlist[1]]=1
    y_new[condlist[2]]=2
    y_new[condlist[3]]=3
    y_new=y_new.reshape(y.shape[0],-1)
    return y_new

def transfer_score(d):
    d_new={}
    quantile=np.linspace(0.05, 0.95, 19, endpoint=True)
    for i in d.keys():
        d_new[i]=np.vstack((np.var(d[i],axis=1),
               np.std(d[i],axis=1),
               np.max(d[i],axis=1),
               np.min(d[i],axis=1),
               np.mean(d[i],axis=1),
               stats.skew(d[i],axis=1),
               stats.kurtosis(d[i],axis=1),
               np.percentile(d[i], [75 ,25],axis=1),
               np.quantile(d[i], quantile, axis=1)
               )).T
    return d_new

class Mydata:
    def __init__(self,Dir):
        self.Dir=Dir
        #self.get_all_files=get_all_files
        self.read_data=read_data
        self.convert_y=convert_y
        self.transfer_score=transfer_score
    
    def process(self):
        all_file_full_path_list = []
        all_file_name_list = []
        def get_all_files(path):
            """
            reference:
                https://blog.csdn.net/xiejunna/article/details/119993813?spm=1001.2101.3001.6650.16&utm_medium=distribute.pc_relevant.none-task-blog-2%7Edefault%7EBlogCommendFromBaidu%7ERate-16.pc_relevant_aa&depth_1-utm_source=distribute.pc_relevant.none-task-blog-2%7Edefault%7EBlogCommendFromBaidu%7ERate-16.pc_relevant_aa&utm_relevant_index=22
            获取指定路径下多层目录内的所有文件全路径及文件名称
            :param path: 需获取文件的指定路径
            :return: 结果1 类型：list<str> ：多层目录下的，全部文件全路径；结果2 类型：list<str> ：多层目录下的，全部文件名称
            """
            all_file_list = os.listdir(path)
            # 遍历该文件夹下的所有目录或文件
            for file in all_file_list:
                file_path = os.path.join(path, file)
                # 如果是文件夹，递归调用当前函数
                if os.path.isdir(file_path):
                    get_all_files(file_path)
                # 如果不是文件夹，保存文件路径及文件名
                elif os.path.isfile(file_path):
                    all_file_full_path_list.append(file_path)
                    all_file_name_list.append(file)
            return all_file_full_path_list, all_file_name_list  
        self.file,_= get_all_files(self.Dir)
        return self.file
    def get_data(self,method='quantile'):
        self.b,self.z=self.read_data(self.file)
        self.b_new={};self.z_new={}
        for i in self.b.keys():
            self.b_new[i]=self.convert_y(self.b[i])
        if method=='quantile':
            self.z_new=self.transfer_score(self.z)
        if method=='origion':
            self.z_new=self.z 
        '''finally concate zcore and behavior together'''
        self.merge_dic={}
        for key in self.z_new.keys():
            self.merge_dic[key]=np.concatenate((self.z_new[key],self.b_new[key]),axis=1)     
        return self.merge_dic
        #return self.z_new,self.b_new
    
 
    
def sample(data,key,gap=30,lag=1):
    #mydic_adj={}
    l=[]
    for key in key:       
        #print(key)
        '''first step change lag order of X and Y'''
        if lag>=1:
        #mydic_adj[key]=np.concatenate((mydic[key][:-lag,:-1],mydic[key][lag:,[-1]]),axis=1)
            tmp=np.concatenate((data[key][:-lag,:-1],data[key][lag:,[-1]]),axis=1) 
        if lag==0:
            tmp=data[key]
            
        for i in range(0,tmp.shape[0]-gap+1):
            l.append(tmp[i:i+gap,:])
    return np.array(l)

def get_test(data):
    X = data[:,:,:-1]
    Y = data[:,-1,-1]
    return torch.tensor(X),torch.tensor(Y)

def split_train_test(data):
    p=data.shape[0]//4
    train=data[:3*p,:,:]
    test=data[3*p:,:,:]
    return train,test
    

def mice_seq_iter_random(corpus, batch_size=128, random_state=82): 
    random.seed(random_state)
    tmp_array=np.array([i for i in range(0,corpus.shape[0])])
    random.shuffle(tmp_array)  
    initial_indices = list(range(0, corpus.shape[0], batch_size))
    #random.shuffle(initial_indices)
    for i in  initial_indices:
        initial_indices_per_batch = tmp_array[i: i + batch_size]
        if initial_indices_per_batch.shape[0]<=16:
            continue
        X = corpus[initial_indices_per_batch,:,:-1]
        Y = corpus[initial_indices_per_batch,-1,-1]
        yield torch.tensor(X), torch.tensor(Y)
    

class mice_loader():
    def __init__(self,corpus, batch_size, random_state):
        self.data_iter_fn=mice_seq_iter_random
        self.corpus=corpus
        self.batch_size=batch_size
        self.random_state=random_state
    def __iter__(self):
        # 此处返回啥无所谓
        return self.data_iter_fn(self.corpus, self.batch_size, self.random_state)
    

