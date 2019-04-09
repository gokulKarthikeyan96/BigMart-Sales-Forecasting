import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import sklearn.preprocessing
import sklearn.linear_model
import sklearn.ensemble
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error

data = pd.read_csv('Train.csv')


#print(data.Item_Fat_Content.value_counts())
#print(data.Item_Type.value_counts())
#print(data.Outlet_Size.value_counts())

data_missing = data.isnull().sum()
print(data_missing)

#Item weight (1463) and Outlet_size has missing values (2410)

twowaytable = pd.crosstab(data['Outlet_Size'], data['Outlet_Type'])
print(twowaytable)
