from mpl_toolkits.mplot3d import Axes3D
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt # plotting
import numpy as np # linear algebra
import os # accessing directory structure
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)

nRowsRead = 1000 # specify 'None' if want to read whole file
# abalone_original.csv has 4177 rows in reality, but we are only loading/previewing the first 1000 rows
df1 = pd.read_csv('abalone.csv', delimiter=',', nrows = nRowsRead)
df1.dataframeName = 'abalone.csv'
nRow, nCol = df1.shape
print(f'There are {nRow} rows and {nCol} columns')
print(df1.head(5))
pd.plotPerColumnDistribution(df1, 10, 5)