#!/usr/bin/env python
# coding: utf-8

# # Appendix
# # Q1:

# In[1]:


import numpy as np
import math
import random
from matplotlib import pyplot as plt
from IPython.display import clear_output


# In[2]:


def get_rand_number(min_value, max_value):
    range = max_value - min_value
    choice = random.uniform(0,1)
    return min_value + range*choice


# In[3]:


def f(x):
    return (3*x+x**2-200*math.cos(x))


# In[4]:


def crude_monte_carlo(num_samples=10000):
    lower_bound = 1
    upper_bound = 8
    
    sum_of_samples = 0
    X = []
    for i in range(num_samples):
        x = get_rand_number(lower_bound, upper_bound)
        sum_of_samples += f(x)
        X.append(f(x)*7)
    
    monte_carlo = (upper_bound - lower_bound) * float(sum_of_samples/num_samples)
    
    return monte_carlo, X


# In[5]:


groud_truth = 235.26
MC_score, X = crude_monte_carlo()
low = np.mean(X)-1.96*np.std(X)/math.sqrt(10000)
up = np.mean(X)+1.96*np.std(X)/math.sqrt(10000)
print('Result is:', MC_score, '\nWith confidence interval [', low, ',', up, ']')


# # Q3:
# 
# (a)

# In[1]:


import pandas as pd
from sklearn import preprocessing


# In[2]:


def LabelEncode(data):
    le = preprocessing.LabelEncoder()
    le.fit(data)
    return le.transform(data)


# In[3]:


df = pd.read_csv('Hitters.csv')
df['League'] = LabelEncode(df['League'])
df['Division'] = LabelEncode(df['Division'])
df['NewLeague'] = LabelEncode(df['NewLeague'])
df.info()


# In[7]:


df.head()


# (b)

# In[5]:


from sklearn.linear_model import LinearRegression
from sklearn.model_selection import cross_val_score
y = df.iloc[:, -2]
X = df.drop(columns=['Salary'])
lm = LinearRegression().fit(X, y)


# In[6]:


score = -1*cross_val_score(lm, X, y, cv=10, scoring='neg_mean_squared_error')   
score.mean()


# In[7]:


score


# (c)

# In[11]:


from sklearn.decomposition import PCA
from sklearn import preprocessing
import matplotlib.pyplot as plt

df_scale = pd.read_csv('Hitters.csv')
df_scale['League'] = LabelEncode(df['League'])
df_scale['Division'] = LabelEncode(df['Division'])
df_scale['NewLeague'] = LabelEncode(df['NewLeague'])

y = df_scale['Salary']
X_scale = df_scale.drop(['Salary'], axis=1)

X_scale


# In[12]:


from sklearn.preprocessing import scale 
pca = PCA()
X_scale = pca.fit_transform(scale(X_scale))


# In[13]:


n = len(X_scale)

lm_scale = LinearRegression()
mse = []

score = -1*cross_val_score(lm_scale, X_scale, y, cv=10, scoring='neg_mean_squared_error').mean()    

for i in range(1, 20):
    score = -1*cross_val_score(lm_scale, X_scale[:,:i], y.ravel(), cv=10, scoring='neg_mean_squared_error').mean()
    mse.append(score)
    print('MSE with', i, 'components:', score)
    
plt.plot(range(1,20), mse)
plt.xlabel('Number of principal components in regression')
plt.ylabel('MSE')
plt.title('Salary');


# (d)

# In[14]:


from sklearn import linear_model
alphas = np.linspace(0, 500, num=100)

lasso_mse = []
for alpha in alphas:
    lasso = linear_model.Lasso(alpha=alpha, max_iter=10000000)
    score = -1*cross_val_score(lasso, X, y, cv=10, scoring='neg_mean_squared_error').mean()
    lasso_mse.append(score)
    
plt.plot(alphas, lasso_mse)


# In[15]:


from sklearn import linear_model
alphas = np.linspace(110, 120, num=100)


# In[16]:


lasso_mse = []
for alpha in alphas:
    lasso = linear_model.Lasso(alpha=alpha, max_iter=10000000)
    score = -1*cross_val_score(lasso, X, y, cv=10, scoring='neg_mean_squared_error').mean()
    lasso_mse.append(score)


# In[17]:


plt.plot(alphas, lasso_mse)


# In[18]:


print('The best lambda is:', alphas[lasso_mse.index(min(lasso_mse))])


# In[19]:


lasso = linear_model.Lasso(alpha=alphas[lasso_mse.index(min(lasso_mse))], max_iter=10000000)
print('The MSE is:', -1*cross_val_score(lasso, X, y, cv=10, scoring='neg_mean_squared_error').mean())


# # Q2
# 
# Ref: http://thatdatatho.com/2018/10/04/cross-validation-the-wrong-way-right-way-feature-selection/

# # Q4
# 
# Ref: https://www.win.tue.nl/~marko/2WB05/lecture8.pdf

# # Q5
# 
# Ref: https://books.google.com.au/books?id=Trj9HQ7G8TUC&pg=PA384&lpg=PA384&dq=relative+error+of+the+estimator++in+CMC&source=bl&ots=1EZbEg3azG&sig=ACfU3U03XjPxPwZzvEW641zHuEDTAWEoCg&hl=en&sa=X&ved=2ahUKEwiGhur66MvpAhV28XMBHb0pBJIQ6AEwAXoECAkQAQ#v=onepage&q=relative%20error%20of%20the%20estimator%20%20in%20CMC&f=false

# In[ ]:




