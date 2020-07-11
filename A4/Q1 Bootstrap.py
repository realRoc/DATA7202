# -*- coding: utf-8 -*-
"""
Created on Fri Jun 26 17:58:50 2020

@author: simon
"""

placebo = [9243, 9671, 11792, 13357, 9055, 6290, 12412, 18806]
old = [17649, 12013, 19979, 21816, 13850, 9806, 17208, 29044]
new = [16449, 14614, 17274, 23798, 12560, 10157, 16570, 26325]

import numpy as np
old_placebo = []
new_old = []
for i in range(8):
    old_placebo.append(old[i]-placebo[i])
    new_old.append(new[i]-old[i])

theta_hat = np.mean(new_old)/np.mean(old_placebo)
print('theta_hat:', round(theta_hat,4))

from numpy.random import choice
np.random.seed(12345)

theta = []
for i in range(1000):
    index = choice(range(8), size=8, replace=True)
    theta.append(np.mean([new_old[i] for i in index])/np.mean([old_placebo[i] for i in index]))

mean = np.mean(theta)
std = np.std(theta)
print("95% CI for theta (", round(theta_hat - 1.96*std,4), ",", round(theta_hat + 1.96*std,4), ")")