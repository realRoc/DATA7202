# -*- coding: utf-8 -*-
"""
Created on Tue Jun  9 15:55:25 2020

@author: Yupeng Wu
"""

from scipy.stats import expon
import numpy as np
import matplotlib.pyplot as plt

# Get the data from csv
def readData(filename):
    data = []
    with open(filename) as f:
        for line in f:
         data.append(line.strip().split(','))
    return data
# Expon pdf
def func(x, a, b, c):
    return a * np.exp(-b * x) + c

# Prepare data
data = readData('data.csv')[1:]
Customers = range(len(data))
arrive = [0 for i in Customers]
service = [0 for i in Customers]

for i in Customers:
    arrive[i] = float(data[i][0])
    service[i] = float(data[i][1])

# Generate plot to show the data is exponentially distributed
plt.plot(sorted(arrive), label = 'arrive')
plt.plot(sorted(service), label = 'service')
plt.axis()
plt.xlabel('Customer No.')
plt.ylabel('Arrive/Service Time')
plt.title('The Arrive/Service Time Are Exponentially Distributed')
plt.legend()
plt.show()

# Compute the mean of the exponential distribution
# scale_arrive = 0.80341291196 (mean arrive time: 1/lambda)
loc_arrive, scale_arrive = expon.fit(arrive, floc=0)
# scale_service = 2.3468397434240003 (mean service time: 1/mu)
loc_service, scale_service = expon.fit(service, floc=0)

# CDF test whether they are exponential
exp_arrive = expon.cdf(np.linspace(0,6,len(arrive)), scale=scale_arrive)
plt.plot(np.linspace(0,6,len(arrive)), exp_arrive, label='cdf')
plt.scatter(sorted(arrive), np.arange(len(arrive))/len(arrive), s=10, c='red', label='data')
plt.axis()
plt.xlabel('Arrive Time')
plt.ylabel('Arrive Time Cumulative Probability')
plt.title('The Arrive Time CDF Test')
plt.legend()
plt.show()

exp_service = expon.cdf(np.linspace(0,17,len(service)), scale=scale_service)
plt.plot(np.linspace(0,17,len(service)), exp_service, label='cdf')
plt.scatter(sorted(service), np.arange(len(service))/len(service), s=10, c='red', label='data')
plt.axis()
plt.xlabel('Service Time')
plt.ylabel('Service Time Cumulative Probability')
plt.title('The Service Time CDF Test')
plt.legend()
plt.show()


import random
import simpy

RANDOM_SEED = 12345
# Total number of customers (set high to make sure there are enough customers)
NEW_CUSTOMERS = 10000
# The scale of the twp exponential distributions
# Arrival 1.245 customer per unit time on average
lambda_arrive = 1.0 / 0.80341291196
# Service 0.426 customer per unit time on average
mu_service = 1.0 / 2.3468397434240003
# Generate new customers roughly every lambda_arrive seconds
INTERVAL_CUSTOMERS = lambda_arrive


def source(env, number, interval, counters):
    """Source generates customers randomly"""
    for i in range(number):
        lengths = []
        for rescource in counters:
            lengths.append(len(rescource.queue) + rescource.count)
        desk = lengths.index(min(lengths))
        counter = counters[desk]
        # Generate customer with the service time
        c = customer(env, 'Customer%02d' % i, counter, desk, service= mu_service)
        env.process(c)
        t = random.expovariate(interval)
        yield env.timeout(t)


def customer(env, name, counter, desk, service):
    """Customer arrives, is served and leaves."""
    arrive = env.now
    #print('%7.4f %s: Here I am to desk %s' % (arrive, name, desk))

    with counter.request() as req:
        # Wait for the counter
        yield req

        wait = env.now - arrive

        WAIT.append(wait)
        
        # We got to the counter
        #print('%7.4f %s: Waited %6.3f' % (env.now, name, wait))

        tib = random.expovariate(service)
        yield env.timeout(tib)
        #print('%7.4f %s: Finished' % (env.now, name))


# Setup and start the simulation
print('Air Secure Service')
random.seed(RANDOM_SEED)
# Generate simpy environment
env = simpy.Environment()
# Set the waiting time list
WAIT = []

# Start processes and run
# Set 4 queues, where each queue has 1 desk
c = 4
counters = [simpy.Resource(env, capacity=1) for i in range(c)]
env.process(source(env, NEW_CUSTOMERS, INTERVAL_CUSTOMERS, counters))
# 3000 units of times
env.run(until=3000)

# Compute the percentage
# Generate zero-one list, if waiting time less than 8 is 1, else 0
WAIT_8 = []
for time in WAIT:
    if time <= 8:
        WAIT_8.append(1)
    else:
        WAIT_8.append(0)

mean_8 = np.mean(WAIT_8)
std_8 = np.std(WAIT_8)/np.sqrt(len(WAIT_8))

rho = lambda_arrive/(c*mu_service)
print('\n Server utilization rho = ', round(rho, 3), ' < 1, with', c, 'desks/servers.')

print('\n The system will have a stationary distribution if the desk is more than: \n',
      round(lambda_arrive/mu_service, 3))

print('\n The probability of customers waiting less than 8 minutes: \n', 
      round(mean_8*100, 3), '%')

print('\n 95% CI for probability of customers waiting less than 8 minutes: \n (',
      round((mean_8 - 1.96*std_8)*100, 3), '%,', round((mean_8 + 1.96*std_8)*100, 3), '% )')

def factorial(n):
    if n == 0 or n == 1:
        return 1
    else:
        return (n*factorial(n-1))

C_prob = 1/(1+(1-rho)*(factorial(c)/((c*rho)**c))*sum(((c*rho)**k)/factorial(k) 
                                                      for k in range(c)))
print('\n The probability that an arriving customer (all servers are occupied) is forced to join the queue is: \n',
      round(C_prob*100, 3), '%')

C_sys = rho/(1-rho)*C_prob + c*rho
print('\n The average number of customers in the system (in service and in the queue) is: \n',
      round(C_sys, 3))

C_response = C_prob/(c*mu_service-lambda_arrive) + 1.0/mu_service
print('\n The response time (the total amount of time a customer spends in both the queue and in service) is: \n',
      round(C_response, 3))












