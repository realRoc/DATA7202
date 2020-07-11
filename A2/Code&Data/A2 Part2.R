library(forecast)
library(tseries)
library(Metrics)
library(numbers)
wk1 = read.csv(file="flow_20130225-20130303.csv",
               header=TRUE, fileEncoding = "utf-8")
wk2 = read.csv(file="flow_20130304-20130310.csv",
               header=TRUE, fileEncoding = "utf-8")
wk3 = read.csv(file="flow_20130311-20130317.csv",
               header=TRUE, fileEncoding = "utf-8")
wk4 = read.csv(file="flow_20130318-20130324.csv",
               header=TRUE, fileEncoding = "utf-8")
translink = rbind(wk1,wk2,wk3,wk4)

# Q3
date = unique(translink$date)
v0_byday = vector(mode = "numeric",length = length(date)*24)
x = 1:(length(date)*24)
for (index in 1:length(translink$date)){
  if (translink$region_from[index] == 1 &&
      translink$region_to[index] == 5){
    day = match(translink$date[index],date)
    hour = translink$time_id[index]
    num = (day-1)*24 + hour
    v0_byday[num] = translink$v0_num_traj[index]
  }}
# We don't need weekend
weekend = v0_byday[121:168]
weekday = v0_byday[1:48]
x = 1:(2*24)
plot(x,weekday,ylab = 'Trajectory', 
     main='Passenger Trajectory on Monday and Tuesday', type = 'l')
plot(x,weekend,ylab = 'Trajectory', 
     main='Passenger Trajectory on Saturday and Sunday', type = 'l')

wk = 1:4
v0_wk1 = v0_byday[1:120]
v0_wk2 = v0_byday[169:288]
v0_wk3 = v0_byday[337:456]
v0_wk4 = v0_byday[505:624]
v0_byday = c(v0_wk1,v0_wk2,v0_wk3,v0_wk4)
v0 = vector(mode = "numeric",length = 19*5*4)
i = 1
for (index in 1:length(v0_byday)){
  if (mod(index,24)>5 | mod(index,24)==0){
    v0[i] = v0_byday[index]
    i = i+1
  }
}

x= 1:(5*4*19)
plot(x,v0, type='l', ylab = 'Trajectory', 
     main = "Passenger's Trajectory on Workdays in One Month")
summary(v0_byday)

v0_mean = c(mean(v0_wk1),mean(v0_wk2),mean(v0_wk3),mean(v0_wk4))
v0_min = c(min(v0_wk1),min(v0_wk2),min(v0_wk3),min(v0_wk4))
v0_max = c(max(v0_wk1),max(v0_wk2),max(v0_wk3),max(v0_wk4))
par(mfrow = c(2,2))
boxplot(v0_wk1, main="Week 1 summary")
boxplot(v0_wk2, main="Week 2 summary")
boxplot(v0_wk3, main="Week 3 summary")
boxplot(v0_wk4, main="Week 4 summary")
par(mfrow = c(1,1))

t = unlist(v0)
ts0 = ts(t,frequency = 19, start = c(1,1))
# stationary
acf(v0, main='acf', 1000)
adf.test(ts0)

# seasonality
library(seastests)
isSeasonal(ts0)
summary(wo(ts0))


# Q4
# ARIMA
plot(ts0, main="Origin Series")
acf(ts0, main="ACF for Origin Series")
pacf(ts0, main="PACF for Origin Series")

v0_diff1 = diff(v0, lag=19)
t_diff1 = unlist(v0_diff1)
ts_diff1 = ts(t_diff1,frequency = 19, start = c(1,1))
plot(ts_diff1, main="Diff1 with lag 19")
acf(ts_diff1, main="ACF after first diff")
pacf(ts_diff1, main="PACF after first diff")

v0_diff2 = diff(v0_diff1)
t_diff2 = unlist(v0_diff2) # d=2
ts_diff2 = ts(t_diff2,frequency = 19, start = c(1,1))
plot(ts_diff2, main="Diff2")
acf(ts_diff2, main="ACF after second diff")
pacf(ts_diff2, main="PACF after second diff")

arimamod = arima(ts0, order = c(3,0,1),seasonal = list(order = c(1,1,1), period=19))
summary(arimamod)
checkresiduals(arimamod)

# SNaive
t = unlist(v0)
tssn = ts(t,frequency = 95)
snmod = snaive(tssn)
snaive(tssn, 19)
summary(snmod)
checkresiduals(snmod)

# Holt Winter
hwmod = hw(ts0,seasonal = "multiplicative")
summary(hwmod)
checkresiduals(hwmod)

# Forecast 25/3/2013
autoplot(ts0) + 
  autolayer(forecast(arimamod,19)$mean, series = "SARIMA") + 
  autolayer(snaive(ts0,19)$mean, series = "SNaive") + 
  autolayer(hw(ts0,19)$mean, series = "Holt Winter") +
  ggtitle("Forecast for translink passenger on 25/3/2013 (based on workday data from 25/2/2013 to 24/3/2013)") +
  xlab("Day") + ylab("Passenger trajectory") +
  guides(colour = guide_legend(title = "Forecast"))

mse(forecast(arimamod,4)$mean[1:4],c(2,18,34,68)) # 283.5557
mse(snaive(ts0,4)$mean,c(2,18,34,68)) # 190.5
mse(hw(ts0,4,seasonal = "multiplicative")$mean,c(2,18,34,68)) # 352.8632




# Q7
plot(forecast(arimamod,380))
forecast(arimamod,19)
plot(snaive(tssn,380))
snaive(tssn,19)
plot(hw(ts0,380))

# Q8
# ARIMA
j = 1
h1 = vector()
for (index in 361:379){
  v0_h = v0[1:index]
  arima_h = arima(v0_h, order = c(2,0,1),seasonal = list(order = c(1,1,1), period=19))
  h1[j] = forecast(arima_h,1)$mean[1:1]
  j = j + 1
}
h1
mse(h1,v0[362:380]) # 160.3136
rmse(h1,v0[362:380]) # 160.3136
write.csv(h1)

j = 1
h2 = vector()
for (index in 360:378){
  v0_h = v0[1:index]
  #arima_h = arima(v0_h, order = c(2,0,1),seasonal = list(order = c(1,1,1), period=19))
  #h2[j] = forecast(arima_h,2)$mean[2:2]
  h2[j] = snaive(v0_h,2)$mean[2:2]
  j = j + 1
}
h2
rmse(h2,v0[362:380]) # 181.5461
write.csv(h2)

# Snaive
t = unlist(v0[1:361])
tssn = ts(t,frequency = 95)
snmod = snaive(tssn)
rmse(snaive(tssn, 19)$mean[1:19],v0[362:380])

# HW
j = 1
h1 = vector()
for (index in 361:379){
  v0_h = v0[1:index]
  ts_hw = ts(unlist(v0_h),frequency = 19)
  h1[j] = hw(ts_hw,1)$mean[1:1]
  j = j + 1
}
h1
rmse(h1,v0[362:380]) # 15.00281
write.csv(h1)

j = 1
h2 = vector()
for (index in 360:378){
  v0_h = v0[1:index]
  ts_hw = ts(unlist(v0_h),frequency = 19)
  h2[j] = hw(ts_hw,2)$mean[2:2]
  j = j + 1
}
h2
rmse(h2,v0[362:380]) # 15.01611
write.csv(h2)

# Q9
v0[381] = 2
v0[382] = 18
v0[383] = 34
v0[384] = 68
t9 = unlist(v0)
ts9 = ts(t9,frequency = 19)
arima9 = arima(ts9, order = c(3,0,1),seasonal = list(order = c(1,1,1), period=19))
write.csv(forecast(arima9,15)$mean[1:15])
rmse(forecast(arimamod,19)$mean[1:4],c(2,18,34,68))