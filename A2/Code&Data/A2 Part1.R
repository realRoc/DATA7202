# Q1
library(lattice)
library(Hmisc)
library(car)
library(MASS)

# Prepare the dataset
ONP = read.csv("C:/Users/simon/Desktop/DATA7202/A1/OnlineNewsPopularity.csv")
attach(ONP)

# Find outlier, set a max limitation for following variables
ds <- ONP[,!names(ONP) %in% c("url", "timedelta",
                              "weekday_is_sunday",
                              "is_weekend", "LDA_04")]
ds <- ds[ds$n_non_stop_words != max(n_non_stop_words),]
ds <- ds[,!names(ds) %in% "kw_min_min"]
ds <- ds[ds$kw_avg_min >= 0 & ds$kw_min_avg >= 0,]

ds <- ds[,!names(ds) %in% c("n_unique_tokens",
                            "n_non_stop_words",
                            "n_non_stop_unique_tokens",
                            "average_token_length",
                            "kw_max_min",
                            "kw_avg_min",
                            "kw_avg_avg",
                            "self_reference_avg_sharess",
                            "rate_positive_words",
                            "rate_negative_words")]

# variables' transformation in ds
ds$n_tokens_content <- log(ds$n_tokens_content + 1)
ds$num_self_hrefs <- log(ds$num_self_hrefs + 1)
ds$kw_min_max <- log(ds$kw_min_max + 1)
ds$kw_max_max <- log(ds$kw_max_max + 1)
ds$kw_avg_max <- log(ds$kw_avg_max + 1)
ds$kw_max_avg <- log(ds$kw_max_avg + 1)
ds$LDA_00 <- log(ds$LDA_00 + 1)
ds$LDA_01 <- log(ds$LDA_01 + 1)
ds$LDA_02 <- log(ds$LDA_02 + 1)
ds$LDA_03 <- log(ds$LDA_03 + 1)
ds$global_subjectivity <- log(ds$global_subjectivity + 1)
ds$global_sentiment_polarity <- ds$global_sentiment_polarity^3

# General Linear Model in A1
lm <- lm(shares~.+num_videos*num_imgs, data = ds)
summary(lm)
1-lm$deviance/lm$null.deviance

# use new variables to build a GLM
library(pROC)
ds_log <- ds
ds_log$shares <- ifelse(ds$shares>1000, 1, 0)
# logistic
logmod <- glm(shares~., family = binomial(link = "logit"), 
              data = ds_log)
summary(logmod) # AIC=43534
# poisson
poismod <- glm(shares~., family = poisson(link = 'sqrt'), data=ds)
summary(poismod) # AIC=228651442
1-poismod$deviance/poismod$null.deviance # R^2=0.1131
poispred = predict.glm(poismod)
# gaussian
gausmod = glm(shares~., family = gaussian(link = 'identity'), data=ds)
summary(gausmod) # AIC=836541
1-gausmod$deviance/gausmod$null.deviance # R^2=0.0209
# gamma
gamamod = glm(shares~., family = Gamma(link = 'log'), data=ds)
summary(gamamod) # AIC=701268
1-gamamod$deviance/gamamod$null.deviance # R^2=0.1630
# negative binomial
nbmod = glm.nb(shares~., link = 'sqrt', data=ds)
summary(nbmod) # AIC=700442
1-nbmod$deviance/nbmod$null.deviance # R^2=0.1702

# Q2
summary(ds)
data <- ds
x = seq(0,5,0.01)
num_imgseq = data.frame(x)

## PI
lmpred = predict(lm,ds,interval = 'predict',level = 0.95)
lmfit = lmpred[order(lmpred[,1]),]
matplot(lmfit,type = 'l',lty = 1:3,col = 1:3,
        main = '95% predictive interval for linear regression')

nbpred = predict(nbmod,ds,interval = 'predict', level = 0.95, se.fit = TRUE)
lwnb = nbpred$fit - 1.96*nbpred$se.fit
upnb = nbpred$fit + 1.96*nbpred$se.fit
nbfit = cbind(nbpred$fit,lwnb,upnb)
nbfit = nbfit[order(nbfit[,1]),]
matplot(nbfit,ylab='glmfit',type = 'l',lty = 1:3,col = 1:3,
        main = '95% predictive interval for negative-binomial regression')

library(tidyverse)
library(broom)
par(mfrow = c(2,2))
plot(nbmod)
par(mfrow = c(1,1))
