# Q1
library(lattice)
library(Hmisc)
library(car)

# Prepare the dataset
ONP = read.csv("C:/Users/simon/Desktop/DATA7202/A1/OnlineNewsPopularity.csv")
attach(ONP)

# 2.1
# Find outlier, set a max limitation for following variables
ds <- ONP[,!names(ONP) %in% c("url", "timedelta",
                              "weekday_is_sunday",
                              "is_weekend", "LDA_04")]
# bwplot(shares~n_tokens_content)
# bwplot(shares~n_non_stop_words)
# bwplot(shares~n_non_stop_unique_token)
ds <- ds[ds$n_non_stop_words != max(n_non_stop_words),]
sum(ds$kw_min_min < 0) # 22979
sum(ds$kw_avg_min < 0) # 833
sum(ds$kw_min_avg < 0) # 6
ds <- ds[,!names(ds) %in% "kw_min_min"]
ds <- ds[ds$kw_avg_min >= 0 & ds$kw_min_avg >= 0,]

lm1 <- lm(shares ~ ., data = ds)
summary(lm1)

vif <- vif(lm1) # delete rows which vif>10

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
lm1 <- lm(shares ~ ., data = ds)
summary(lm1)
vif(lm1)

# 1.4
aov <- aov(beaver2$temp~beaver2$time+beaver2$activ+
             beaver2$time*beaver2$activ)
summary(aov)
beaver <- lm(temp~time+activ+time*activ, data = beaver2)
summary(beaver)
#interaction.plot(beaver2$time, beaver2$activ, beaver2$temp, fixed = TRUE, col = 2:3, leg.bty = "o", type = 'l')


# 2.2
m1 <- glm(shares~num_videos*num_imgs)
summary(m1)
# par(mfrow = c(2,2))
# plot(m1)

lm2 <- lm(shares~.+num_videos*num_imgs, data = ds)
summary(lm2)

#2.3
library(tidyverse)
library(broom)
par(mfrow = c(2,2))
plot(lm2)

#2.4
# store the original data in df, which will be used in Q4
ds_4 <- ds
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
ds$shares <- log(ds$shares + 1)
# use new variables to build a LM
lm3 <- lm(shares~.+num_videos*num_imgs, data = ds)
summary(lm3)
# par(mfrow = c(2,2))
# plot(lm3)

# 3.2
confint(lm3)

# 3.3
library(lm.beta)
coef <- lm.beta(lm3)
sort(coef$standardized.coefficients)

# 4.1
ds_4$shares <- ifelse(ds_4$shares>1000, 1, 0)
df_sample <- sample_n(ds_4, 10000)
log_model <- glm(df_sample$shares~., family = binomial(link = "logit"), data = df_sample)

# Predict the probability (p) of shares
probabilities <- predict(log_model, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)
# Linearity assumption
# Select only numeric predictors
mydata <- df_sample %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
# Create the scatter plots
ggplot(mydata, aes(logit, predictor.value))+
       geom_point(size = 0.5, alpha = 0.5) +
       geom_smooth(method = "loess") + 
       theme_bw() + 
       facet_wrap(~predictors, scales = "free_y")

# Influential values
plot(log_model, which = 4, id.n = 5)
# Extract model results
log_model.data <- augment(log_model) %>%
  mutate(index = 1:n())
log_model.data %>% top_n(5, .cooksd)

ggplot(log_model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = df_sample.shares), alpha = .5) +
  theme_bw()

log_model.data %>%
  filter(abs(.std.resid) > 3)

# Multicollinearty
car::vif(log_model)
summary(log_model)

# 4.2
library(pROC)
ds_log <- ds
ds_log$shares <- ds_4$shares
# Randomly seperate to train & test
train_index <- sample(nrow(ds_log), 0.7 * nrow(ds_log))
train_data <- ds_log[train_index, ]
test_data <- ds_log[-train_index, ]
# Using train data to build the model
model_4_2 <- glm(shares~., family = binomial(link = "logit"), 
                 data = train_data)

# 4.3
summary(model_4_2)
confint(model_4_2)

# 4.4
coef_log <- lm.beta(model_4_2)
sort(coef_log$standardized.coefficients)


# 5
library(caret)
# Linear regression
linear_train <- ds[train_index, ]
linear_test <- ds[-train_index, ]
linear_model <- lm(shares~.+num_videos*num_imgs,
                   data = linear_train)
linear_pred <- linear_model %>% predict(linear_test)
# Model performance
RMSE(linear_pred, linear_test$shares)
R2(linear_pred, linear_test$shares)
summary(linear_model)

# Logistic regression
# Predict
pre_logistic<-as.numeric(predict(model_4_2,newdata=test_data,
                                 type="response")>0.5)
obs_p_logistic = data.frame(prob=pre_logistic,obs=test_data$shares)
# confusion matrix
table(test_data$shares,pre_logistic,dnn=c("True","Predict"))
confusionMatrix(as.factor(pre_logistic),
                as.factor(test_data$shares), 
                positive = as.character(1), 
                mode = 'prec_recall')
# ROC
logistic_roc <- roc(test_data$shares,
                    predict(model_4_2,
                            newdata=test_data,
                            type="response"))
plot(logistic_roc, print.auc=TRUE, auc.polygon=TRUE,
     grid=c(0.1, 0.2),grid.col=c("green", "red"),
     max.auc.polygon=TRUE,auc.polygon.col="skyblue",
     print.thres=TRUE,main='ROC')

# 7.2
predict_linear <- linear_model %>% predict(ds)
ONP[rownames(ONP) == which(predict_linear %in% max(predict_linear)),]

predict_log <- predict(model_4_2,newdata=ds,
                       type="response")
ONP[rownames(ONP) == which(predict_log %in% max(predict_log)),]

# 7.3
summary(lm3)
summary(model_4_2)