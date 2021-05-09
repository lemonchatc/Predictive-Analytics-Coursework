#hw2_Lee_YuHsin(Kathy)
library(dplyr)
library(leaps)

setwd("/Users/KathyLee/UCI MSBA/Course Material/05 Spring Quarter/BANA288 Predictive Analytics/Homework")
data <- read.csv("hw2_hour.csv")
names(data)

# 1.	Perform all necessary cleaning and transformation of the data to make it 
#     useful for regression modeling.
#  Create dat2 – with indicator variables
mnth <- as.factor(data$mnth)
season <- as.factor(data$season)
hr <- as.factor(data$hr)
wkday <- as.factor(data$wkday)
weathersit <- as.factor(data$weathersit)
tmp_mnth <- data.frame(model.matrix(~mnth-1))
tmp_season <- data.frame(model.matrix(~season-1))
tmp_hr <- data.frame(model.matrix(~hr-1))
tmp_wkday <- data.frame(model.matrix(~wkday-1))
tmp_weathersit <- data.frame(model.matrix(~weathersit-1))
dat2 <- cbind(data[,c(15,1,4)], tmp_season[,1:3], 
              tmp_mnth[,1:11], data[,c(7,9)], 
              tmp_wkday[,1:6], tmp_hr[,1:23], 
              tmp_weathersit[,2:4], data[,11:14])
rm(mnth, season, hr, wkday, weathersit)
rm(tmp_mnth, tmp_season, tmp_hr, tmp_wkday)
rm(tmp_weathersit)
str(dat2)


# 2.	Compute the correlation between all independent variables and the response.  
#     List the top 3 (or more) correlated variables. Explain why these make 
#     sense or why they are surprising. 
data2 <- subset(data, select = -dteday)
cor(data2)[14,]

# The top 3 correlated variables are atemp, temp and hr. atemp and temp 
# represents the temperature and feeling temperature respectively. This makes
# sense as people are more likely to rent a bike in a warmer day. As for hr, 
# more people rent a bike during time before work time (7 & 8am) and time off 
# work (5 & 6pm). It makes sense as people use bike as their commuting tool.


# 3.	Using the seasonal variables, the hourly indicators, and the weather 
#     variables, construct a linear regression model predicting the rental count
#     (cnt) using this set of variables.  Interpret the coefficient of 
#     determination.  Which variables are significant at the 5% level?
mod1 <- lm(cnt ~ . - cnt - obs - yr - mnth1 - mnth2 - mnth3 - mnth4 - mnth5 - 
             mnth6 - mnth7 - mnth8 - mnth9 - mnth10 - mnth11 - holiday - 
             workday - wkday0 - wkday1 - wkday2 - wkday3 - wkday4 - wkday5, 
           data = dat2)
summary(mod1)


#  The R-squared (multiple r-squared) value is 0.6261. 62.61% of the variation  
#  in count of total rental bikes in this dataset is explained by the seasonal
#  variables, hourly indicators, and weather variables in the model above.
#  All variables are significant at the 5% level except weathersit4. 

# cnt = 69.519 + (-62.348)*season1 + (-26.551)*season2 + (-45.673)season3 +
#       (-31.293)*hr0 + (-48.091)*hr1 + (-56.227)*hr2 + (-66.252)*hr3 +
#       (-68.314)*hr4 + (-52.036)*hr5 + 6.402*hr6 + 140.715*hr7 + 279.962*hr8 +
#       130.527*hr9 + 74.039*hr10 + 97.624*hr11 + 135.411*hr12 + 129.463*hr13 +
#       113.023*hr14 + 122.297*hr15 + 184.710*hr16 + 339.198*hr17 + 308.301*hr18
#       + 201.119*hr19 + 122.729*hr20 + 74.311*hr21 + 38.190*hr22 + 
#       (-5.756)*weathersit2 + (-60.245)*weathersit3 + (-36.057)*weathersit4 +
#       192.291*temp + 94.438*atemp + (-102.320)*hum + (-40.439)*windspeed

#  Within the variables, humidity (hum) has the largest negative impact on cnt,
#  while hr17 has the largest positive impact on cnt. For each additional 
#  humidity increases, the count of rental bikes decreases 102. If the time
#  is 17:00, the count of rental bikes increases 339. Within season variables,
#  Summer has less negative impact on cnt than Spring and Fall. Within hour
#  variables, hours between 00:00 and 05:00 have negative impact. Hours between 
#  06:00-22:00 have positive impact on cnt, especially 7-9am, 12-13pm, and 
#  16-19pm. Compared with other weather variables, weathersit3 (light snow, 
#  light rain + thunderstorm + scattered clouds, light Rain + Scattered clouds) 
#  has the largest negative impact.


# 4.	Choose preferred linear regression model predicting the rental count (cnt)
#     using a subset of the variables described in the prior question. Justify 
#     the model choice by using the nested F-test to compare the chosen model in 
#     this question to the model fit in prior question. Interpret the result of 
#     the hypothesis test.
mod2 <- lm(cnt ~ . - hr6 - weathersit4 - cnt - obs - yr - mnth1 - mnth2 - 
             mnth3 - mnth4 - mnth5 - mnth6 - mnth7 - mnth8 - mnth9 - mnth10 - 
             mnth11 - holiday - workday - wkday0 - wkday1 - wkday2 - wkday3 - 
             wkday4 - wkday5, data = dat2)
summary(mod2)

anova(mod2, mod1)
#     Res.Df     RSS     Df  Sum of Sq     F      Pr(>F)
#  1  17347   213779250                           
#  2  17345   213760547  2    18703     0.7588   0.4682

#  Ho:  Models are same
#  Ha:  Model with more variables is better
#  The R-squared value of both models is 0.6261. The p-value = 0.4682 is larger 
#  than alpha. Ho is not rejected. Therefore, mod2 is better at explaining the 
#  rental count (cnt) than mod1. 


# 5.	Using the entire data set, choose a preferred model for predicting rentals.
#     Justify the model choice both using metrics for measuring model quality 
#     and practically. 
reg <- regsubsets(cnt ~ ., data = dat2, nvmax = 55, method = "forward")
summary(reg)

coef(reg, 40)

# Using 40 variables
mod3 <- lm(cnt ~ obs + yr + season1 + season2 + season3 + mnth5 + mnth7 + 
             mnth9 + mnth10 + holiday + wkday0 + hr1 + hr2 + hr3 + hr4 + hr5 + 
             hr6 + hr7 + hr8 + hr9 + hr10 + hr11 + hr12 + hr13 + hr14 + hr15 + 
             hr16 + hr17 + hr18 + hr19 + hr20 + hr21 + hr22 + weathersit2 + 
             weathersit3 + weathersit4 + atemp + hum + windspeed + wkday5, 
           data = dat2)
summary(mod3)

# Remove variables that are not significant
mod4 <- lm(cnt ~ yr + season1 + season2 + season3 + mnth5 + mnth7 + 
             mnth9 + mnth10 + holiday + wkday0 + hr1 + hr2 + hr3 + hr4 + hr5 + 
             hr6 + hr7 + hr8 + hr9 + hr10 + hr11 + hr12 + hr13 + hr14 + hr15 + 
             hr16 + hr17 + hr18 + hr19 + hr20 + hr21 + hr22 + weathersit2 + 
             weathersit3 + atemp + hum + windspeed, data = dat2)
summary(mod4)

anova(mod3, mod4)

#    Res.Df     RSS     Df   Sum of Sq      F      Pr(>F)   
#  1  17338  180178934                                
#  2  17341  180302483  -3   -123549    3.9629   0.007788 **

# Here, I first used stepwise regression to find subset. After that, I ran the 
# model with 40 variables once and then removed variables that are not  
# significant. The R-Squared value of both models is similar (mod3 = 0.6849, 
# mod4 = 0.6847). The p-value (0.0078) is larger than alpha (0.001), therefore, 
# we can say that mod4 is better at explaining the rental count (cnt) than mod3. 
# Plus, mod4 is more simple than mod3.


# 6.	Randomly select approximately half of the observations to create a 
#     training data set.  Add the remaining rows to create a test data set.  
#     Using the variables chosen in the model from question 5, compute the RSS, 
#     MSE, and RMSE on the training data.  Repeat this on the test data, by 
#     using the model fit on the training data to compute predictions on the 
#     test set.  Comment on the performance of the model with respect to the 
#     bias-variance tradeoff.
set.seed(123456)
nrows.train <- 8690
train <- sample(1:nrow(dat2), nrows.train)
train[1:10]
dat.train <- dat2[train,]
dat.test <- dat2[-train,]

mod6 <- lm(cnt ~ yr + season1 + season2 + season3 + mnth5 + mnth7 + 
             mnth9 + mnth10 + holiday + wkday0 + hr1 + hr2 + hr3 + hr4 + hr5 + 
             hr6 + hr7 + hr8 + hr9 + hr10 + hr11 + hr12 + hr13 + hr14 + hr15 + 
             hr16 + hr17 + hr18 + hr19 + hr20 + hr21 + hr22 + weathersit2 + 
             weathersit3 + atemp + hum + windspeed, data = dat.train)
summary(mod6)

# Compute the RSS, MSE, and RMSE on training and test data
rss.train <- sum(mod6$residuals^2)
mse.train <- rss.train/(nrow(dat.train)-37-1)
rmse.train <- mse.train^0.5

yhat <- predict(mod6, dat.test)
rss.test <- sum((dat.test$cnt - yhat)^2)
mse.test <- rss.test/(nrow(dat.test)-37-1)
rmse.test <- mse.test^0.5

tab <- matrix(c(rss.train, mse.train, rmse.train, rss.test, mse.test, 
                rmse.test), ncol = 2, byrow = FALSE)
colnames(tab) <- c("Training", "Test")
rownames(tab) <- c("RSS", "MSE", "RMSE")
tab

#          Training          Test
#  RSS  89428853.135   91112666.474
#  MSE     10336.206      10532.039
#  RMSE     101.6671       102.6257

#  Compared with the training dataset, the RSS, MSE, and RMSE increased a little
#  in the test dataset but they are still pretty similar but overall they are
#  pretty similar. This indicates that the model fits almost equally on both 
#  datasets. There aren't big concern on high bias or high variance.


# 7.	Repeat question 6 using the same training and test data sets. In this  
#     case, fit the model using all variables in the data set. Again, compute  
#     the model fit errors on the training set as well as on the test set by  
#     using the training model to make the test set predictions. Comment on the 
#     performance of the model with respect to the bias-variance tradeoff. 
#     Compare results to question 6.
mod7 <- lm(cnt ~ ., data = dat.train)
summary(mod7)

# Compute the RSS, MSE, and RMSE on training and test data
rss.train2 <- sum(mod7$residuals^2)
mse.train2 <- rss.train2/(nrow(dat.train)-54-1)
rmse.train2 <- mse.train2^0.5

yhat2 <- predict(mod7, dat.test)
rss.test2 <- sum((dat.test$cnt - yhat2)^2)
mse.test2 <- rss.test2/(nrow(dat.test)-54-1)
rmse.test2 <- mse.test2^0.5

tab2 <- matrix(c(rss.train2, mse.train2, rmse.train2, rss.test2, mse.test2,  
                rmse.test2), ncol = 2, byrow = FALSE)
colnames(tab2) <- c("Training", "Test")
rownames(tab2) <- c("RSS", "MSE", "RMSE")
tab2

#          Training         Test
#  RSS  88926139.581   90684064.667
#  MSE     10298.337      10503.135
#  RMSE     101.4807       102.4848

#  Compared with mod6, the RSS, MSE, and RMSE of mod7 are slightly smaller. 
#  This can be explained as mod7 has more variables than mod6. Overall, the RSS,
#  MSE, and RMSE are similar in both dataset. There aren't big concern on high 
#  bias or high variance.


# 8.	Repeat question 7 using the first year’s data as the training data set and 
#     the second year’s data as the test set.  How does this result compare to 
#     the two earlier questions?
dat.train8 <- dat2 %>% filter(yr == 0)
dat.test8 <- dat2 %>% filter(yr == 1)

mod8 <- lm(cnt ~ ., data = dat.train8)
summary(mod8)

# Compute the RSS, MSE, and RMSE on training and test data
rss.train8 <- sum(mod8$residuals^2)
mse.train8 <- rss.train8/(nrow(dat.train8)-54-1)
rmse.train8 <- mse.train8^0.5

yhat8 <- predict(mod8, dat.test8)
rss.test8 <- sum((dat.test8$cnt - yhat8)^2)
mse.test8 <- rss.test8/(nrow(dat.test8)-54-1)
rmse.test8 <- mse.test8^0.5

tab3 <- matrix(c(rss.train8, mse.train8, rmse.train8, rss.test8, mse.test8, 
                 rmse.test8), ncol = 2, byrow = FALSE)
colnames(tab3) <- c("Training", "Test")
rownames(tab3) <- c("RSS", "MSE", "RMSE")
tab3

#         Training           Test
#  RSS  48765469.373   318114363.7956
#  MSE      5677.005        36653.343
#  RMSE       75.346          191.451

#  Compared with mod6 and mod7, the difference between training and test data is
#  larger and RSS, MSE, and RMSE are much smaller in mod8. The MSE and RMSE are 
#  larger in test data. This shows that mod8 fits better in training data. 
#  Compared with mod6 and mod7, mod8 is more likely to have high variation.


# 9.	Summarize, briefly, what the managers of the bike rental concern should 
#     know about the prediction models found. How effective are the models?  
#     What model should be used to predict in the future (year 3)?
#     How confident are you in future predictions? 

#  The count of rental bikes is highly correlated with temperature, feeling 
#  temperature, hours, and humidity. The warmer the weather is, the more bikes
#  will be rented. Furthermore, it is expected that people are more likely to 
#  rent a bike during commuting hours, such as 8am and 16-19pm. 
#  I am confident that mod4 should be used to predict in the future as it 
#  explains nearly 70% of variation in the count of rental bikes.


# 10.	Are there any transformations of the variables that can be added to 
#     improve the model fit (e.g., squared variables)? Are there any interaction 
#     variables (products of variables) that can be added?  Try at least one 
#     variable with higher order terms (e.g., squared,) and at least one 
#     interaction model to see if the biased can be reduced in fitting the model 
#     on the training set. Report results. 

# Higher order terms
mod9 <- lm(cnt ~ I(hum^2) + yr + season1 + season2 + season3 + mnth5 + mnth7 + 
             mnth9 + mnth10 + holiday + wkday0 + hr1 + hr2 + hr3 + hr4 + hr5 + 
             hr6 + hr7 + hr8 + hr9 + hr10 + hr11 + hr12 + hr13 + hr14 + hr15 + 
             hr16 + hr17 + hr18 + hr19 + hr20 + hr21 + hr22 + weathersit2 + 
             weathersit3 + atemp + hum + windspeed, data = dat.train)
summary(mod9)

# Compute the RSS, MSE, and RMSE on training and test data
rss.train9 <- sum(mod9$residuals^2)
mse.train9 <- rss.train9/(nrow(dat.train)-38-1)
rmse.train9 <- mse.train9^0.5

yhat9 <- predict(mod9, dat.test)
rss.test9 <- sum((dat.test$cnt - yhat9)^2)
mse.test9 <- rss.test9/(nrow(dat.test)-38-1)
rmse.test9 <- mse.test9^0.5

tab4 <- matrix(c(rss.train9, mse.train9, rmse.train9, rss.test9, mse.test9, 
                  rmse.test9), ncol = 2, byrow = FALSE)
colnames(tab4) <- c("Training", "Test")
rownames(tab4) <- c("RSS", "MSE", "RMSE")
tab4

# Interaction model
mod10 <- lm(cnt ~ atemp*hum + yr + season1 + season2 + season3 + mnth5 + mnth7 + 
             mnth9 + mnth10 + holiday + wkday0 + hr1 + hr2 + hr3 + hr4 + hr5 + 
             hr6 + hr7 + hr8 + hr9 + hr10 + hr11 + hr12 + hr13 + hr14 + hr15 + 
             hr16 + hr17 + hr18 + hr19 + hr20 + hr21 + hr22 + weathersit2 + 
             weathersit3 + atemp + hum + windspeed, data = dat.train)
summary(mod10)

# Compute the RSS, MSE, and RMSE on training and test data
rss.train10 <- sum(mod10$residuals^2)
mse.train10 <- rss.train10/(nrow(dat.train)-38-1)
rmse.train10 <- mse.train10^0.5

yhat10 <- predict(mod10, dat.test)
rss.test10 <- sum((dat.test$cnt - yhat10)^2)
mse.test10 <- rss.test10/(nrow(dat.test)-38-1)
rmse.test10 <- mse.test10^0.5

tab5 <- matrix(c(rss.train10, mse.train10, rmse.train10, rss.test10, mse.test10, 
                 rmse.test10), ncol = 2, byrow = FALSE)
colnames(tab5) <- c("Training", "Test")
rownames(tab5) <- c("RSS", "MSE", "RMSE")
tab5

#  Results of mod9:
#          Training          Test
#  RSS  89217941.960   91073324.248
#  MSE     10313.021      10528.708
#  RMSE      101.553       102.6095

# Results of mod10:
#         Training        Test
#  RSS  88229632.108   90016804.0799
#  MSE     10198.778       10406.567
#  RMSE      100.989         102.013

#  Compared with mod6, mod7, and mod8, using higher order terms and interaction
#  did help the model fits slightly better. It also improves the R-squared value.

