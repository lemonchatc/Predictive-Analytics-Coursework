#hw1_Lee_Kathy
library(dplyr)
library(ggplot2)

# Load Data
setwd("/Users/KathyLee/UCI MSBA/Course Material/05 Spring Quarter/BANA288 Predictive Analytics/Homework")
data <- read.csv("hw1_universal_bank.csv")
names(data)
str(data)

# a. Write scripts to display at least 4 graphical statistical analyses
#    Create at least on each: pie chart, bar chart, histogram, and box plots
# Pie Chart
blues = c("DarkBlue", "CornflowerBlue")

crd <- table(data$Credit_Card)
pie(crd, border = "white", radius = 1, labels = as.factor(crd), 
    main = "Using a Credit Card Issued by the Bank", col = blues)
legend("topright", c("Yes","No"), cex = 0.8, fill = blues)

sec <- table(data$Sec_Account)
pie(sec, c("Yes", "No"), border = "white", radius = 1, 
    main = "Having a Securities Account with the Bank", 
    col = blues)
legend("topright", c("Yes","No"), cex = 0.8, fill = blues)

cd <- table(data$CD_Account)
pie(cd, c("Yes", "No"), border = "white", radius = 1, 
    main = "Having a Certificate of Deposit (CD) account with the Bank", 
    col = blues)
legend("topright", c("Yes","No"), cex = 0.8, fill = blues)

ob <- table(data$Online_Bank)
pie(ob, c("Yes", "No"), border = "white", radius = 1, 
    main = "Using Internet Banking Facilities", 
    col = blues)
legend("topright", c("Yes","No"), cex = 0.8, fill = blues)

# Bar Chart
fam <- table(data$Fam_Size)
barplot(fam, main = "Family Size", xlab = "Size", ylab = "Count", 
        border = "white", cex.main = 2, font.main = 2, cex.lab = 1.2, 
        font.lab = 2, col = c("LightBlue", "SkyBlue", "CornflowerBlue", 
                              "DarkBlue"))

# Histogram
inc <- table(data$Income)
hist(inc, main = "Annual Income", xlab = "Income ($1000)", ylab = "Frequency",
     border = "white", cex.main = 2, font.main = 2, cex.lab = 1.2, font.lab = 2, 
     col = "CornflowerBlue")

# Box Plot
boxplot(Mortgage~Acpt_Offer, data = data, horizontal = T,
        main = "Boxplot of Mortgage",
        col = c("#C8A2C8","LightBlue"))


# b. Create a boxplot of the Average Credit Card Debt conditional on the 
#      customer’s acceptance of the bank’s offer
boxplot(Crdt_Crd_Avg ~ Acpt_Offer, data = data, horizontal = T, 
        main = "Boxplot of Average Credit Card Debt", 
        col = c("#C8A2C8","LightBlue"))

#    Create a second “conditioned” chart of your choice as well.
boxplot(Income ~ Education, data = data, horizontal = T, 
        main = "Boxplot of Income", 
        col = c("#C8A2C8","LightBlue"))


# c. Create a scatter plot with Income on the x-axis and Average Credit Card 
#     Debt on the y-axis. Add the least squares regression line to the plot. 
#     In the context of this data set, what information does this chart convey?
ggplot(data, aes(Income, Crdt_Crd_Avg)) + 
    geom_point(color = "cornflowerblue", alpha = 0.5) + 
    geom_smooth(method = lm, se = FALSE, linetype = "dashed", color = "red") + 
    labs(title = "Income v.s. Average Credit Card Debt", x = "Income ($1000)", 
         y = "Average Credit Card Debt ($1000)")

#    Create a second scatter plot with Average Credit Card Debt on the x-axis 
#     and Accept Offer on the y-axis.
ggplot(data, aes(Crdt_Crd_Avg, Acpt_Offer)) + 
    geom_point(color = "cornflowerblue", alpha = 0.5) + 
    geom_smooth(method = lm, se = FALSE, linetype = "dashed", color = "red") + 
    labs(title = "Average Credit Card Debt v.s. Accept Offer", 
         x = "Average Credit Card Debt ($1000)", y = "Accept Offer")


# d. In well-written sentences, provide at least three unique insights
# 1. The boxplot of Average Credit Card Debt indicates that customers who 
#    accepted personal loan offer have an overall higher average credit card 
#    debt, compared with customers who didn't accept offer.
# 2. From the boxplot of income, it shows that customers whose education level 
#    is less than college have the highest average of income. Customers who 
#    have a master degree have slightly higher average income than those who 
#    have a college degree. Both of them contain many outliers.
# 3. From the scatter plot, we can see that customer with higher income tend to 
#    have higher average credit card debt. Income of most customers is within 
#    the range of 0 to 100


# e.	Compute descriptive statistics for all variables in the data set.
library(pastecs)
ds <- stat.desc(data)
round(ds, 2)


# f. Compute the Pearson correlation for all pairs variables.
#    Given the goal of finding out which customers will accept the bank’s offer, 
#    which three variables appear to be the most likely predictors?
cor(data, method = "pearson")[15,]

#  Income, Crdt_Crd_Avg, and CD_Account appear to be the most likely predictors.


# g. Fit a simple linear regression that predicts monthly average credit card 
#    debt as a function of income. What is the estimated regression model?
#    How well does this model fit? Interpret the value of the slope coefficient 
#    in this regression.
model1 <- lm(Crdt_Crd_Avg ~ Income, data = data)
summary(model1)

# Based on the result of the Summary command, the estimated model is:
#   Estimated Average Credit Card Debt = 0.1104 + 0.0247 * Income.

#  Interpretations of the Slope and Intercept:
#  -For each additional $1000 income increases, accept offer increases by 0.025.
#  -The average accept offer is 0.11

#  The overall model fit is fine.
#  The R-squared (multiple r-squared) is 0.4132
#  Interpretation of R-squared:
#  41.3% of the variation in average credit card debt in this dataset is 
#  explained by the income variable in the model above.


# h. Fit a simple linear regression that predicts which customers will accept 
#    the bank’s offer of the personal loan product as a function of monthly 
#    average credit card debt. How well does this model fit?  
#    Interpret the value of the slope coefficient in the regression.
#    Discuss the usefulness of this model.

#  Accept offer is a binary variable. We should use logistic regression.


# i. For the models fit in parts h and i, provide and interpret a 99% prediction 
#   interval estimate for the dependent variable. Assume that given values for 
#   income and credit card debt to use for prediction are $75,000 and $1,250, 
#   respectively.  Are these estimates useful?  Why or why not?  
dat1 <- data.frame(Income = 75)
predict(model1, dat1, interval = "prediction", level = 0.99)

#     fit        lwr       upr
#1  1.964954  -1.581923  5.51183 
# Estimated Average Credit Card Debt = 0.110367 + 0.0247278 * 75 = 1.964954.
# These estimates are useful. The 99% interval estimate for average credit
# card debt of a customer with $75,000 income is between 0 and 5511.83.

dat2 <- data.frame(Crdt_Crd_Avg = 1.25)
predict(model2, dat2, interval = "prediction", level = 0.99)

#      fit         lwr       upr
#1  0.05775935 -0.6665824 0.7821011
# Estimated Accept offer = -0.02181 + 0.063656 * 1.25 = 0.05775935.
# These estimates are useful. The 99% interval estimate for accept offer of a 
# customer with $1,250 average credit card debt is between 0 and 0.78.


# j. Suppose the goal was to predict whether a customer would accept the bank’s 
#    product offer (Acpt_Offer). Provide 2 examples of statistical modeling 
#    methods would be used. Justify this answer.  

#  To predict whether a customer would accept the bank's product offer, examples
#  of statistical modeling methods we could use would be logistic regression and
#  K-Nearest Neighbor. Acpt_offer is a binary variable. Logistic regression is 
#  suitable for building model for binary dependent variable. K-Nearest Neighbor
#  can help find similar data points to help predict if a customer would accept
#  the bank's offer.
