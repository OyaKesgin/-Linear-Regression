# Q1:
Credit <- read.csv("~/Downloads/Credit.csv")
View(Credit)
# credit.data=read.csv("Credit.csv")
attach(Credit)
str(Credit)
Gender=as.factor(Gender)
Married=as.factor(Married)
Ethnicity=as.factor(Ethnicity)
credit1=Credit[,-c(1,8,9,10,11)]
pairs(credit1)
#Summary of Credit1 variables, min max Median Mean first and third quartiles
summary(credit1)
#finding correlation between variables
cor(credit1)
#regression of rating against Income,Limit,Cards,age, Education,Balance
credit.reg=lm(Rating ~ Income+Limit+Cards+Age+Education+Balance)
summary(credit.reg)
credit.reg2=lm(Rating ~ Limit+Cards)
summary(credit.reg2)
# The regression summary suggests that Income and Rating are the most significant predictors.
# The correlation matrix shows that Limit is strongly correlated with both Income and Balance. Checking the linear regression model
# with Limit and Cards only shows that the R^2 is extremely high and basically the same as R^2 with all numerical predictors.
# This is thus the best linear regression model.

credit.reg3=lm(Balance ~ Ethnicity + Gender)
summary(credit.reg3)
b0=credit.reg3$coefficients[1]
b1=credit.reg3$coefficients[2]
b2=credit.reg3$coefficients[3]
b3=credit.reg3$coefficients[4]
# When running the regression with qualitative predictors only, you obtain
# the average of the response for each of the categories. The values in the summary
# are interpreted as:
# 1.beta0 is the average balance of an African American female
# 2.beta0+beta3 is the average balance of African American male
# 3.beta0+beta1 is the average balance of an Asian female
# 4.beta0+beta1+beta3 is the average balance of an Asian male
# 5.beta0+beta2 is the average balance of a Caucasian female
# 6.beta0+beta2+beta3 is the average balance of a Caucasian male
