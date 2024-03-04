#Regression - Part2
# Dimension Reduction: Variable selection procedures
#When you have lot of columns in a typical company data base, some of them are not useful so you cut them out = dimention reduction)
# Stepwise Regression

# WestRoxbury dataset is a dataset about houses in Boston. 
# Our purpose here is is to predict a new house's value using other houses
# in the dataset.
# This dataset explored and cleaned in Part-1.


#----------------------------------------------------------------------------
# 9) Creating a Regression Model with ALL Possible Predictors
#----------------------------------------------------------------------------

reg.All = lm(TOTAL.VALUE ~ ., data = train.data) #period means all variables #everything but one variable will = period then Negative "Name of the variable)
 #reg.All = lm(TOTAL.VALUE ~ . -TAX, data = train.data) = everything but TAX


summary(reg.All)

# Calculate accuracy measures on validation data using full regression model
# Compare the values to reg1's

library(forecast)

pred.All= predict(reg.All, newdata = valid.data)

accuracy(pred.All, valid.data$TOTAL.VALUE)

#  pred1           ME       RMSE      MAE       MPE     MAPE
# Test set      1.253675 48.04713 36.61889 -1.241814 9.446921


# Pred.All        ME          RMSE      MAE       MPE     MAPE
#Test set All -4.874143e-13 41.57133 31.88376 -1.063865 8.413145


# Since room and bedroom variables are not significant, we will drop them
# in our next model, reg3

#drop variables not significant . In this case - bedrooms and rooms

#write new model

reg3 = lm (TOTAL.VALUE ~ . -ROOMS -BEDROOMS , data = train.data)
summary(reg3)

pred3 = predict(reg3, newdata = valid.data)

accuracy (pred3, valid.data$TOTAL.VALUE)

# Pred 3      ME       RMSE      MAE        MPE     MAPE
#Test set 3   1.435063 41.932 32.19843 -0.7562762 8.499599


#Compare predictions by RMSE, ME...
#----------------------------------------------------------------------------
# 10) Stepwise regression to automate the process of dropping the predictors 
#we drop variables from the model not from the data set
#----------------------------------------------------------------------------


# Goal: Find parsimonious model (the simplest model that performs sufficiently well)
# It means more robust and higher predictive accuracy

# Stepwise regression approach has a search algorithm looking for the best or at least
# good subset of predictors out of all possible predictors


# To find the best subset: Use exhaustive search which may take a long time
# To find a good subset: Use forward, backward or stepwise search methods


#   We will examine how to find a good subset rather than the best subset (see package leaps for this)

#   All of these methods use one or multiple performance measures, including:
#
#   a) R2: Coefficient of determination, measuring the goodness of fit.
#      Percentage of variation in the outcome that the model is able to explain.
#   b) Adj-R2: Adjusted R-squared value is like R2 except it factors in the number
#      of predictors. Having too many predictors in the model is penalized in Adj-R2.
#   c) Akaike Information Criterion (AIC) and Schwartz's Bayesian Information
#      Criterion (BIC). They are like Adj-R2, based in information theory.
#      As such, they can be used to compare various models for the same data set.
#      AIC estimates the relative amount of information lost by a given model: 
#      the less information a model loses, the higher the quality of that model.
#      Thus, a lower AIC score indicates a relatively better model.
#   d) Mallow's Cp: It is based on the idea that good models are those
#      that have values of Cp near p + 1 and that have small p. 
#      p is the number of predictors in the model.


# Stepwise regression procedures has three slightly different operating procedures:
# 1) Forward selection, 2) Backward selection, and 3) both method at the same time
#    When making the selections, algorithm prefers smaller AIC values
#    which indicates a better model fit. 


#----------------------------------------------------------------------------
# 11) Forward Selection #no predictors only intercept. 
#----------------------------------------------------------------------------

# The algorithm starts with no predictors and
# then add predictors one by one. Each predictor added is the one
# (among all predictors) that has the largest contribution to the
# performance measure (typically adj-R2) on top of the predictors that are already in it.
# The algorithm stops when the contribution of additional predictors
# is not statistically significant.


# Use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)

# First, create a model with no predictors. See special notation below with "1"
# We will call the model reg.null

reg.null = lm(TOTAL.VALUE ~ 1, data = train.data) # regression null is an empty model. 1 means no predictor, dot/period means all)

# use step() to run forward regression.
# scope is to tell the computer to start with an empty model (reg.null) and
# up to all variables included, which is reg.All. Range of search area.


reg.forward <- step(reg.null, scope = list(lower = reg.null, upper = reg.All), direction = "forward") 

summary(reg.forward)  #we want AIC number to be lower. As you add each variable see how the AIC number changes. We stop when AIC starts to increase

#----------------------------------------------------------------------------
# 12) Backward Elimination
#----------------------------------------------------------------------------
# Reverse of forward selection, the procedure creates a regression model
# using ALL predictors first and calculates performance measures.
# Then, it drops one predictor that is not statistically significant. 
# The algorithm stops when all remaining predictors are significant.

reg.backward <- step(reg.All, direction = "backward")
summary(reg.backward)

#----------------------------------------------------------------------------
# 13) Stepwise Approach: both directions
#----------------------------------------------------------------------------
# A combination of forward and backward methods.
# The algorithm adds one predictor at each step like forward selection, but then considers dropping
# predictors that are not statistically significant at that moment, as in backward elimination.

# Start with the full model

reg.step <- step(reg.All, direction = "both")
summary(reg.step)

#----------------------------------------------------------------------------
# 14) An exercise: Boston Housing dataset
#----------------------------------------------------------------------------

d = read.csv("BostonHousing.csv")
View(d)

# Objective: Create a regression model to predict MEDV (median value of houses in a Boston neighborhood)

# 1) Create partitions: 60% training, and 40% validation

set.seed(14)

total.rows = dim(d)[1] #total number of rows

train.rows <- sample( c(1:total.rows), total.rows*0.7 ) #out of total I am taking 70% of them

train.data <- d[train.rows, ]
valid.data <- d[-train.rows, ]

# 2) Use stepwise regression to select good predictors


# a) Forward
# null model: reg.null
# full model: reg.full (except CAT.MED variable which is a derivative of MEDV)


reg.null = lm(MEDV ~1, data = train.data)
reg.full = lm(MEDV ~ .-CAT..MEDV, data = train.data)

reg.forward <- step(reg.null, scope = list(lower = reg.null, upper = reg.full), direction = "forward")

summary(reg.forward)

str(d) #how many predictors are there in the dataset use this code



# b) Backward

reg.backward <- step(reg.full, direction = "backward")
summary(reg.backward)


# c) Both

reg.step <- step(reg.full, direction = "both")
summary(reg.step)






