
# Download "ToyotaCorolla.csv" from Canvas.
# ToyotaCorolla.csv dataset shows used toyota cars for sale in 2004 in a dealer.
# Read the dataset into a dataframe by running the following code:
# The name of the dataset is "t".

t = read.csv("ToyotaCorolla.csv")
View(t)

# OUR MAIN JOB IS TO CREATE A PREDICTION MODEL 
# TO PREDICT "PRICE" OF A USED CAR WHEN SPECIFIC INFORMATION OF A
# USED CAR IS GIVEN

# QUESTIONS:

# 1) Get a summary of the variables in the dataset. 
#    Note variables with any extreme values (if any).

summary(t)

# 2) Check the dataset for missing values. How many?

sum(is.na(t)) #No missing values


# 3) How many rows and columns are there in the dataset?
dim(t) # rows = 1436 colums =  39


# 4) Are there any categorical variables that need to be converted to factor type?
#    If so, convert.

str(t)

t$Color = as.factor(t$Color)

t$Fuel_Type = as.factor(t$Fuel_Type) #overwrites the original remodel data to factors) - use this for nominal values



# 5) Which columns cannot be used as a predictor in the regression model?

#Id & Model

# 6) Create a scatter plot matrix for variables 3 through 10.
#    By examining the plot, suggest possible predictors 
#    for the outcome variable price.

plot(t[ , 3:10])
cor(t$Price, t$Age_08_04)
cor(t$Price, t$Mfg_Year)
cor(t$Price, t$KM)


# 7) By looking at the plot, can you tell multicolinearity is an issue?
#    If so, confirm it by calculating correlation coefficient between the two variables.

cor(t$Age_08_04, t$Mfg_Year) #-0.98


# 8) Create training and validation partitions
#    Training partition is 60% and validation partition is 40%
#    Use the following seed: 14


set.seed(14)

total.rows = dim(t)[1] 

train.rows <- sample( c(1:total.rows), total.rows*0.7 ) 

train.data <- t[train.rows, ]
valid.data <- t[-train.rows, ]

# 9) Develop a regression model predicting Price with the following
#    four predictors using the training partition
#    Name this model "reg1"
#    Predictors: Age_08_04, KM, Fuel_Type, HP

reg1 = lm (Price ~ Age_08_04 + KM + Fuel_Type + HP , data = train.data)
summary(reg1)

plot(reg1$residuals)
hist(reg1$residuals)

#The regression model is a good fit to the data

