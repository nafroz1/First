
# Download "ToyotaCorolla.csv" from Canvas.
# ToyotaCorolla.csv dataset shows used toyota cars for sale in 2004 in a dealer.
# Read the dataset into a dataframe by running the following code:
# The name of the dataset is "t".

t = read.csv("ToyotaCorolla.csv")
View(t)

# OUR MAIN JOB IS TO CREATE A PREDICTION MODEL 
# TO PREDICT "PRICE" OF A USED CAR WHEN SPECIFIC INFORMATION OF A
# USED CAR IS GIVEN

# IMPORTANT NOTE: 
# This homework continues from your previous homework. If you have not done
# the following operations on the data, do so now.

# correct data types
t$Fuel_Type = as.factor(t$Fuel_Type)
t$Color = as.factor(t$Color)

# split the dataset
set.seed(14)
total.rows = dim(t)[1]
train.rows <- sample( c(1:total.rows), total.rows*0.60 )
train.data <- t[train.rows, ]
valid.data <- t[-train.rows, ]

#    Develop a regression model 

reg1 = lm(Price ~ Age_08_04 + KM + Fuel_Type + HP, data = train.data)

summary(reg1)

# QUESTIONS

# 1) Are the regression coefficients statistically significant?

summary(reg1)

  #All coefficients are statistically significant except Fuel_TypePetrol 
#categorical values are automatically changed to dummies

table(t$Fuel_Type)

#we will take Diesel as it is but Petrol is insignificant so we will combine it with invisible variable CNG
#implicit base is CNG

# 2) Is the model as a whole statistically significant?

#Yes  - based on main p value.


# 3) Comment on Adjusted R-squared value.

#82% variation is explained by the model. ADJUSTED R means the model captures 82% of variation in the price variable

# 4) Check normality of the residuals by visual inspection

plot(reg1$residuals)
hist(reg1$residuals)

#normal distribution


# 5) Calculate accuracy measures of the model using the training partition
library(forecast)

pred1 = predict(reg1, newdata = train.data)
accuracy(pred1, train.data$Price)

#pred 1 
#ME                 RMSE                  
# 2.113009e-11     1611.5   


# 6) Calculate accuracy measures of the model using the validation partition
pred2 = predict(reg1, newdata = valid.data)
accuracy(pred2, valid.data$Price)

#pred2
#ME               RMSE            
# -48.11071    1292.712   

# 7) Comment on how different ME and RMSE calculated in the training
#     and validation partitions.

#RMSE in training  and validation data is pretty close
#ME in validation data is overpredicting 

#Error = Actual - Predictor
 #= ME is big means overpredicting . Predictor value is big.


#If RMSE is too big in valid data compared to train data. This means something is wrong. 
# If RMSE in training data is smaller than valid data, it means the model is unusually perfect. This has an over fitting problem.


# 8) Apply the regression model on the following dataset to predict Price.

# Age_08_04 = 20,
# KM = 25000, 
# Fuel_Type = "Diesel",
# HP = 90



new.car = data.frame(
  
  Age_08_04 = 20,
  KM = 25000, 
  Fuel_Type = "Diesel",
  HP = 90

)

predict(reg1, newdata = new.car) 

#Price = 18073.56 



#Alternate 

a = data.frame(
  Age_08_04 = 20,
  KM = 25000, 
  Fuel_Type = "Diesel",
  HP = 90
  
)

predict (reg1,a)