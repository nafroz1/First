t = read.csv("ToyotaCorolla.csv")
View(t)

summary(t)

sum(is.na(t)) # No missing values

dim(t) # rows = 1436, columns = 39

str(t)

t$Color = as.factor(t$Color)
t$Fuel_Type = as.factor(t$Fuel_Type)

str(t)

# Exploratory Data Analysis (EDA) via Scatter Plot Matrix
plot(t[, 3:10])
cor(t$Price, t$Age_08_04)
cor(t$Price, t$Mfg_Year)

# Multicollinearity Check
cor(t$Age_08_04, t$Mfg_Year) # -0.98, 98%

# Data Partitioning into Training and Validation Sets

set.seed(14)
total.rows = dim(t)[1]
train.rows <- sample(c(1:total.rows), total.rows*0.7)
train.data <- t[train.rows, ]
valid.data <- t[-train.rows, ]

# Regression Model Development
reg1 = lm(Price ~ Age_08_04 + KM + Fuel_Type + HP, data = train.data)
summary(reg1)

plot(reg1$residuals)
hist(reg1$residuals)


#The regression model is a good fit to the data

