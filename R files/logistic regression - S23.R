# Logistic Regression

# Logistic regression is a popular method for classification of
# categorical outcomes. It is based on multiple linear regression.
# In multiple linear regression, we have a model that looks like this:

# Y = B0 + B1X1 + B2X2 + B3X3 + ... BqXq

# In logistic regression, right side of the equal sign remains the same.
# However, the outcome variable on the left side takes a different form.

# Here we will focus on only binary outcomes, such as 1/0, YES/NO,
# POSITIVE/NEGATIVE, BUYER/NONBUYER, FAIL/SUCCESS.
# Therefore, the outcome Y can take only 1 or 0.

# We will calculate a probability for a given record. If the probability is 
# greater than 0.5 (or any other cutoff set by us), we will predict that
# the outcome Y is 1. If the probability is less than 0.5, we will predict
# that the outcome Y is 0.

# How to calculate probabilities?

# Probability p can take any fractional value between [0,1]

# We cannot estimate it directly like:
# p = B0 + B1X1 + B2X2 + B3X3 + ... BqXq
# Because (B0 + B1X1 + B2X2 + B3X3 + ... BqXq) can take values outside [0,1]

# So, we need to convert B0 + B1X1 + B2X2 + B3X3 + ... BqXq to values 
# between [0,1]

# To do this, we use the concept of "odds". The odds of Y belonging to class 1 is:
# Odds = p/(1-p)
# Odds are guesses like how many times one contestant is superior to the other.
# 2 to 1, 4 to 1, 10 to 1, etc.

# Example:
# Probability of winning a game is 80%. What are the odds?
# Odds(Y=Win) is :

p=0.80
odds = p/(1-p)
odds
# odds = 4 (to 1).
4+1 = 5
# 4/5 is 0.80

# Vice versa, if you isolate p in the above formula, you can get
# p formula in terms of odds.
# p = Odds/(1+Odds)

# The odds of winning a game is 5 to 1. What is the
# probability of winning the game?
odds = 5
p = odds/(1+odds)
p

# When odds are 1, it means bets are 1 to 1, meaning equal powers competing,
# the probability p is 0.50.

# Exponential functions: y = 2^x, y = 3^x, ... y = e^x

x=seq(-5,5, 0.1)
y=exp(x)
plot(x, y)

# logarithmic function: y=log2(x), y=log3(x), ...  y=loge(x)
x=seq(0,10, 0.1)
y=log(x)
plot(x, y)


# Now we create a model that looks like this:
# Assume we have only one predictor, that is X, we write a model below:

# odds = e ^ (B0 + B1*X)

# It means: The odds of Y belonging to class 1 when predictor X is given.
# It is calculated as e number raised to power (linear regression model)
# Say, B0 = -2, B1 = 0.1, and X = 50, then odds would be
B0 = -2
B1 = 0.1
X = 50
odds = exp(B0 + B1*X)
odds

# It is like when X is 50, the odds of the record being classified as 1
# is 20.08 to 1. This corresponds to probability 95.2%:
p = odds/(1+odds)
p

# So we predict that when X is 50, the class is 1. Because 95.2% >= 50%

# Now we have a way to project any value into the probability range which is [0,1]

# This model, odds(Y=1|X) = e ^ (B0 + B1*X), is nice but
# we can get rid of e^ (i.e. power part) if we take a logarithm
# on both sides:

# Log( odds ) = (B0 + B1*X) * log(e)

# As you know, log(e) = 1, so

# Log( odds ) = B0 + B1*X

# Now the right side looks like totally linear, on which our previous
# knowledge of regression analysis can work. And, the left side,
# Log( odds(Y=1|X) ), is called a "logit"

# Let's write the general form of this model. This is our final 
# logistic regression model.

# Log(odds) = B0 + B1X1 + B2X2 + B3X3 + ... BqXq


# Procedure: 
# 1) Develop multiple regression model using concepts previously studied.
# 2) Convert the equation value, first to odds, then probabilities.
#     odds = e to power (B0 + B1X1 + B2X2 + B3X3 + ... BqXq)
#     p = odds/(odds+1)
# 3) Class is 1 if probability is greater than cutoff (i.e. 0.50)
#    Otherwise, class is 0.

# This procedure is automatically done by R packages!

# In regular linear regression, B coefficient is interpreted as:
# One unit increase in predictor X, is associated with B units of change
# in outcome Y on average (holding all other predictors constant).

# Now, we talk similarly:
# One unit increase in predictor X, is associated with a factor of e^B change
# on average in the odds (holding all other predictors constant).


# Example

# Predict if a customer would accept personal loan offer.

bank.df <- read.csv("datasets/UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.
View(bank.df)
str(bank.df)

# Treat Education as categorical; convert it to factors first. No worries about creating
# dummy variables because R's glm() function will create dummy variables automatically. 

# Instead of seeing numbers, let's be more specific. Use names instead.
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), 
                            labels = c("Undergrad", "Graduate", "Advanced.Professional"))

bank.df$Personal.Loan = factor(bank.df$Personal.Loan)

# partition data
set.seed(2)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]


# run logistic regression
# In logistic regression, the B coefficients are estimated using "Maximum Likelihood" method.
# In linear regression, "least squares" method is used.

# Use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.


logit.reg <- glm(Personal.Loan ~ . , data = train.df, family = "binomial")

options(scipen=999)

# See your model:
summary(logit.reg)


# Now use predict() with type = "response" to compute predicted probabilities
# on validation. 

logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

logit.reg.class <- factor(ifelse(logit.reg.pred >= 0.5, "1", "0"))

confusionMatrix( logit.reg.class, factor(valid.df$Personal.Loan), positive = "1")

# Accuracy : 0.9585 
# Sensitivity : 0.6543               
# Specificity : 0.9901  


# Predict personal loan status of new data:
new.data = data.frame(
  Age = 33,
  Experience = 15,   
  Income = 90,
  Family = 3,
  CCAvg = 2.20,
  Education = "Graduate",
  Mortgage = 110,
  Securities.Account = 0,
  CD.Account  = 0,    
  Online = 1,
  CreditCard = 1
)

new.data.pred = predict(logit.reg, new.data, type = "response")

factor(ifelse(new.data.pred  >= 0.5, "1", "0"))
# zero not a loan accepter


# 1) multiple regression for numerical outcomes, all predictors are numerical
# 2) knn for categorical outcomes, all predictors are numerical, standardized
# 3) naive bayes for categorical outcomes, all predictors are categorical
# 4) decision trees for categorical/numerical outcomes, no restriction on predictors
# 5) logistic regression for categorical outcomes, all predictors are numerical




