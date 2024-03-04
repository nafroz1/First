# Introduction to Logistic Regression
# Explanation of logistic regression model structure

# Calculating odds and converting to probabilities
p=0.80 # Example probability calculation
odds = p/(1-p) # Calculating odds from probability
odds # Displaying odds
p = odds/(1+odds) # Converting odds back to probability
p # Displaying converted probability

# Graphing exponential functions
x=seq(-5,5, 0.1) # Defining x range for graphing exponential function
y=exp(x) # Calculating exponential function values
plot(x, y) # Plotting exponential function

# Graphing logarithmic functions
x=seq(0,10, 0.1) # Defining x range for graphing logarithmic function
y=log(x) # Calculating logarithmic function values
plot(x, y) # Plotting logarithmic function

# Example logistic regression model with a single predictor
B0 = -2 # Example coefficient B0
B1 = 0.1 # Example coefficient B1
X = 50 # Example predictor value
odds = exp(B0 + B1*X) # Calculating odds for logistic regression
odds # Displaying odds
p = odds/(1+odds) # Converting odds to probability
p # Displaying probability

# Data preparation and logistic regression model fitting
bank.df <- read.csv("datasets/UniversalBank.csv") # Loading dataset
bank.df <- bank.df[ , -c(1, 5)] # Dropping ID and ZIP Code columns
View(bank.df) # Viewing the dataset
str(bank.df) # Checking structure of the dataset

# Converting Education to categorical variable
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3), labels = c("Undergrad", "Graduate", "Advanced.Professional")) # Converting Education to factor
bank.df$Personal.Loan = factor(bank.df$Personal.Loan) # Converting Personal Loan to factor

# Data partitioning into training and validation sets
set.seed(2) # Setting seed for reproducibility
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6) # Sampling for training data
train.df <- bank.df[train.index, ] # Creating training dataset
valid.df <- bank.df[-train.index, ] # Creating validation dataset

# Fitting logistic regression model
logit.reg <- glm(Personal.Loan ~ . , data = train.df, family = "binomial") # Fitting logistic regression model
options(scipen=999) # Setting options to avoid scientific notation
summary(logit.reg) # Displaying model summary

# Predicting on validation set
logit.reg.pred <- predict(logit.reg, valid.df, type = "response") # Predicting probabilities on validation set
logit.reg.class <- factor(ifelse(logit.reg.pred >= 0.5, "1", "0")) # Classifying based on predicted probabilities

# Confusion matrix and performance metrics
confusionMatrix(logit.reg.class, factor(valid.df$Personal.Loan), positive = "1") # Creating confusion matrix

# Predicting personal loan status for new data
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
) # Defining new customer data
new.data.pred = predict(logit.reg, new.data, type = "response") # Predicting for new data
factor(ifelse(new.data.pred  >= 0.5, "1", "0")) # Classifying new data based on prediction
