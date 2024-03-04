# Data loading and initial inspection
d=read_csv("C:/Users/Nahid/Desktop/R files/UniversalBank.csv") # Data loading step
View(d) # Data viewing step

# Data structure and missing value check
dim(d) # Checking dimensions of the dataset
str(d) # Checking structure of the dataset
sum(is.na(d)) # Checking for missing values

# Calculating naive prediction
table(d$Personal.Loan)/5000   # Calculating naive prediction percentage

# Removing irrelevant variables
d <- d[ , -c(1,5)] # Removing ID and ZipCode columns
View(d) # Viewing updated dataset

# Creating separate outcome vector
outcome = as.data.frame( d[, 8] ) # Creating outcome dataframe
colnames(outcome) = c("Personal.Loan") # Renaming outcome column
View(outcome) # Viewing outcome dataframe
outcome$Personal.Loan <- factor(outcome$Personal.Loan) # Converting outcome variable to factor

# Dropping Personal Loan column from predictors dataset
d <- d[ , -8] # Dropping Personal Loan column
View(d) # Viewing the updated dataset

# Converting Education to dummy variables
library(dummy) # Loading dummy package
d$Education = as.factor(d$Education) # Converting Education column to factor
dumEducation = dummy(d, int= TRUE) # Creating dummy variables for Education
View(dumEducation) # Viewing dummy variables
d = data.frame(d, dumEducation) # Merging original dataset with dummy variables
d$Education <- NULL # Removing original Education column

# Normalizing the predictor data
library(caret) # Loading caret library
norm.values <- preProcess(d, method=c("center", "scale")) # Normalizing data
d.norm <- predict(norm.values, d) # Applying normalization
View(d.norm) # Viewing normalized data

# Data partitioning into training and validation sets
set.seed(2023) # Setting seed for reproducibility
train.rows = sample( 1:total.rows, total.rows*0.6 ) # Sampling for training data
valid.rows = setdiff(1:total.rows, train.rows) # Calculating validation rows
train.df = d.norm[train.rows, ] # Creating training dataset
valid.df = d.norm[-train.rows, ] # Creating validation dataset

# k-NN model creation and application on validation set
library(FNN) # Loading FNN library
knn.pred <- knn(train = train.df, test = valid.df, cl = outcome[train.rows,], k = 1) # Applying k-NN with k=1

# Actual vs Predicted comparison
p=data.frame(Actual=outcome[valid.rows,], Predicted = knn.pred) # Creating comparison dataframe
View(p) # Viewing comparison

# Confusion matrix and accuracy calculation
a=confusionMatrix(knn.pred, outcome[valid.rows,], positive = "1") # Creating confusion matrix
a # Viewing confusion matrix

# Finding the best k value
mydata <- data.frame(k = seq(1, 20), accuracy = rep(0, 20), sensitivity = rep(0, 20), specificity = rep(0, 20)) # Initializing data frame
for(i in myk) { # Looping over k values
  myknn <- knn(train.df, test = valid.df, cl = outcome[train.rows,], k = i) # Applying k-NN model
  a = confusionMatrix(myknn, outcome[valid.rows,], positive = "1") # Generating confusion matrix
  mydata[i, 1] = i # Storing k value
  mydata[i, 2] = a$overall[1] # Storing accuracy
  mydata[i, 3] = a$byClass[1] # Storing sensitivity
  mydata[i, 4] = a$byClass[2] # Storing specificity
}
View(mydata) # Viewing results to decide on best k

# Predicting Personal Loan acceptance for a new customer scenario
new.customer = data.frame(
  Age = 40, Experience = 10, Income = 84, Family = 2, 
  CCAvg = 2, Education_1= 0, Education_2 = 1, Education_3 = 0,
  Mortgage = 0, Securities.Account = 0, CD.Account = 0, 
  Online = 1, CreditCard = 1
) # Defining new customer data
new.customer.norm <- predict(norm.values, new.customer) # Normalizing new customer data
knn.pred.new <- knn(train = train.df, test = new.customer.norm, cl = outcome[train.rows,], k = 1) # Applying k-NN model
p=data.frame(Actual="Unknown", Predicted = knn.pred.new) # Creating prediction outcome dataframe
p # Viewing prediction outcome

# ROC curve plotting
library(pROC) # Loading pROC library
r <- roc(actual, predictions ) # Creating ROC curve
plot.roc(r) # Plotting ROC curve
auc(r) # Calculating Area Under Curve (AUC)

# Propensity analysis and Lift Charts creation
owner.df <- read_csv("C:/Users/Nahid/Desktop/R files/ownerExample.csv") # Loading example data for propensity analysis
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.5, 'owner', 'nonowner')), as.factor(owner.df$Class), positive = 'owner') # Creating confusion matrix with different cutoff values
library(gains) # Loading gains library for Lift Charts
gain <- gains(owner.df$Actual, owner.df$Probability, groups=dim(owner.df)[1]) # Calculating gains
plot(c(0, gain$cume.pct.of.total*sum(owner.df$Actual)) ~ c(0, gain$cume.obs), xlab = "# cases", ylab = "Cumulative", type="l") # Plotting Lift Chart
