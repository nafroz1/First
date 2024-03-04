# Introduction to Naive Bayes Method
# Loading dataset and initial inspection
d <- read.csv("C:/Users/Nahid/Desktop/WestRoxbury.csv", header = TRUE) # Load dataset
View(d) # View dataset

# Data preprocessing
round(1.7) # Example rounding for binning

# Dataset preparation for Naive Bayes
# House value categorization
d$TOTAL.VALUE.CAT = as.factor(ifelse(d$TOTAL.VALUE>= 600, "Expensive", "Normal")) # Categorizing house value

# Year built conversion to categorical
d$YR.BUILT = factor(round(d$YR.BUILT/100)) # Converting year built to categorical

# Converting discrete variables to factor type
d$FLOORS = factor(d$FLOORS)
d$ROOMS = factor(d$ROOMS)
d$FIREPLACE = factor(d$FIREPLACE)

# Data selection for modeling
str(d) # Structure of dataset to select variables
selected.var <- c(4,7,8,13,14,15) # Selecting variables for model

# Data partitioning
set.seed(123) # Setting seed for reproducibility
train.index <- sample(c(1:dim(d)[1]), dim(d)[1]*0.6) # Sampling for training data
train.df <- d[train.index, selected.var] # Creating training dataset
valid.df <- d[-train.index, selected.var] # Creating validation dataset

# Naive Bayes model training
library(e1071) # Loading e1071 package for naiveBayes function
d.nb = naiveBayes(TOTAL.VALUE.CAT ~ . , data = train.df) # Training Naive Bayes model

# Viewing model details
d.nb # Viewing trained model

# Examining a-priori probabilities
table(train.df$TOTAL.VALUE.CAT)/dim(train.df)[1] # Viewing a-priori probabilities

# Predicting with Naive Bayes model
pred.prob = predict(d.nb, newdata = valid.df, type = "raw") # Predicting probabilities
pred.class = predict(d.nb, newdata = valid.df, type = "class") # Predicting classes

# Combining actual and predicted results for comparison
results = data.frame(actual = valid.df$TOTAL.VALUE.CAT, predicted=pred.class, probability = pred.prob)
View(results) # Viewing prediction results

# Confusion matrix for training data
pred.class <- predict(d.nb, newdata = train.df,type = "class")
confusionMatrix(pred.class, train.df$TOTAL.VALUE.CAT, positive = "Expensive") # Training data confusion matrix

# Confusion matrix for validation data
pred.class = predict(d.nb, newdata = valid.df, type = "class")
confusionMatrix(pred.class, valid.df$TOTAL.VALUE.CAT, positive = "Expensive") # Validation data confusion matrix

# Adjusting prediction cutoff for sensitivity analysis
pred.class75 = as.factor(ifelse(pred.prob$Expensive >= 0.75, "Expensive","Normal" ))
confusionMatrix(pred.class75, valid.df$TOTAL.VALUE.CAT, positive = "Expensive") # Using higher cutoff

pred.class25 = as.factor(ifelse(pred.prob$Expensive >= 0.25, "Expensive","Normal" ))
confusionMatrix(pred.class25, valid.df$TOTAL.VALUE.CAT, positive = "Expensive") # Using lower cutoff

# Gain/Lift Chart analysis
library(caret) # Loading caret library for lift chart
mylift <- lift(relevel(valid.df$TOTAL.VALUE.CAT, ref="Expensive") ~ pred.prob$Expensive)
xyplot(mylift, plot = "gain") # Plotting gain/lift chart

# ROC curve analysis
library(pROC) # Loading pROC library for ROC curve
myroc <- roc(results$actual , results$probability.Expensive, levels = c("Expensive","Normal"))
plot(myroc, main = "ROC curve for the model", col = "blue", lwd = 2, legacy.axes = TRUE) # Plotting ROC curve
auc(myroc) # Calculating Area Under Curve (AUC)
