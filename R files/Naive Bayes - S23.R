# The Naive Bayes Method

# It is named after the Reverend Thomas Bayes (1702–1761). 
# To understand the naive Bayes classifier, we first look at the complete,
# or exact, Bayesian classifier. (See class notes in Excel)

# Since the predictor values could be so diverse, there may be
# instances that may not exist in the training set. Because of this
# issue, we do not use Exact Bayes. Instead, we use Naive Bayes,
# that is a good approximation of the exact Bayes.

# In naive Bayes approach, we associate each record with a probability 
# in the validation or new data. This is the probability (ie. propensity)
# that a record may belong to the class we are interested in.

# For example, 90% chance that a person could be a buyer.
# Possibly, for another person this probability is only 65%. However,
# we have to set a cutoff percentage and determine that anything beyond
# this cutoff is considered a "buyer", and anything lower than that is a
# "nonbuyer". In many situations, this cutoff is 50%.

# To use the Naive Bayes classier, the most important requirement
# is that ALL PREDICTORS AND OUTCOME MUST BE CATEGORICAL.

# If you have numerical variables, their values need to be "binned"
# i.e. grouped. For example, age variable is converted to binned_age
# in such a way that 1-10 is 1, 10-20 is 2, 20-30 is 3, and so on.

# An easier approach: age 17 is category 2 : 17/10 = 1.7 -> round(1.7) = 2

round(1.7)

# Alternatively, special packages and functions can be used to create bins.

# When using R packages, it is also recommended that ordinal
# categories should be converted to "factor". For example, student
# undergrad years could be 1 through 5 but these numbers need to be converted to
# categorical type (i.e. factor).

# Let's use the WestRoxbury dataset as an example.

d <- read.csv("C:/Users/Nahid/Desktop/WestRoxbury.csv", header = TRUE)    # load the data into memory
View(d)

# 1) Since the value of the house is numerical, we create a categorical
#    variable showing which houses are "expensive", meaning >= $600k

# 2) We saw that there are many numerical but discrete variables. We
#    converted them to factor type so that Bayesian function can use them
#    as categories.

# 3) Year built variable converted to categories in a simple method,
#    yielding categories of 18, 19, 20.

# data cleaning and fixing missing values

summary(d$YR.BUILT)    # there is a typo, year built is zero!

d = d[-1493, ]         # fix this!

# Create two categories using total value column. We will try to predict
# a house if expensive or normal priced.

d$TOTAL.VALUE.CAT = as.factor(ifelse(d$TOTAL.VALUE>= 600, "Expensive", "Normal"))

d$YR.BUILT = factor(round(d$YR.BUILT/100))   # categorical years

# Other categories created
d$FLOORS = factor(d$FLOORS)
d$ROOMS = factor(d$ROOMS)
d$FIREPLACE = factor(d$FIREPLACE)

# Let's use the categorical columns to create our model:
str(d)

# YR.BUILT, FLOORS , ROOMS, FIREPLACE, REMODEL, TOTAL.VALUE.CAT
selected.var <- c(4,7,8,13,14,15)

dim(d)

# Partition 60% to 40%
set.seed(123)
train.index <- sample(c(1:dim(d)[1]), dim(d)[1]*0.6)  
train.df <- d[train.index, selected.var]
valid.df <- d[-train.index, selected.var]

# We will use the naiveBayes() from the e1071 library - install the package if you don't have it
library(e1071)

d.nb = naiveBayes(TOTAL.VALUE.CAT ~ . , data = train.df)

d.nb

# Target's categories in the training (ie. A-priori probabilities) :
table(train.df$TOTAL.VALUE.CAT)/dim(train.df)[1]

# Now let's apply naive Bayes model for prediction

# predict probabilities on the validation set
# type is "raw" meaning probability values is requested.

str(d$TOTAL.VALUE.CAT)

pred.prob = predict(d.nb, newdata = valid.df, type = "raw")    # calculate probability of "Expensive" and "Normal"

pred.class = predict(d.nb, newdata = valid.df, type = "class")  # Calculate predicted classes in the validation

results = data.frame(actual = valid.df$TOTAL.VALUE.CAT, predicted=pred.class, probability = pred.prob )

View(results)

# confusion matrix on the training partition
pred.class <- predict(d.nb, newdata = train.df,type = "class")
confusionMatrix(pred.class, train.df$TOTAL.VALUE.CAT, positive = "Expensive")

# Accuracy : 0.9595  
# Sensitivity : 0.44304         
# Specificity : 0.98405 

# confusion matrix on the validation partition
pred.class = predict(d.nb, newdata = valid.df, type = "class")  # Calculate predicted classes in the validation
confusionMatrix(pred.class, valid.df$TOTAL.VALUE.CAT, positive = "Expensive")

# Accuracy : 0.9599   
# Sensitivity : 0.49412 
# Specificity : 0.97764 

# Changing the cutoff from 0.5 to 0.75

# confusion matrix on the validation partition using cutoff=0.75 for positive category

pred.prob = predict(d.nb, newdata = valid.df, type = "raw")   # get propensities
pred.prob = data.frame(pred.prob)

pred.class75 = as.factor(ifelse(pred.prob75$Expensive >= 0.75, "Expensive","Normal" ))
confusionMatrix(pred.class75, valid.df$TOTAL.VALUE.CAT, positive = "Expensive")

#Increase sensitivity = by changing cutoff

# Changing the cutoff from 0.5 to 0.25

pred.class = as.factor(ifelse(pred.prob$Expensive >= 0.25, "Expensive","Normal" ))
confusionMatrix(pred.class, valid.df$TOTAL.VALUE.CAT, positive = "Expensive")

# Gain/Lift Chart

# first option with 'caret' library:
library(caret)

pred.prob = predict(d.nb, newdata = valid.df, type = "raw")   # get propensities
pred.prob = data.frame(pred.prob)

mylift <- lift(relevel(valid.df$TOTAL.VALUE.CAT, ref="Expensive") ~ pred.prob$Expensive)
xyplot(mylift, plot = "gain")

#smaller shaded area shows mistakes we made, the model isnt working for us too well. But not the worst either.

# we can do K-NN graph and super impose on this plot and see which one is working better (just a tit bit)

# ROC
library(pROC)

myroc <- roc(results$actual , results$probability.Expensive, levels = c("Expensive","Normal"))

plot(myroc, main = "ROC curve for the model",
       col = "blue", lwd = 2, legacy.axes = TRUE)    # if add = TRUE, you can add more charts on top of each

#If you flip the graph - it will go from 1minus specificity to specificity. 

#Be away from the straight line in the graph. Closer to the line worse the model. Look at pic in phone. Area under curve (AUC)

auc(myroc)

dev.
