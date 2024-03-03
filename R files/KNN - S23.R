# Chapter-7
# K-Nearest Neighbors (k-NN) Method

# k-nearest-neighbors algorithm can be used
# for classification (of a categorical outcome)
# or prediction (of a numerical outcome).

# In this section, we focus on classification of a categorical outcome.
# For example, we will predict if the next person is a "buyer" or "non-buyer"
# as opposed to a numerical outcome (e.g. purchase amount) in regression.


# The "theme" or basic idea in k-nn is "you are who your friends are"
# or "you are who your neighbors are".
# If most of your neighbors' income is greater than $150k, probably your
# income level is also greater than $150k. 
# If most of your friends speak Spanish, you probably speak Spanish.
# and so on.

# The idea in k-nearest-neighbors methods is to identify k records in the training
# dataset that are similar to a new record that we wish to classify. We then use these
# similar (neighboring) records to classify the new record into a class, assigning the
# new record to the predominant class among these neighbors.

# Issues: 
# 1) How do you define the borders of the neighborhood? How many blocks, streets,
# houses are considered "in" the neighborhood?
# 2) How do you measure distance from you to your neighbors?
# 3) How do you determine the general characteristic of the neighborhood?
# 4) When we say "most of", does it mean 50% or more?


# K-NN is a nonparametric method because it does not involve estimation of parameters
# in an assumed function form, such as the linear form assumed in linear regression
# However, the method needs the training data available all the time in making classifications.


# Procedure:
# 1) Assume outcome variable Y has two levels: Y1 and Y2.
# 2) A new record will be labeled as Y1 or Y2 based on its predictors, X1, X2, ...predictors are numeric
# 3) Calculate the distance from the new record to the existing records (i.e. each neighbors)
# 4) Which ones have the shortest distance (i.e.closest neighbors)
# 5) Determine the borders of the neighborhood. i.e. how many closest records are considered "in" the
#    neighborhood. This is parameter k.
# 6) What is the general characteristics of these neighbors? Mostly Y1, or mostly Y2?
#    i.e. Count! What is the "majority vote"?
# 7) The new record is labeled as the majority characteristic (Y1 or Y2) of the neighbors.

# Distance calculation

# A central question is how to measure the distance between records based on
# their predictor values. The most popular measure of distance is the Euclidean
# distance. The Euclidean distance between two records, A(a1, a2, ... ap) and
# B(b1, b2,.. bp) with p number of predictors is:

#  SQRT( (a1 - b1)^2 + (a2 - b2)^2 + ...  (ap - bp)^2   )

# Although there are other distance measures, Euclidean is the most popular.

# Predictors should first be standardized before computing a Euclidean
# distance to prevent the dominance of the variables with large values.
# Not only the training data but the validation and new records are standardized
# as well, but the means and standard deviations used to standardize
# them should be those of the training data.
# Z-score based standardization: z = (x-mean)/stdev

# Determining k

# The simplest case is k = 1, where we look for the record that is closest
# (the nearest neighbor) and classify the new record as belonging to
# the same class as its closest neighbor.

# The idea of the 1-nearest neighbor can be extended to k > 1 neighbors as
# follows:
# 1. Find the nearest k neighbors to the record to be classified.
# 2. Use a majority decision rule to classify the record, where the record is
#    classified as a member of the majority class of the k neighbors.

# A simple example:
# A riding-mower (tractor) manufacturer would like to find a way of classifying families
# in a city into those likely to purchase a riding mower and those not likely to
# buy one.  

# A pilot random sample is taken of 12 owners and 12 nonowners in the city. 
# We first partition the data into
# training data (60% -> 14 households) and validation data (40% -> 10 households).

# install.packages("caret")  # this package provides functions for performance measures
# install.packages("FNN", dependencies = TRUE)  # this package provides knn procedure

library(caret)
library(FNN)


mower.df <- read.csv("C:/Users/Nahid/Desktop/R files/RidingMowers.csv")
View(mower.df)

set.seed(2023)
total.rows = dim(mower.df)[1]
train.rows <- sample( 1:total.rows, total.rows*0.6 )
train.data <- mower.df[train.rows, ]
valid.data <- mower.df[-train.rows, ]

## One new household in town!
new.df <- data.frame(Income = 60, Lot_Size = 20)
View(new.df)
## scatter plot for training
plot(train.data$Lot_Size ~ train.data$Income)
plot(Lot_Size ~ Income, data = train.data)
# This plot does not convey information on who is buyer or not.

# The argument "pch" in plot() below, is for plotting a character:
# 1 is for circle, 2 for triangle, 3 for plus sign, 4 is x

# owner -> o, nonowner -> x
plot(Lot_Size ~ Income, data=train.data, pch=ifelse(train.data$Ownership=="Owner", 1, 4))

# Now write the new household on the chart
text(new.df$Income, new.df$Lot_Size, "N")

# Now legend on the chart
legend("topright", c("owner", "non-owner", "newhousehold"), pch = c('o', 'x', 'N'))

# Write row number next to each dot (pos = 4 means right of the dot)
text(train.data$Income, train.data$Lot_Size, rownames(train.data), pos=4)

#K is closest neighbor. Always picking out odd numbers for K. Because majority wins. 

# After visually inspecting the plot, 
# if we use a 1-NN classifier, we would classify the new household as an owner,
# because household 4 is the nearest and it is an owner.

# If we use k = 3, the three nearest households are 4, 9, and 14, 
# as can be seen visually in the scatter plot.
# Two of these neighbors are owners of riding mowers, and one is a nonowner.
# The majority vote is therefore "owner" (2 out of 3), and the new
# household would be classified as an owner.

# Now lets do the same analysis using FNN package!

# Since income and size variables are in different magnitudes,
# we should normalize these two columns. 

library(caret)

# standardize the datasets
# use preProcess() function from the caret package to normalize Income and Lot_Size.
# method = "center" subtracts the mean of the data
# from the values (i.e, x - mean) 
# while method = "scale" divides the result by the standard deviation.
# Just like z = (x - mean)/s


norm.values <- preProcess(train.data, method=c("center", "scale")) #center = mean, standard dev. = scale
summary(norm.values)

# norm.values is like a placeholder or a model, holding important information about the normalized data
# More specifically, it keeps mean and sd calculated from the training set.
# These mean and sd are then used to standardize all the data.
# We do this using predict() function:

train.norm.df <- predict(norm.values, train.data)
valid.norm.df <- predict(norm.values, valid.data)
new.norm.df   <- predict(norm.values, new.df)

# See what it looks like:  
View(train.norm.df)

library(FNN)

# We use knn() function from FNN package, shown below:

# knn( train = training dataset columns, ie. predictors, test = new data columns, 
#         cl=true classification column, i.e. the outcome, k=number of neighbors)

# Let's try 3 nearest neighbors, k=3
# Third column is the target, so we exclude it in the training dataset

knn1 = knn(train = train.norm.df[ , -3], test = new.norm.df , cl = train.norm.df$Ownership , k=1, prob = TRUE)
knn1  # owner!


knn3 = knn(train = train.norm.df[ , -3], test = new.norm.df , cl = train.norm.df$Ownership , k=3, prob = TRUE)
knn3
summary(knn3)  # owner!


knn5 = knn(train = train.norm.df[ , -3], test = new.norm.df , cl = train.norm.df$Ownership , k=5, prob = TRUE)
knn5  # nonowner!

#------- How do we determine the best k value ---------------
# lower k -> possibility of overfitting (too specific - "variance" problem)
# higher k -> possibility of overgeneralization (too general - "noise/bias" problem) -> naive rule

# naive rule: the new record is classified as the majority characteristic
# in the dataset, ignoring predictor information. 

# Example: In a dataset of 1400 buyers and 1200 nonbuyers,
# the naive rule classification results in "buyer" due to buyer's majority
# It means, every new customer is classified as "buyer" 
# but this prediction is correct only 1400 out of (1400+1200) times
# Accuracy = 1400/(1400+1200) = 0.5384615
# Can we beat this accuracy with our data mining models, like KNN?


# Try different k values developed in training data, then
# apply it on validation data and calculate
# accuracy. Keep the one with the most accurate k. 
# Use this k for new predictions.


#---- Assessing the performance of the classification -----------

# confusionMatrix: A matrix showing what is predicted and what is actual

# confusionMatrix(pedicted, actual, positive = the class we are interested in)

#-------------------------------------------------
#                Actual Class (ie. Reference)
#                     C1         C2
# Predicted    C1     n11        n21
# Class        C2     n12        n22
#-------------------------------------------------

# n11 = number of C1 records classified correctly
# n12 = number of C1 records classified incorrectly as C2
# n21 = number of C2 records classified incorrectly as C1
# n22 = number of C2 records classified correctly

# Accuracy = (n11+n22)/(n11+n12+n21+n22)

# Example-1:
#                  Reference
#    Prediction   Nonowner  Owner
#     Nonowner      110      42
#     Owner         26       48

acc = (110+48)/(110+26+42+48)
errate = 1 - acc
errate = (26+42)/(110+26+42+48)


#--------- Sensitivity and Specificity ------------
# Sometimes it is more important to predict membership correctly 
# in one class than the other class. For example, predicting bankruptcy
# of a company seems to be a more significant task, 
# rather than predicting if it is in good shape.
# Say the important category for you is C1 (not C2)

# The "sensitivity" (also termed "recall") of a classifier is 
# its ability to detect the important class members correctly.

# Sensitivity = n11/(n11 + n12) = (number of correctly identified C1) / (total C1)

# What is the sensitivity for detecting "owners"? Calculate using the previous example.


# The "specificity" of a classifier is its ability to rule out C2 members correctly.
# specificity =  n22/(n21 +n22) = the percentage of C2 members classified correctly.

#----------------------------------------------------
#              Reference (Assuming positive = C1)
# Prediction          C1                  C2
#     C1        True Positive      False Positive
#     C2        False Negative     True Negative  
#----------------------------------------------------

# In medical diagnosis, sensitivity is the ability of 
# a test to correctly identify those with the disease
# (true positive rate = true positives out of all positives),
# whereas test specificity is the ability of the test to correctly 
# identify those without the disease (true negative rate = true negatives out of all negatives).


# Now let's apply the knn1 on validation dataset and calculate confusion matrix

#----- try k=1 on validation 
knn1 <- knn(train.norm.df[, -3], test = valid.norm.df[, -3], 
            cl = train.norm.df$Ownership, k = 1)

confusionMatrix(knn1, as.factor(valid.norm.df$Ownership), positive = "Owner") #being compared predictor and non predictor

#----- try k=3 on validation 
knn3 <- knn(train.norm.df[, -3], test = valid.norm.df[, -3], 
            cl = train.norm.df$Ownership, k = 3)

a=confusionMatrix(knn3, as.factor(valid.norm.df$Ownership), positive = "Owner")

a$overall[1]# how to find accuracy
a$byClass[1]#sensitivy
a$byClass[2]#specificity. picking up individual elements. 


#----- try k=5 on validation 
knn5 <- knn(train.norm.df[, -3], test = valid.norm.df[, -3], 
            cl = train.norm.df$Ownership, k = 5)

confusionMatrix(knn5, as.factor(valid.norm.df$Ownership), positive = "Owner")

# Can we automate this process? I.e increase k one by one using code automatically

myk = c(1,3,5,7,9)
t=1
mydata = data.frame(k=1:5, Accuracy = 1:5, Sensitivity = 1:5, Specificity = 1:5) #1-5 rows

View(mydata)



for (i in myk) {                             #looping func. 5 total cycles. c values are i 
  


myknn <- knn(train.norm.df[, -3], test = valid.norm.df[, -3], 
            cl = train.norm.df$Ownership, k = i)


a=confusionMatrix(myknn, as.factor(valid.norm.df$Ownership), positive = "Owner")

a$overall[1]# how to find accuracy
a$byClass[1]#sensitivy
a$byClass[2]#specificity. picking up individual elements. 

mydata[t,1] = i
mydata[t,2] = a$overall[1]
mydata[t,3] = a$byClass[1]
mydata[t,4] = a$byClass[2]
t = t+1
}