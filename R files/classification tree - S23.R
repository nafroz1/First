# Classification and Regression Trees

# This is a flexible and data-driven method that can be used for
# both classification (called classification tree) 
# and prediction (called regression tree).

# Among the data-driven methods, trees are the most transparent
# and easy to interpret. Trees are based on separating records into
# subgroups by creating splits on predictors. These splits create
# logical rules that are transparent and easily understandable,
# for example, “IF Age < 55 AND Education > 12 THEN class = "Buyer".

# A tree based procedure basically creates this kind of rules to be
# used later for classification or prediction of new data.

# How does the procedure work?
# Assume that there are two predictors, X1 and X2, with an outcome
# variable Y which takes two categories only. When X1 is in the
# horizontal axis, and X2 is the vertical axis, Y is plotted as
# black or red dots representing each class. The procedure examines
# X1 or X2 values and figure a suitable split point, separating 
# the plot into two parts (i.e. rectangles).
# What is inside each part are black and red dots. But the 
# split is so chosen, that in each part, there are less "impurity" 
# overall than before the split. This means each part is mostly black
# or mostly red. Then, the procedure is applied again
# on each rectangle, creating more parts. The procedure tries to make
# each part all black, or all red ideally. Since these dots probably
# all mixed up everywhere, this ideal cannot be reached but the algorithm
# should stop after reaching some satisfactory "purity" level.

# A completely pure rectangle is one that is composed of a single class (e.g., owners). 

# How to measure impurity?
# There are a number of ways to measure impurity. The two most popular
# measures are the Gini index and an entropy measure.
# Gini index takes values between 0 (when all the records belong to the same class)
# and (m−1)/m (when all m classes are equally represented). m is number of classes in outcome.
# We don't want equal representation in this approach. We want one class
# to take the majority in a rectangle (actually, best is all records 
# belonging to the same class. Total homogeneity). So, any
# rectangle with a Gini value closer to zero is near pure.
# Entropy index has a similar approach but use a slightly different formula.


# Example1

# Package rpart
# Recursive Partitioning and Regression Trees (RPART)
# is R's implementation of Classification and Regression Trees (CART)

library(rpart)
library(rpart.plot)
mower.df <- read.csv("RidingMowers.csv")
View(mower.df)


plot(mower.df$Lot_Size ~ mower.df$Income,
     col = ifelse(mower.df$Ownership == "Owner", "red", "black"))
abline(v=58, col="blue")
abline(h=18, col="blue")

# Use rpart() to run a classification tree.
# method parameter: classification, or prediction

class.tree <- rpart(Ownership ~ . , data = mower.df, method = "class")

# Use prp() function to plot the tree. 
# You can control plotting parameters such as color, shape, 
# and information displayed (which and where).

prp(class.tree, type = 4, extra = 1, split.font = 2, varlen = -8) 


# Decision Rules created. For example:
# IF INCOME < 60, THEN this record is NONOWNER
# (because of majority voting 7 to 1 nonowner wins)

mower.df[mower.df$Income< 60, "Ownership"]

table(mower.df[mower.df$Income< 60, "Ownership"])   # Nonowner=7, Owner=1


# We have two types of nodes in a tree: decision nodes and terminal nodes.
# Nodes that have successors are called decision nodes.
# Nodes that have no successors are called the terminal nodes (or leaves of the tree)


# You can tell the computer to grow the tree (i.e. keep splitting).
# The code below instructs the algorithm to grow the tree branches
# until minimum number of records in each "leaf" hits 1 (i.e.full tree),
# or desired purity is reached.


# minsplit = 1
# minsplit = the minimum number of observations (ie. rows) that must exist in a node
# in order for a split to be attempted.

class.tree <- rpart(Ownership ~ ., data = mower.df, 
                    control = rpart.control(minsplit = 1),
                    method = "class") #grow the tree as much as you can/ keep branching the tree until you meet 1(minsplit)


prp(class.tree, type = 4, extra = 1, split.font = 2, varlen = -8) 


# "dropping a new record down the tree" : Follow the decision rules in the tree
# to predict the new record.

# The class with the highest vote is assigned to the new record. 

# Usual training and validation partitions are used. However,
# depending on what is in the training partition, tree structure can be
# quite unstable, shifting substantially depending on the sample chosen.
# Also, a fully-fit tree will likely lead to overfitting.

# So, to avoid these problems, the model needs to be tuned.


# Example2

# The goal in Universal Bank analysis is to model the previous campaign’s
# customer behavior to see what combination of factors make a customer
# more likely to accept a personal loan. 
# This will serve as the basis for the design of a new campaign. 

# Our predictive model will be a classification tree.
# Partition the data into training and validation and apply the 
# classification tree approach.
# The bank’s dataset includes data on 3000 customers.
# install the following packages: rpart and rpart.plot

library(rpart)
library(rpart.plot)

bank.df <- read.csv("C:/Users/Nahid/Desktop/UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)]  # Drop ID and zip code columns.
View(bank.df)


# partition
set.seed(1)  
total.rows = dim(bank.df)[1]
train.rows = sample(1:total.rows, total.rows*0.6)  
valid.rows = setdiff(1:total.rows, train.rows)
train.df = bank.df[train.rows, ]
valid.df = bank.df[-train.rows, ]


# Create a classification tree model using all default values of the procedure
default.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class")
# plot tree
prp(default.ct, type = 4, extra = 1, under = TRUE, split.font = 2, varlen = -10)

# Want to see all the details about each node?
summary(default.ct)

# Using functions's default values, the procedure made 7 splits and then stopped.

# The top node refers to all the records in the training set, 
# of which 361 customers did not accept the loan and 37 customers 
# accepted the loan. The “0” in the top node’s rectangle represents the majority
# class (“did not accept” is 0).

# The first split, which is on the income variable, generates left and
# right child nodes.

# The eventual classification of customer appears in the terminal nodes.
# Of the five terminal nodes, three lead to classification of “did not accept”
# and two lead to classification of “accept.” 


#------- Fully grown tree using cp=0, and minsplit = 1 ----------------

# cp = complexity parameter. Any split that does not decrease the overall
# lack of fit by a factor of cp is not attempted. In other words, 
# it is the required improvement amount to make a split.
# For instance, in a regression tree, this means that the overall R-squared
# must increase by cp at each step. 

# with zero cp = grow to the fullest extent, just split as many as it can
full.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", cp = 0, minsplit = 1)

prp(full.ct, type = 4, extra = 1, under = TRUE, split.font = 2, varlen = -10)


# with a bigger but not a full tree, use a cp different than zero
deeper.ct <- rpart(Personal.Loan ~ ., data = train.df, method = "class", cp = 0.009, minsplit = 1) #change CP and see how trees are different

prp(deeper.ct, type = 4, extra = 1, under = TRUE, split.font = 2, varlen = -10)

# When the tree grows bigger, chance of overfitting., losing generalization and prediction power
# When the tree is too small, the model too general -> prediction performance gets closer to naive method


# --------------------------------------------
# Back to default.ct model

# See how it performs on the training and validation

# classify records in the training data.
# set argument type = "class" in predict() to generate predicted class membership.

default.ct.pred.train <- predict(default.ct, train.df, type = "class")
library(caret)
# generate confusion matrix for training data
confusionMatrix(default.ct.pred.train, as.factor(train.df$Personal.Loan), positive = "1")


### repeat the code for the validation set
default.ct.pred.valid <- predict(default.ct, valid.df, type = "class")

# generate confusion matrix for validation data
confusionMatrix(default.ct.pred.valid, as.factor(valid.df$Personal.Loan), positive = "1")


# With the deeper tree, how is the prediction performance?

### repeat the code for the validation set
deeper.ct.pred.valid <- predict(deeper.ct, valid.df, type = "class")

# generate confusion matrix for validation data
confusionMatrix(deeper.ct.pred.valid, as.factor(valid.df$Personal.Loan), positive = "1")


# Overfitting is observed with the deeper tree with so many splits.
# One reason a large tree may overfit is that its final splits are based
# on very small numbers of records. 

# Need to figure out to avoid overfitting and stop the overgrowth
# It is difficult to determine what is a good stopping point

# Methods to stop the growth of the tree:
# 1) CHAID: Chi-squared automatic interaction detection based on Chi-squared
# test of independence. Splits are made based on statistical significance.
# If there is no more contribution to statistical significance, the procedure 
# is stopped.

# 2) CART, MARS and C4.5: Classification And Regression Tree approaches 
#    based on pruning branches of a full tree. They have proven to be more
#    successful than stopping tree growth.

#    These procedures use validation data to prune back the tree that has
#    deliberately been overgrown using the training data.
#    Pruning consists of successively selecting a decision node and 
#    re-designating it as a terminal node.

# Cross-Validation: Using the same data set for training and validation.
# Validation part can be generated randomly out of training dataset.
# Each set generated this way is also called "folds".

# The problem with randomly picking up a training partition
# is that each random sample may generate a totally different tree.
# To minimize this issue, multiple samples using the same training data 
# are taken automatically and some measure called complexity parameter (CP) 
# is recorded in each tree. 
# Later, the values of the measure are analyzed and an overall good tree is determined.

# CP is calculated as a composite of misclassification errors and penalty of having
# a big tree. As CP goes smaller, the tree becomes full grown.


#Decision tree approach is very well established and used in many places due to 
#its simplicity and transparency. More algorithms are develiped combining other techniques
#with decision trees. One suh technique is called "random forests"


d <- read.csv("C:/Users/Nahid/Desktop/WestRoxbury.csv", header = TRUE)
View(d)

d$TOTAL.VALUE.CAT =ifelse(d$TOTAL.VALUE > 600, "Expensive", "Normal")

d$TOTAL.VALUE <- NULL
d$TAX <- NULL

set.seed(1)  
total.rows = dim(d)[1]
train.rows = sample(1:total.rows, total.rows*0.6)  
valid.rows = setdiff(1:total.rows, train.rows)
train.df = d[train.rows, ]
valid.df = d[-train.rows, ]

default.ct <- rpart(TOTAL.VALUE.CAT ~ ., data = train.df, method = "class")
# plot tree
prp(default.ct, type = 4, extra = 1, under = TRUE, split.font = 2, varlen = -10)

default.ct.pred.train <- predict(default.ct, train.df, type = "class")

summary(default.ct)

library(caret)

##repeat the code for training set 
default.ct.pred.train <- predict(default.ct, train.df, type = "class")

# generate confusion matrix for training data
confusionMatrix(default.ct.pred.train, as.factor(train.df$TOTAL.VALUE.CAT), positive = "Expensive")

##repeat the code for valid set
default.ct.pred.valid <- predict(default.ct, valid.df, type = "class")

confusionMatrix(default.ct.pred.valid, as.factor(valid.df$TOTAL.VALUE.CAT), positive = "Expensive")
