#-------------------------------------------
# BANL6625 Data Mining
# Final Exam - SET B
#-------------------------------------------
# Name: 
#-------------------------------------------
# EXAM RULES: PLEASE READ.
# GRAD STUDENTS: ANSWER ALL FOUR PARTS.
# DO NOT SUBMIT WORD OR TEXT DOCUMENTS. IT HAS TO BE R 
# SO THAT I CAN RUN YOUR CODE.
# ANY ATTEMPT FOR SHARING YOUR WORK OR TEAM WORK WILL BE PENALIZED.
# SUBMIT YOUR FINISHED EXAM BY THE TIME IT IS DUE. 
# ANY LATE SUBMISSION WILL BE ASSESSED A LATENESS PENALTY.
#-------------------------------------------------------

#    A data set about births of babies in a hospital is collected. A doctor
#    is interested in predicting a newborn baby's weight as low (or normal)
#    using "lowbirthweight" variable as outcome, and all the remaining
#    variables as predictors. Read the dataset now:

d=read.csv("babyweight.csv")
View(d)

# When the "lowbirthweight" variable is 1, baby's weight is lower than normal, 
# and when it is 0, baby's weight is normal.

# The doctor is more interested in predicting low birth weight (i.e. category 1)

# The data is partitioned below for your convenience, as 70% for training and 30%
# for validation using seed number 2023. Run this code now:

set.seed(2023)
train.index <- sample( c(1:dim(d)[1]), dim(d)[1]*0.7 )
train.df <- d[train.index, ]
valid.df <- d[-train.index, ]

#------------- PART-1: Logistic Regression -------------

# 1) Create a logistic regression model using glm() function. 
#    Use training data including ALL predictors. Name your model logit.reg.
options(scipen=999)  # get rid of scientific notation with exp. numbers

logit.reg <- glm(lowbirthweight ~ . , data = train.df, family = "binomial")

# 2) Get a summary about your model. 
#    Which predictors are statistically significant at 10% level?

summary(logit.reg)

# 3) Now calculate predicted probabilities on validation set using your model.
#    Name it logit.reg.pred.

logit.reg.pred <- predict(logit.reg, valid.df, type = "response")


# 4) Convert predicted probabilities to 1 if probability >= 0.5, otherwise 0
#    using ifelse(). Name it logit.reg.pred.class.

logit.reg.pred.class = ifelse(logit.reg.pred >= 0.5, "1", "0"  )

# 5) Calculate confusion matrix on validation.
# What is the accuracy? What is the sensitivity? NOTE: Positive is "1".
library(caret)

confusionMatrix(as.factor(logit.reg.pred.class), as.factor(valid.df$lowbirthweight), positive = "1")

#Accuracy : 95%   
#Sensitivity : 67%        



# 6) A dataset of 10 mothers have arrived. (Download "newmothers.csv"
#     dataset from Canvas). Run the following code to read the dataset:

new.mothers=read.csv("newmothers.csv")
View(new.mothers)

# Now using your model (logit.reg), make predictions for the new mothers.
# Name this variable new.mothers.pred.

new.mothers.pred <- predict(logit.reg, valid.df, type = "response")

# 7) Convert predicted probabilities of new mothers to 1 if probability >= 0.5, otherwise 0
#    using ifelse(). Name your variable new.mothers.class. Which mothers are predicted
#    to have a low weight baby?

new.mothers.class <- factor(ifelse(new.mothers.pred >= 0.5, "1", "0"))

#24 mothers are predicted to have a low weight baby. 

#------------- PART-2: Classification Tree -------------
#    FOR THIS QUESTION, YOU WILL USE THE CLASSIFICATION TREE APPROACH 
#    USING THE SAME TRAINING AND VALIDATION DATA WITH THE SAME OBJECTIVE.

# Use classification tree packages
library(rpart)
library(rpart.plot)


# 8)  Develop a classification tree model using lowbirthweight as outcome, 
#     and all other variables as predictors in the training partition. 
#     Use default settings in rpart() function. Name this model default.ct.

default.ct <- rpart(lowbirthweight ~ . , data = train.df, method = "class")

# 9) Plot the resulting tree using prp() function.

prp(default.ct, type = 4, extra = 1, under = TRUE, split.font = 2, varlen = -10)


# 10) Apply the model on validation data using predict() function.
#    Name these predictions default.ct.pred.valid

default.ct.pred.valid <- predict(default.ct, valid.df, type = "class")


# 11) Generate the confusion matrix for the validation data. What is
#    the accuracy and sensitivity? NOTE: Positive class is "1"  (i.e. low birth weight)

confusionMatrix(default.ct.pred.valid, as.factor(valid.df$lowbirthweight), positive = "1")

#Accuracy : 94%          
#Sensitivity : 63%     


# 12) Now using your model, make predictions for the new mothers.
#    (i.e. use new.mothers dataset you used in the previous question).
#    Name this variable new.mothers.pred.ct. Which mothers are predicted
#    to have a low weight baby?

new.mothers.pred.ct <- predict(default.ct, new.mothers, type = "class")

#The mother with index 10 is predicted to have a low baby weight because the value for her prediction in new.mothers.pred.ct is 1, 
#which means that her predicted probability of having a low baby weight was greater than or equal to 0.5. 

#------------- PART-3: KNN -------------
#    FOR THIS QUESTION, YOU WILL USE THE K-NEAREST NEIGHBORS APPROACH 
#    USING THE SAME TRAINING AND VALIDATION DATA WITH THE SAME OBJECTIVE.

# To do the analysis conveniently, we create a separate vector for the lowbirthweight variable,
# naming the resulting dataframe as "outcome", and converted to factor. Run the following code:

outcome = as.data.frame( factor(d[, 8]) )
colnames(outcome) = c("lowbirthweight")
d <- d[ , -8]   # Drop lowbirthweight column from the dataset - not needed anymore

# We create dummies and standardize the dataset. Run the code below:

library(dummy)
dumd = dummy(d, int= TRUE) 
dd = data.frame(d, dumd)       # combine original data frame with dummies data frame
dd <- dd[ , -c(2,4,6,8,9,10)]  # no need for the original columns
View(dd)


# Normalize the predictor data. Run the code below:

library(caret)

norm.values <- preProcess(dd, method=c("center", "scale"))
dd.norm <- predict(norm.values, dd)  
View(dd.norm)   

# Partition the data into training (70%) and validation (30%). Run the code below:

set.seed(2023)
train.rows <- sample(row.names(dd.norm), 0.7*dim(dd.norm)[1])  
valid.rows <- setdiff(row.names(dd.norm), train.rows)  
train.df <- dd.norm[train.rows, ]
valid.df <- dd.norm[valid.rows, ]


# 13) Create a KNN model with k=1 using training set. Name it knn.pred.
library(FNN)

  knn.pred <- knn(train.df[, -3], test = valid.df[, -3], 
              cl = train.df$lowbirthweight, k = 1)
  
  


# 14) Generate the confusion matrix for the validation data. What is
#    the accuracy and sensitivity? NOTE: Positive class is "1"  (i.e. low birth weight)

confusionMatrix(knn.pred, as.factor(valid.norm.df$lowbirthweight), positive = "1")
 #RStudio not generating values 

# 15) Try another KNN model with k=3 using the training set. 
#    Generate the confusion matrix for the validation data. Which model is better? The one
#    with k=1 or k=5 ? Explain how you determined this.

knn3 <- knn(train.df[, -3], test = valid.df[, -3], 
                cl = train.df$lowbirthweight, k = 3)

confusionMatrix(knn3, as.factor(valid.norm.df$lowbirthweight), positive = "1")

knn5 <- knn(train.df[, -3], test = valid.df[, -3], 
            cl = train.df$lowbirthweight, k = 5)

confusionMatrix(knn5, as.factor(valid.norm.df$lowbirthweight), positive = "1")




#------------- PART-5: NAIVE BAYES -------------
#    FOR THIS QUESTION, YOU WILL USE THE NAIVE BAYES APPROACH 
#    USING THE SAME TRAINING AND VALIDATION DATA WITH THE SAME OBJECTIVE.
#    Read the dataset AGAIN and prepare it for the Naive Bayes:

d=read.csv("babyweight.csv")
View(d)

# When the "lowbirthweight" variable is 1, baby's weight is lower than normal, 
# and when it is 0, baby's weight is normal.

# The doctor is more interested in predicting low birth weight (i.e. category 1)

# Since all variables need to be categorical in Naive Bayes, 
# Convert mage, weeks, visits, gained variables into categorical 
# variables dividing by 10 and rounding. Run the following code:

d$mage = as.factor(round(d$mage/10))
d$weeks = as.factor(round(d$weeks/10))
d$visits = as.factor(round(d$visits/10))
d$gained = as.factor(round(d$gained/10))

d$lowbirthweight = factor(d$lowbirthweight)

# The data is partitioned below for your convenience, as 70% for training and 30%
# for validation using seed number 2022. Run this code now:

set.seed(2023)
train.index <- sample( c(1:dim(d)[1]), dim(d)[1]*0.7 )
train.df <- d[train.index, ]
valid.df <- d[-train.index, ]


# 16) Create a Naive Bayes model using the training set. 
#    Outcome variable is lowbirthweight and the rest are the predictors.
#    Call it d.nb
library(e1071)

d.nb <- naiveBayes( lowbirthweight ~ ., data = train.df)
d.nb  


# 17) Calculate class predictions on the validation set. Call it pred.class.
#    Hint: type is "class"

pred.class <- predict(d.nb, newdata = valid.df, type = "class")

# 18) Calculate confusion matrix on validation data. What is
#    the accuracy and sensitivity? Positive is "1".
library(caret)

confusionMatrix(pred.class, valid.df$lowbirthweight,positive = "1")

#Accuracy : 94%       
#Sensitivity : 67%        
        

# 19) Since you applied Logistic Regression, Classification Tree, KNN and Naive Bayes,
#    can you tell which one of these techniques is best for future predictions 
#    of low baby weight? Why?


#Since my computer was crashing I could not see accuracy and sensitivity values for the KNN part.Beyond that, it seems logistic regression has the high accuracy.

#------------- PART-4: Association Analysis -------------

# The Coursetopics data are for purchases of online courses at 
# an educational company. Each row represents the courses
# attended by a single student. The company wishes to assess
# alternative bundling of courses, i.e. which two or three courses
# go together well?
# Use association rules to analyze the data, and interpret several
# of the resulting rules.

library(arules)

course.df <- read.csv("Coursetopics.csv")
View(course.df)

# The data is already given as a binary incidence data frame. 

# 20) Convert this incidence data frame to matrix.

course.mat <- as.matrix(course.df)


# 21) Convert the incidence matrix into a transactions set format

course.trans <- as(course.mat, "transactions")

# 22) Plot a histogram of the data

itemFrequencyPlot(course.trans)

# 23) Run apriori procedure with minimum support 10 out of total 365 records
#    and minimum confidence 60%.

rules <- apriori(course.trans, 
                 parameter = list(supp= 10/365, conf = 0.6, target = "rules"))

# 24) Inspect rules sorted by lift and confidence ratio

inspect(sort(rules, by = c("confidence")))
inspect(sort(rules, by = c("lift")))

# 25) What courses are included in the first 3 rules? Copy them below:

#By confidence
#     lhs                         rhs        support    confidence coverage   lift     count
#[1] {Regression, SW}         => {Intro}    0.03835616 0.7000000  0.05479452 1.774306 14   
#[2] {Survey, SW}             => {Intro}    0.03287671 0.6666667  0.04931507 1.689815 12   
#[3] {Intro, DOE}             => {SW}       0.03013699 0.6470588  0.04657534 2.915759 11   

#By lift
#    lhs                         rhs        support    confidence coverage   lift     count
#[1] {DataMining, Regression} => {Cat.Data} 0.02739726 0.6250000  0.04383562 3.001645 10   
#[2] {Intro, DOE}             => {SW}       0.03013699 0.6470588  0.04657534 2.915759 11   
#[3] {Regression, SW}         => {Intro}    0.03835616 0.7000000  0.05479452 1.774306 14   



#------------------ END OF THE EXAM ------------------------------

