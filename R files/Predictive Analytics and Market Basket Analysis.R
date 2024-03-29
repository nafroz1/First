# Logistic Regression for Accident Severity Prediction

# The file accidents.csv contains a subset of information on actual automobile
# accidents in 2001 in the United States that involved one of three 
# levels of injury: NO INJURY, INJURY, or FATALITY. For each accident,
# additional information is recorded, such as day of week, weather conditions,
# and road type. 

# A firm might be interested in developing a system for quickly 
# classifying the severity of an accident based on initial reports
# and associated data in the system. Read the data now from Canvas.


d = read.csv("C:/Users/Nahid/Desktop/accidents.csv")
View(d)

# Our goal here is to predict whether an accident just reported will
# involve an injury (MAX_SEV = "fatal" or "non-fatal"),
# or will not (MAX_SEV = "no-injury" ).

# Variable definitions:

# rush hour	1=rush hour, 0=not (rush = 6-9 am, 4-7 pm)
# WRK_ZONE	1= yes, 0= no
# WKDY	1=weekday, 0=weekend
# INT_HWY	Interstate? 1=yes, 0= no 
# LGTCON	Light conditions 1=day, 0=other
# LEVEL   Road level 1= level, 0=other
# SPD_LMT Speed limit, miles per hour
# SUR_COND_DRY Surface conditions 1=dry, 0= other
# TRAF_Two_way 1=two-way traffic, 0=other
# WEATHER_adverse 1=adverse, 0=other


# First, we create the outcome variable, INJURY, which has only two levels: yes or no.

d$INJURY = factor( ifelse(d$MAX_SEV == "no-injury", "NO", "YES" ) )

# We are more interested in correctly identifying fatal or injury cases, 
# which is YES category of the INJURY variable. 

# Now removing unnecessary MAX_SEV variable
d$MAX_SEV <- NULL

View(d)

# Partition the data

set.seed(2023)
train.index <- sample( c(1:dim(d)[1]), dim(d)[1]*0.6 )
train.df <- d[train.index, ]
valid.df <- d[-train.index, ]

library(rpart)
library(rpart.plot)

# QUESTIONS:

# NOW use the Logistic Regression approach for the accidents problem.

# 1) Create a logistic regression model using glm() function 
#    using training data including ALL predictors. Name your model logit.reg.
View(d)
options(scipen=999)

logit.reg <- glm(INJURY ~ . , data = train.df, family = "binomial")




# 2) Get a summary about your model.

summary(logit.reg)

# 3) Now calculate predicted probabilities on validation set using your model. Call it logit.reg.pred.
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")





# 4) Convert predicted probabilities to "YES" if probability >= 0.5, otherwise "NO", using ifelse()
#    Call it logit.reg.pred.class
logit.reg.pred.class <- factor(ifelse(logit.reg.pred >= 0.5, "YES", "NO"))


# 5) Calculate confusion matrix on validation. HINT: Use as.factor()
#    What is the accuracy? What is the sensitivity? NOTE: Positive is "YES".
library(caret)

confusionMatrix( logit.reg.pred.class, as.factor(valid.df$INJURY), positive = "YES")
# Accuracy : 0.4917 
# Sensitivity : 0.6726              
# Specificity : 0.3307  


# 6) Predict the injury status of a new traffic accident using
#     the following data in new.df dataframe. What is your prediction?
#     Injury yes or no?

new.df = data.frame(
  
  RushHour = 1,
  WRK_ZONE = 1,
  WKDY = 1,
  INT_HWY = 1,
  LGTCON_day = 0,
  LEVEL = 1,
  SPD_LIM = 6,
  SUR_COND_dry = 0,
  TRAF_two_way = 0,
  WEATHER_adverse = 0
  
)
new.df.pred = predict(logit.reg, new.df, type = "response")

ifelse(new.df.pred  >= 0.5, "YES", "NO")
#injury YES

# PART-2


# A drug store chain wants to learn more about cosmetics buyers
# purchase patterns. Specifically, they want to know what items are 
# purchased in conjunction with each other, for purposes of display, 
# point of sale special offers, and to eventually implement a real time
# recommender system to cross-sell items at time of purchase.

# The data are in the form of a matrix in which each column represents
# a product group, and each row a customer transaction.

# Read this small dataset below:

cos.df <- read.csv("Downloads/Cosmetics.csv")
View(cos.df)

library(arules)

# QUESTIONS

# 1)  Delete the first column.

cos.df<-(cos.df[,-1])

# 2) Convert it to a matrix.

cos.df.mat<- as.matrix(cos.df)


# 3) Convert the matrix into a transactions set format.
cos.trans <- as(cos.df.mat, "transactions")


# 4) Inspect what it looks like.

inspect(cos.trans)




# 5) Plot a histogram (frequency plot) of the data.

itemFrequencyPlot(cos.trans)


# 6) Run apriori procedure with minimum support 100 (out of total 1000 records)
#    and minimum confidence 60%.

rules <- apriori(cos.trans, parameter = list(supp= 100/1000, conf = 0.6, target = "rules"))



# 7) How many rules are generated by the procedure?

print(rules) #77 rules

# 8) Inspect rules sorted by confidence ratio

inspect(sort(rules, by = "confidence"))


# 9) Copy paste the first 3 rules below.
#    lhs                                       rhs            support confidence coverage lift    
#[1]  {Brushes}                           => {Nail.Polish} 0.149   1.0000000  0.149    3.571429
#[2]  {Blush, Concealer, Eye.shadow}      => {Mascara}     0.119   0.9596774  0.124    2.688172
#[3]  {Blush, Eye.shadow}                 => {Mascara}     0.169   0.9285714  0.182    2.601040



# 10) Write the meaning of the first rule.

#The first rule states that if someone buys Brushes, then there is a strong association that 
#they will also buy Nail.Polish with 100% confidence. 
#The lift ratio of 3.57 indicates that the likelihood of purchasing Nail Polish increases by 3.57 times when someone purchases Brushes.


# 11) What is the meaning of the support and confidence in the second rule?

#The support of the second rule is 0.119, which means that in 11.9% of all transactions in the dataset, 
#customers have purchased both {Blush, Concealer, Eye.shadow} and {Mascara}. 
#The confidence of 0.9596774 means that out of all the transactions that contain {Blush, Concealer, Eye.shadow}, 
#almost 96% of them also contain {Mascara}. This suggests a strong association between the two sets of items in the rule.









