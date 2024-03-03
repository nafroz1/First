# H O M E W O R K

# The file accidents.csv contains a subset of information on actual automobile
# accidents in 2001 in the United States that involved one of three 
# levels of injury: NO INJURY, INJURY, or FATALITY. For each accident,
# additional information is recorded, such as day of week, weather conditions,
# and road type. 

# A firm might be interested in developing a system for quickly 
# classifying the severity of an accident based on initial reports
# and associated data in the system. Read the data now from Canvas.


d = read.csv("datasets/accidents.csv")
View(d)

# Our goal here is to predict whether an accident just reported will
# involve an injury (MAX_SEV = "fatal" or "non-fatal"),
# or will not (MAX_SEV = "no-injury" ).

# YOU DID THIS HOMEWORK WITH BAYESIAN APPROACH.
# NOW USE THE CLASSIFICATION TREE APPROACH FOR THE SAME PROBLEM

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

# now removing unnecessary MAX_SEV variable
d$MAX_SEV <- NULL

View(d)

# Partition the data

set.seed(14)
total.rows = dim(d)[1]
train.rows = sample(1:total.rows, total.rows*0.6)  
valid.rows = setdiff(1:total.rows, train.rows)
train.df = d[train.rows, ]
valid.df = d[-train.rows, ]

library(rpart)
library(rpart.plot)

# QUESTIONS:

# 1) Run a classification tree model using INJURY as outcome, and all other variables
#     as predictors. Use default settings in rpart() function.
#     Name this model default.ct



# 2) Plot the resulting tree using prp() function.




# 3) Apply the model on training data using predict() function.
#    Name the predictions default.ct.pred.train




# 4) Generate the confusion matrix for the training data. What is
#    the accuracy? Specificity? 



# 5) Apply the model on validation data.
#    Name the predictions default.ct.pred.valid




# 6) Generate the confusion matrix for the validation data. What is
#    the accuracy? Specificity? 



# 7) What do you think about the accuracy level on the validation partition? Is it
#    better than naive method?



# 8) Predict the injury status of a new traffic accident using
#     the following data. Create  new data frame and name it "new.df" dataframe.
#     What is your prediction? Injury yes or no?

#  RushHour = 1,
#  WRK_ZONE = 1,
#  WKDY = 1,
#  INT_HWY = 1,
#  LGTCON_day = 0,
#  LEVEL = 1,
#  SPD_LIM = 6,
#  SUR_COND_dry = 0,
#  TRAF_two_way = 0,
#  WEATHER_adverse = 0
  

