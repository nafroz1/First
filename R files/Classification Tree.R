# Classification Tree

# The file accidents.csv contains a subset of information on actual automobile
# accidents in 2001 in the United States that involved one of three levels of injury: NO INJURY, INJURY, or FATALITY. 
For each accident,# additional information is recorded, such as day of week, weather conditions, and road type. 

# A firm might be interested in developing a system for quickly classifying the severity of an accident based on initial reports
# and associated data in the system. 

d = read.csv("datasets/accidents.csv")
View(d)

# Involve an injury (MAX_SEV = "fatal" or "non-fatal"),
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


#Factor creation - two levels: yes or no.

d$INJURY = factor( ifelse(d$MAX_SEV == "no-injury", "NO", "YES" ) )

#  Interested in correctly identifying fatal or injury cases, which is YES category of the INJURY variable. 

# So removing unnecessary MAX_SEV variable
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

# Classification tree model 

default.ct <- rpart(INJURY ~ . , data = train.df, method = "class")


# Plot resulting tree using prp() function.

prp(default.ct, type = 4, extra = 1, under = TRUE, split.font = 2, varlen = -10)

#Model Prediction and Evaluation
# Training data prediction and evaluation
default.ct.pred.train <- predict(default.ct, train.df, type = "class")
confusionMatrix(default.ct.pred.train, as.factor(train.df$INJURY), positive = "YES")

# Validation data prediction and evaluation
default.ct.pred.valid <- predict(default.ct, valid.df, type = "class")
confusionMatrix(default.ct.pred.valid, as.factor(valid.df$INJURY), positive = "YES")


# Accuracy level on the validation partition? Comparing with naive method

table(d$INJURY)/(292+308)
# With naive method, proportion of YES is 51% which is accuracy.
# With decision tree, our accuracy is 55%, which is better.

# Prediction for new traffic accident
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

new.df.pred <- predict(default.ct, new.df, type = "class")
print(new.df.pred)


