# HOMEWORK

# PART-A

# Routine breast cancer screening allows cancer to be diagnosed early.
# If machine learning could automate the identification of cancerous cells,
# it would provide considerable benefit to the health system. 
# In this homework, you will use machine learning for detecting cancer by applying
# the k-NN algorithm to measurements of biopsied cells from women with 
# abnormal breast masses.

# download and read the dataset "wisc_bc_data.csv"
d=read.csv("C:/Users/Nahid/Desktop/wisc_bc_data.csv")
View(d)

# The breast cancer data includes 569 examples of cancer biopsies,
# each with 32 columns. First column is the identification (patient) number, 
# second column is the cancer diagnosis, and the rest of the columns are 
# numeric-valued laboratory measurements (predictors). 

# The diagnosis is coded as:
# "M" to indicate malignant, or
# "B" to indicate benign.

# We are more interested in correctly diagnosing malignant (M)
# cases than benign (B) cases. 

# The 30 numeric measurements comprise the mean, standard error, and 
# worst (that is, largest) value for 10 different characteristics
# of the digitized cell nuclei. 

# Preparing the dataset:

# Since ID column is nothing to do with the diagnosis,
# we drop it from the dataframe. Run the following code.

d = d[, -1]
View(d)

# For convenience, we will use a separate dataframe named "outcome" for the diagnosis variable.
# We create this dataframe and name it outcome$diagnosis. It will be a factor type. 
# Then, we drop diagnosis column from the original dataframe d. Run the following code.

outcome = as.data.frame(d[ , 1])
colnames(outcome) = "diagnosis"
outcome$diagnosis = as.factor(outcome$diagnosis)
View(outcome)

d = d[, -1]   # drop the diagnosis column from the original dataset now
View(d)

# QUESTIONS


# 1) How many benign (B) and malignant (M) cases are there in the outcome dataset?
#    If you use the naive rule approach with majority voting, how would you
#    classify a new patient? B or M? What would be the overall accuracy
#    in this approach?

table(outcome)
#B   M 
#357 212 

accuracy = 357 / (357 + 212)
#accuracy = 0.6274165
#62.74%


# 2) Since the variable values in the predictors dataset (ie. d) include very small
#    and very large values, normalization is needed. Normalize the data now with center and scale. 
#    Name the normalization variable "norm.values". 
#    Name the resulting normalized data set as "d.norm.df".
library(caret)

norm.values <- preProcess(d, method=c("center", "scale"))
d.norm <- predict(norm.values, d)
View(d.norm) 




# 3) Now partition the normalized data into training (70%) and validation (30%).
#    Make sure your code include total.rows, train.rows and valid.rows.

set.seed(2023)
total.rows = dim(d.norm)[1]
train.rows = sample(1:total.rows, total.rows*0.7)
valid.rows = setdiff(1:total.rows, train.rows)  
train.df = d.norm[train.rows, ]
valid.df = d.norm[-train.rows, ]

# 4) Now find the best k value by trying different k values from 1 to 20.
#    Copy and modify the looping code shown in the class. Positive is "M".

mydata <- data.frame(k = seq(1, 20), accuracy = rep(0, 20), sensitivity = rep(0, 20), specificity = rep(0, 20))
myk = 1:20

for(i in myk) {       
  
  myknn <- knn(train.df, test = valid.df, 
               cl = outcome[train.rows,], k = i)                     
  
  a = confusionMatrix(myknn, outcome[valid.rows,], positive = "M")    
  
  
  mydata[i, 1] = i
  mydata[i, 2] = a$overall[1]  # Accuracy
  mydata[i, 3] = a$byClass[1]  # Sensitivity
  mydata[i, 4] = a$byClass[2]  # Specificity
  
}

View(mydata)

#sensitivity is more important in this case because identifying a true positive (malignant tumor) is more 
# important than a true negative (benign tumor)
# Therefore, k=3 has the highest sensitivity

# 5) Create a knn model with the k value you decided in the previous question. Apply it on
#    the validation set, get the confusion matrix and accuracy numbers.

knn.pred <- knn(train = train.df, test = valid.df, 
                cl = outcome[train.rows, ], k = 3)   
class(knn.pred)   
p=data.frame(Actual=outcome[valid.rows, ], Predicted = knn.pred)
View(p)

table(p)

a=confusionMatrix(knn.pred, outcome[valid.rows,], positive = "M")   
a
a$overall[1] 
#Accuracy is 95%

# 6) A dataset of 10 new patients arrived. (Download newpatients dataset from Canvas)
new.patients= read.csv("C:/Users/Nahid/Desktop/newpatients.csv")
View(new.patients)

#    A diagnosis need to be made for these patients.
#    Drop ID column and normalize the data. Note: Use "norm.values" that is already created.
#    Make the diagnosis using the k value you recommended in Q4. 
#    How many "malignant" predictions are made?

new.patients$id <- NULL
new.patients.norm <- predict(norm.values, new.patients)
new.patients.norm

knn.pred.new <- knn(train = train.df, test = new.patients.norm, 
                    cl = outcome[train.rows,], k = 3)

p=data.frame(Actual="Unknown", Predicted = knn.pred.new)
p

# 2 Malignant predictions are made

#-----------------------------------------------------------------------------

# PART-B

# The file accidents.csv contains a subset of information on actual automobile
# accidents in 2001 in the United States that involved one of the three 
# levels of injury: NO INJURY, INJURY, or FATALITY. For each accident,
# additional information is recorded, such as day of week, weather conditions,
# and road type. 

# A firm might be interested in developing a system for quickly 
# classifying the severity of an accident based on initial reports
# and associated data in the system. Read the data now from Canvas.


# Our goal here is to predict whether an accident just reported will
# involve an injury (i.e., MAX_SEV = "fatal" or "non-fatal"),
# or will not (MAX_SEV = "no-injury"). You will use Naive Bayes method
# for this problem.

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

# QUESTIONS:

# 1) The outcome variable, MAX_SEV has three levels ("fatal", "non-fatal", "no-injury" ).
# However, we are interested in only two events, either there is injury, or no injury.
# For this purpose, insert a new factor variable in d called INJURY 
# that takes the value “NO” if MAX_SEV == "no-injury", and otherwise “YES”.
# Hint: Use ifelse(.....). 
# When done, drop the MAX_SEV column.

d$INJURY <- as.factor(ifelse(d$MAX_SEV == "no-injury", "NO", "YES"))


# 2) Speed limit variable is a numerical variable ranging from 10 to 75. You need to
#    bin (i.e. group) this variable: First, divide it by 10, and then round.

d$binned_SPD_LIM = d$SPD_LIM/10
d$binned_SPD_LIM <-round(d$binned_SPD_LIM)



# 3) Since Naive Bayes technique requires all variables to be categorical,
#    convert all variables to factor using lapply(d, factor). Convert
#    the resulting list into dataframe using as.data.frame().

d=as.data.frame(lapply(d,factor))
str(d)


# 4) # Now partition the dataset into training (60%) and validation (40%).
# Make sure you have total.rows, train.rows, valid.rows.

set.seed(2023)
total.rows = dim(d.norm)[1]
train.rows = sample(1:total.rows, total.rows*0.6)
valid.rows = setdiff(1:total.rows, train.rows)  

# 5) Develop a Naive Bayes model using INJURY as outcome, and all other variables
#    as predictors on the training set. Hint: INJURY ~ . 
#    Call the model "traffic.nb"

library(e1071)
selected.var=c(1:13)
set.seed(2023)
train.index=sample(c(1:dim(d)[1]), dim(d)[1]*0.6)  
train.df=d[train.index, selected.var]
valid.df=d[-train.index, selected.var]

d.nb = naiveBayes(INJURY ~ . , data = train.df)

d.nb



# 6) Apply the model on validation data by doing the following steps:

# a) Predict probabilities: type is "raw" (i.e. probability values)

table(train.df$INJURY)/dim(train.df)[1]
pred.prob = predict(d.nb, newdata = valid.df, type = "raw") 

# b) Predict class membership: type is "class" (i.e. class names)

pred.class = predict(d.nb, newdata = valid.df, type = "class")


# c) Put everything in a dataframe called "df" for presentation purposes
# [actual, predicted, probabilities]

df = data.frame( actual = valid.df$INJURY, predicted=pred.class, probability = pred.prob )

View(df)



# 7) Check the level of accuracy on the training, and then validation partitions
#    using predict() and confusionMatrix() functions. Hint: positive = "YES"

library(caret)

pred.class <- predict( d.nb, newdata = train.df,type = "class" )
confusionMatrix( pred.class, train.df$INJURY, positive = "YES" )

pred.class <- predict( d.nb, newdata = valid.df, type = "class" )
confusionMatrix( pred.class, valid.df$INJURY, positive = "YES" )


# 9) Create a lift chart using the results in "df" dataframe. Hint: Use
#    df$actual, df$YES, ref="YES"

library(caret)

pred.prob = predict(d.nb, newdata = valid.df, type = "raw")
pred.prob = data.frame(pred.prob)

mylift <- lift(relevel(valid.df$INJURY, ref="YES") ~ pred.prob$YES)
xyplot(mylift, plot = "gain")



# 10) Create an ROC chart using the results in "df" dataframe. Hint: Use
#    df$actual, df$YES, ref="YES", levels = c("YES","NO")


library(pROC)


myroc <- roc( df$actual , df$probability.YES, levels = c("YES","NO"))

plot(myroc, main = "ROC curve for the model",
     col = "blue", lwd = 2, legacy.axes = TRUE)



