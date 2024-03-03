#-----------------------------------
# Introduction to Data Mining
# Midterm Exam
#-----------------------------------
# Name:
#-----------------------------------
# EXAM RULES: PLEASE READ.

# ANSWER ALL 15 QUESTIONS.

# DO NOT SUBMIT WORD DOCUMENTS. IT HAS TO BE R SO THAT I CAN RUN YOUR CODE.
# ANY ATTEMPT FOR SHARING YOUR WORK OR TEAM WORK WILL BE PENALIZED.
# SUBMIT YOUR WORK BY THE TIME IT IS DUE. ANY LATE SUBMISSION WILL BE 
# ASSESSED A LATE PENALTY.

#-----------------------------------------------
# You should use the packages shown in the class. 
# If I don't recognize a function from a package
# unknown to me, then your answer will 
# be penalized.
#-----------------------------------------------

# Run the following libraries

library(ggplot2)
library(forecast)
library(reshape)

# Use "patients.csv" dataset for the following questions. 

# This dataset is about diabetic patients and their
# re-admission data. Patient re-admission rates is an important
# metric that is monitored by hospital administrators.

d=read.csv("C:/Users/Nahid/Desktop/patients.csv")
dim(d)
str(d)
View(d)

#-----------------------------------------------
# Questions
#-----------------------------------------------
# PART-1: Data Exploration and Cleaning
#-----------------------------------------------
# 1) The data has a lot of missing values in many columns. However,
#    they are not represented by NA. Instead, "?" question mark is used.
#    Now convert all "?" marks in columns: race, weight, payer_code, medical_specialty
#    diag_2, and diag_3 to NA.


d[ d$race == "?", "race"] = NA

d[ d$weight == "?", "weight"] = NA

d[ d$payer_code == "?", "payer_code"] = NA

d[ d$medical_specialty == "?", "medical_specialty"] = NA

d[ d$diag_2 == "?", "diag_2"] = NA

d[ d$diag_3 == "?", "diag_3"] = NA



#-----------------------------------------------

# 2) Calculate total number of NAs in the dataset. And, then,
#    Create a heatmap to see the extent of the NA situation.
#    Comment on the heatmap.


sum(is.na(d)) 
#5717

missing.mat = 1 * is.na(d)

class(missing.mat)
melted.missing.mat <- melt(missing.mat)
View(melted.missing.mat)    


ggplot(melted.missing.mat, aes(x = X1, y = X2, fill = value) ) + 
  geom_tile() + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Missing Val") 


#-----------------------------------------------

# 3) Based on your visual inspection of the heatmap, which column looks like
#    having the highest number of missing values? Now drop this column if it has
#    more than 90% missing values.
#    HINT: First, calculate percent missing in this column.


sum(is.na(d$weight))/length(d$weight)
#97% 

d$weight <- NULL


#-----------------------------------------------

# 4) Imputate missing values in the race column with the most frequent category.
#    HINT: table() first.


table(d$race) #Most frequent category is mode which here is "Caucasian"
mode_race = "Caucasian"
d$race[is.na(d$race)] =mode_race


#-----------------------------------------------

# 5) 
#   a) Create a heatmap including the following columns:
#   time_in_hospital, num_lab_procedures , num_procedures , num_medications , 
#   number_outpatient , number_emergency , number_inpatient, number_diagnoses.
#   HINT: Use myColumns variable (below) to select these columns conveniently.

myColumns = c("time_in_hospital", "num_lab_procedures" , "num_procedures" , "num_medications" , 
              "number_outpatient" , "number_emergency" , "number_inpatient", "number_diagnoses")





myColumns = c("time_in_hospital", "num_lab_procedures" , "num_procedures" , "num_medications" , 
              "number_outpatient" , "number_emergency" , "number_inpatient", "number_diagnoses")
cor.mat = cor(d[myColumns])

cor.mat <- round(cor.mat, 2)
melted.cor.mat <- melt(cor.mat)

ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value) ) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "yellow", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(x = X1, y = X2, label = value),color="black")



#   b) Write the names of three variables that have the highest correlations with
#   time_in_hospital

#num_diagnosis = 0.23, num_lab_procedures = 0.3 and num_medications = 0.49

#-----------------------------------------------

# 6) Assume that our target variable is time_in_hospital (days stayed
#    in the hospital). Create the following boxplots in a 2 window panel (1 x 2):
#    a) time_in_hospital & race
#    b) time_in_hospital & age

dev.off()
par(mfcol = c(1, 2))

boxplot(d$time_in_hospital ~ d$race)
boxplot(d$time_in_hospital ~ d$age)


#-----------------------------------------------

# 7) As you see in the boxplot (time_in_hospital and age), there are
#    similar categories in the age variable. Now, combine 
#    "[30-40)","[40-50)", "[50-60)" categories into one category called
#    "[30-60)". Name the resulting variable newAge.


b = d$age %in% c("[30-40)","[40-50)", "[50-60)")
d$newAge = ifelse(b,"[30-60)",d$age)

table(d$newAge)



#-----------------------------------------------
# 8) 
#    a) Create a scatter plot using ggplot2 based on the following
#       specifications:
# y = num_medications
# x = num_lab_procedures
# size=time_in_hospital
# color = readmitted
# alpha = 0.50

ggplot(data = d) +
  geom_point( mapping = aes(x = num_lab_procedures, y = num_medications, size=time_in_hospital, color=readmitted ), alpha = 0.50 )


#    b) "<30" category in readmitted variable is something not desirable
#       by the hospital. Do you see any pattern in the chart in terms of this category?


#There are more patients in the >30 and NO category compared to <30.With an increase in num_lab_procedures (number of lab procedures), num_medications(number of medications) is low.


#-----------------------------------------------
# PART-2
#-----------------------------------------------

# 9) Partition the dataset into training and validation using 60%-40% split.
#    Seed value is 23.


set.seed(23)

total.rows = dim(d)[1]  

train.rows <- sample( c(1:total.rows), total.rows*0.6 )  

train.data <- d [train.rows, ]                            
valid.data <- d[-train.rows, ]                           

View(train.data)     
View(valid.data)

#-----------------------------------------------

# 10) Create a linear regression model (reg1) on the training dataset
#     predicting time_in_hospital using the following predictors:
#     num_lab_procedures, num_medications, number_diagnoses,
#     and max_glu_serum 


reg1 = lm(time_in_hospital ~ num_lab_procedures + num_medications + number_diagnoses + max_glu_serum, data = train.data)

summary(reg1)
#-----------------------------------------------

# 11) Which predictors are statistically insignificant at 5% level?


#max_glu_serumNone and max_glu_serumNorm are insignificant at 5%.

#-----------------------------------------------

# 12) Comment on adj-R-squared value. Comment on model's significance as a whole.

#30% of variation in time-in-hospital is explained by the model. Overall the model is significant as the p-value is less than 0.05.

#-----------------------------------------------
# 13) Apply the model on the training set. Calculate accuracy measures.

pred1 = predict(reg1, newdata = train.data)   

accuracy(pred1, train.data$time_in_hospital )

#                               ME     RMSE      MAE       MPE     MAPE
#Test set1 -0.000000000000008491987 2.476164 1.858949 -42.51244 67.22577

#-----------------------------------------------

# 14) Apply the model on the validation set. Calculate accuracy measures.

pred2 = predict(reg1, newdata = valid.data)   

accuracy(pred2, valid.data$time_in_hospital )

#                 ME     RMSE      MAE       MPE     MAPE
#Test set2  0.01339925 2.448957 1.882038 -40.65776 65.78725

#-----------------------------------------------

# 15) Comment on differences between accuracy measures in Question 13 and 14.

# The ME for the training data is very close to 0  which suggests that the model is making predictions that are very close to the actual values in the training data.
#The ME for the validation data is positive and non-zero, which suggests that the model is making some errors in its predictions on the validation data. 
#However, a ME of 0.01339925 is relatively small and may be considered acceptable.

#RMSE in both training data and validation data are almost similar.



#------------------END OF THE EXAM-----------------------------

