# PART-A: Machine Learning for Breast Cancer Detection

# Download and read the dataset "wisc_bc_data.csv"
d = read.csv("R files/wisc_bc_data.csv")
View(d)

# Preparing the dataset
d = d[, -1] # Drop ID column
View(d)

# Create outcome dataframe for diagnosis
outcome = as.data.frame(d[,1])
colnames(outcome) = "diagnosis"
outcome$diagnosis = as.factor(outcome$diagnosis)
View(outcome)

d = d[, -1] # Drop diagnosis column
View(d)

# Question 1: Count benign (B) and malignant (M) cases
table(outcome$diagnosis)

# Question 2: Normalize predictor values
library(caret)
norm.values <- preProcess(d, method=c("center", "scale"))
d.norm.df <- predict(norm.values, d)
View(d.norm.df)

# Question 3: Partition normalized data into training and validation
set.seed(2023)
total.rows = dim(d.norm.df)[1]
train.rows <- sample(1:total.rows, total.rows*0.7)
valid.rows <- setdiff(1:total.rows, train.rows)
train.df <- d.norm.df[train.rows, ]
valid.df <- d.norm.df[valid.rows, ]
View(train.df)

# Question 4: Find the best k value for k-NN
library(FNN)
mydata <- data.frame(k = 1:20, accuracy = rep(0, 20), sensitivity = rep(0, 20), specificity = rep(0, 20))
for(i in 1:20) {
  knn.pred <- knn(train.df, valid.df, cl = outcome[train.rows,], k = i)
  cm <- confusionMatrix(knn.pred, outcome[valid.rows,], positive = "M")
  mydata[i,] <- c(i, cm$overall[1], cm$byClass[1], cm$byClass[2])
}
View(mydata)

# Question 5: Apply k-NN model on validation set
knn3 <- knn(train.df, valid.df, cl = outcome[train.rows, "diagnosis"], k = 3)
confusionMatrix(knn3, outcome[valid.rows, "diagnosis"], positive = "M")

# Question 6: Diagnose new patients
new.patients <- read.csv("datasets/newpatients.csv")
View(new.patients)
new.patients.norm.df <- predict(norm.values, new.patients[, -1])
new.patients.pred <- knn(train.df, new.patients.norm.df, cl = outcome[train.rows, "diagnosis"], k = 3)
new.patients.pred

# PART-B: Accident Severity Classification with Naive Bayes

# Read accidents data
d = read.csv("datasets/accidents.csv")
View(d)

# Prepare outcome variable INJURY
d$INJURY = ifelse(d$MAX_SEV == "no-injury", "NO", "YES")
d$MAX_SEV <- NULL

# Bin Speed limit variable
d$SPD_LIM <- round(d$SPD_LIM/10)

# Convert variables to factor for Naive Bayes
d <- as.data.frame(lapply(d, factor))
str(d)
View(d)

# Partition data into training and validation
set.seed(14)
train.rows <- sample(1:dim(d)[1], dim(d)[1]*0.6)
valid.rows <- setdiff(1:dim(d)[1], train.rows)
train.df <- d[train.rows, ]
valid.df <- d[valid.rows, ]

# Develop Naive Bayes model
library(e1071)
traffic.nb <- naiveBayes(INJURY ~ ., data = train.df)
View(traffic.nb)

# Apply model on validation data
pred.prob <- predict(traffic.nb, valid.df, type = "raw")
pred.class <- predict(traffic.nb, valid.df, type = "class")
df <- data.frame(actual = valid.df$INJURY, predicted = pred.class, pred.prob)
View(df)

# Check accuracy on training and validation

confusionMatrix(pred.class.train, train.df$INJURY, positive = "YES")
confusionMatrix(pred.class.valid, valid.df$INJURY, positive = "YES")

# Create lift and ROC charts
library(caret)
mylift <- lift(relevel(df$actual, ref="YES") ~ df$YES)
xyplot(mylift, plot = "gain")

library(pROC)
myroc <- roc(df$actual, df$YES, levels = c("YES","NO"))
plot(myroc, main = "ROC curve for the model", col = "blue", lwd = 2, legacy.axes = TRUE)
auc(myroc)




