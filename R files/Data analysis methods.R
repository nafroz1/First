# Data Mining Final Exam - Logistic Regression and Classification Tree Example

# Reading the dataset
d = read.csv("babyweight.csv")

# Partitioning the data: 70% training, 30% validation
set.seed(2023)
train.index <- sample(1:dim(d)[1], dim(d)[1]*0.7)
train.df <- d[train.index, ]
valid.df <- d[-train.index, ]

# Logistic Regression
logit.reg <- glm(lowbirthweight ~ . , data = train.df, family = "binomial")
summary(logit.reg)
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")
logit.reg.pred.class = ifelse(logit.reg.pred >= 0.5, "1", "0")

# Confusion Matrix for Logistic Regression
library(caret)
confusionMatrix(as.factor(logit.reg.pred.class), as.factor(valid.df$lowbirthweight), positive = "1")

# Classification Tree
library(rpart)
library(rpart.plot)
default.ct <- rpart(lowbirthweight ~ . , data = train.df, method = "class")
prp(default.ct, type = 4, extra = 1, under = TRUE, split.font = 2, varlen = -10)
default.ct.pred.valid <- predict(default.ct, valid.df, type = "class")
confusionMatrix(default.ct.pred.valid, as.factor(valid.df$lowbirthweight), positive = "1")

# KNN
library(FNN)
train.df.norm <- scale(train.df[, -which(names(train.df) == "lowbirthweight")])
valid.df.norm <- scale(valid.df[, -which(names(valid.df) == "lowbirthweight")], center = attr(train.df.norm, "scaled:center"), scale = attr(train.df.norm, "scaled:scale"))
knn.pred <- knn(train.df.norm, valid.df.norm, cl = train.df$lowbirthweight, k = 1)
confusionMatrix(as.factor(knn.pred), as.factor(valid.df$lowbirthweight), positive = "1")

# Naive Bayes
library(e1071)
d.nb <- naiveBayes(lowbirthweight ~ ., data = train.df)
pred.class <- predict(d.nb, newdata = valid.df, type = "class")
confusionMatrix(pred.class, valid.df$lowbirthweight, positive = "1")

# Association Analysis
library(arules)
course.df <- read.csv("Coursetopics.csv")
course.mat <- as.matrix(course.df)
course.trans <- as(course.mat, "transactions")
itemFrequencyPlot(course.trans)
rules <- apriori(course.trans, parameter = list(supp = 10/365, conf = 0.6, target = "rules"))
inspect(sort(rules, by = c("confidence")))
inspect(sort(rules, by = c("lift")))
