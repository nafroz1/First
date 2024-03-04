library(caret)
library(FNN)

mower.df <- read.csv("C:/Users/Nahid/Desktop/R files/RidingMowers.csv") # Data loading
View(mower.df)

# Data Partitioning
set.seed(2023) # Setting seed for reproducibility
total.rows = dim(mower.df)[1] # Total number of rows
train.rows <- sample( 1:total.rows, total.rows*0.6 ) # Sampling for training data
train.data <- mower.df[train.rows, ] # Creating training data
valid.data <- mower.df[-train.rows, ] # Creating validation data

# Visualizing Data
plot(train.data$Lot_Size ~ train.data$Income) # Plotting training data
plot(Lot_Size ~ Income, data = train.data) # Alternate plotting method
plot(Lot_Size ~ Income, data=train.data, pch=ifelse(train.data$Ownership=="Owner", 1, 4)) # Plotting with custom markers
text(new.df$Income, new.df$Lot_Size, "N") # Adding text to plot
legend("topright", c("owner", "non-owner", "newhousehold"), pch = c('o', 'x', 'N')) # Adding legend
text(train.data$Income, train.data$Lot_Size, rownames(train.data), pos=4) # Adding row numbers to plot

# Normalizing Data
norm.values <- preProcess(train.data, method=c("center", "scale")) # Normalizing data
summary(norm.values) # Viewing normalization summary
train.norm.df <- predict(norm.values, train.data) # Applying normalization to training data
valid.norm.df <- predict(norm.values, valid.data) # Applying normalization to validation data
new.norm.df   <- predict(norm.values, new.df) # Applying normalization to new data

# Viewing normalized data
View(train.norm.df)

# k-NN Classification
knn1 = knn(train = train.norm.df[ , -3], test = new.norm.df , cl = train.norm.df$Ownership , k=1, prob = TRUE) # k-NN with k=1
knn3 = knn(train = train.norm.df[ , -3], test = new.norm.df , cl = train.norm.df$Ownership , k=3, prob = TRUE) # k-NN with k=3
knn5 = knn(train = train.norm.df[ , -3], test = new.norm.df , cl = train.norm.df$Ownership , k=5, prob = TRUE) # k-NN with k=5

# Confusion Matrix Calculation
confusionMatrix(knn1, as.factor(valid.norm.df$Ownership), positive = "Owner") # Confusion matrix for k=1
a=confusionMatrix(knn3, as.factor(valid.norm.df$Ownership), positive = "Owner") # Confusion matrix for k=3 and storing result
confusionMatrix(knn5, as.factor(valid.norm.df$Ownership), positive = "Owner") # Confusion matrix for k=5

# Looping through different k values
myk = c(1,3,5,7,9)
t=1
mydata = data.frame(k=1:5, Accuracy = 1:5, Sensitivity = 1:5, Specificity = 1:5) # Initializing dataframe for storing results

for (i in myk) {
  myknn <- knn(train.norm.df[, -3], test = valid.norm.df[, -3], cl = train.norm.df$Ownership, k = i)
  a=confusionMatrix(myknn, as.factor(valid.norm.df$Ownership), positive = "Owner")
  mydata[t,1] = i
  mydata[t,2] = a$overall[1]
  mydata[t,3] = a$byClass[1]
  mydata[t,4] = a$byClass[2]
  t = t+1
}
View(mydata)
