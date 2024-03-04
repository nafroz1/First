# Homework - Data Mining Tasks

# PART-1: Logistic Regression for Accident Severity Prediction

# Read dataset
d = read.csv("Downloads/accidents.csv")

# Create binary outcome variable for injury
d$INJURY = factor(ifelse(d$MAX_SEV == "no-injury", "NO", "YES"))
d$MAX_SEV <- NULL # Remove unnecessary variable

# Partition data into training and validation sets
set.seed(2023)
train.index <- sample(1:dim(d)[1], dim(d)[1]*0.6)
train.df <- d[train.index, ]
valid.df <- d[-train.index, ]

# Fit logistic regression model
logit.reg <- glm(INJURY ~ ., data = train.df, family = "binomial")

# Model summary
summary(logit.reg)

# Predict probabilities on validation set
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

# Classify predictions
logit.reg.pred.class <- factor(ifelse(logit.reg.pred >= 0.5, "YES", "NO"))

# Evaluate model performance
library(caret)
confusionMatrix(logit.reg.pred.class, as.factor(valid.df$INJURY), positive = "YES")

# Predict injury status for new data
new.df = data.frame(
  RushHour = 1, WRK_ZONE = 1, WKDY = 1, INT_HWY = 1,
  LGTCON_day = 0, LEVEL = 1, SPD_LIM = 6, SUR_COND_dry = 0,
  TRAF_two_way = 0, WEATHER_adverse = 0
)
new.df.pred = predict(logit.reg, new.df, type = "response")
ifelse(new.df.pred >= 0.5, "YES", "NO")

# PART-2: Market Basket Analysis for Cosmetics Purchase Patterns

# Read dataset
cos.df <- read.csv("Downloads/Cosmetics.csv")

# Preprocess data
cos.df <- cos.df[,-1] # Remove first column
cos.df.mat <- as.matrix(cos.df) # Convert to matrix
cos.trans <- as(cos.df.mat, "transactions") # Convert to transactions format

# Inspect transactions
inspect(cos.trans)

# Plot item frequency
itemFrequencyPlot(cos.trans)

# Run apriori algorithm
rules <- apriori(cos.trans, parameter = list(supp= 100/1000, conf = 0.6, target = "rules"))

# Number of rules generated
print(rules)

# Inspect rules sorted by confidence
inspect(sort(rules, by = "confidence"))

# First 3 rules and their interpretations
# [1] {Brushes} => {Nail.Polish} 0.149 1.0000000 0.149 3.571429
# [2] {Blush, Concealer, Eye.shadow} => {Mascara} 0.119 0.9596774 0.124 2.688172
# [3] {Blush, Eye.shadow} => {Mascara} 0.169 0.9285714 0.182 2.601040

# Interpretation of Rule 1:
# - 100% confidence that purchasing Brushes leads to purchasing Nail Polish.
# - Lift of 3.57 indicates that Nail Polish is 3.57 times more likely to be purchased when Brushes are bought.

# Interpretation of Rule 2:
# - Support of 0.119 means 11.9% of transactions include both sets of items.
# - Confidence of 95.97% means nearly all transactions with {Blush, Concealer, Eye.shadow} also include Mascara.

# Interpretation of Rule 3:
# - Support of 0.169 indicates that 16.9% of all transactions contain both {Blush, Eye.shadow} and {Mascara}.
# - Confidence of 92.85714% means that when Blush and Eye shadow are purchased together, there's a 92.86% chance Mascara will also be bought.
# - Lift of 2.601040 suggests that Mascara is 2.6 times more likely to be purchased when Blush and Eye shadow are bought together compared to its overall purchase probability.











