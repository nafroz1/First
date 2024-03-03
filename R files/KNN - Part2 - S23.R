# Example

# Universal Bank is a relatively young bank growing rapidly in terms
# of overall customer acquisition. The majority of these customers are 
# liability customers (i.e. depositors) with varying sizes of relationship 
# with the bank. The customer base of asset customers (i.e. borrowers) is quite small,
# and the bank is interested in expanding this base rapidly to bring in more
# loan business. In particular, it wants to explore ways of converting its
# liability customers to personal loan customers (while retaining them as 
# depositors).

# The goal is to use k-NN to predict whether a new customer will accept a loan
# offer. This will serve as the basis for the design of a new campaign.
# The file UniversalBank.csv contains data on 5000 customers.  The data include
# customer demographic information (age, income, etc.), the customer’s relationship
# with the bank (mortgage, securities account, etc.), and the customer response
# to the last personal loan campaign (Personal Loan). Among these 5000 customers,
# only 480 (= 9.6%) accepted the personal loan that was offered to them in the
# earlier campaign.

# Data mining task: Create a classification model using k-nn approach to predict
# if a new bank customer would accept the personal loan offer, or not.
# Personal Loan = 1 if customer accepted the loan; it is zero, otherwise.

# Variable definitions in the dataset
# ID:	Customer ID							
# Age:	Customer's age in completed years							
# Experience:	years of professional experience							
# Income:	Annual income of the customer ($000)							
# ZIPCode:	Home Address ZIP code.							
# Family:	Family size of the customer							
# CCAvg:	Avg. spending on credit cards per month ($000)							
# Education:	Education Level. 1: Undergrad; 2: Graduate; 3: Advanced/Professional							
# Mortgage:	Value of house mortgage if any. ($000)							
# Personal Loan:	Did this customer accept the personal loan offered in the last campaign?							
# Securities Account:	Does the customer have a securities account with the bank?							
# CD Account:	Does the customer have a certificate of deposit (CD) account with the bank?							
# Online:	Does the customer use internet banking facilities?							
# CreditCard:	Does the customer use a credit card issued by UniversalBank?							

# 1) Read the data and view the data. Think which variables to use
# and which variables to discard.

d=read_csv("C:/Users/Nahid/Desktop/R files/UniversalBank.csv")
View(d)

# Regular checks
dim(d)
str(d)
sum(is.na(d))

# Naive prediction: percent of the most frequent category
table(d$Personal.Loan)/5000   # 90.4% is zero meaning 90.4% of customers
# are not a loan accepter. 
# According to the naive prediction technique, all future customers are scored
# (i.e. classified) as "non-accepters" due to majority vote.

# percent of personal loan accepters = 9.6%

# According to the naive rule approach, all future customers are predicted
# to be no-loan-accepters, which will be correct about 90 percent of the time. 

# ID and Zip code is nothing to do with loan acceptance
# so we use the remaining 12 variables.
# Remove them from the dataset

d <- d[ , -c(1,5)]
View(d)


# Also let's create a separate vector for the outcome,
# the Personal Loan variable, so that we can process the predictors
# more easily. The remaining columns will be normalized and distances
# will be calculated using them.

# Personal Loan is the 8th column

outcome = as.data.frame( d[, 8] )    # still a dataframe

colnames(outcome) = c("Personal.Loan")  # rename the column
View(outcome)
class(outcome$Personal.Loan)    # it is an integer type but it should be categorical
outcome$Personal.Loan <- factor(outcome$Personal.Loan)    # convert to factor
class(outcome$Personal.Loan)


# Now we can safely drop personal loan column from the dataset
d <- d[ , -8]
View(d)

# 2) Education column looks like numerical but when we read the explanation:
# Education:	Education Level. 1: Undergrad; 2: Graduate; 3: Advanced/Professional							
# We see that it is actually categorical. We wonder if this is nominal or ordinal
# categorical? Levels 1, 2, 3 do not indicate a higher order, because advanced/professional
# category could be lower than undergrad, which is not clear.
# So we conclude that education is nominal categorical and therefore we cannot
# use 1,2,3 designation. We have to convert these levels into dummies.

# Use the library dummies:
# install.packages("dummy")

library(dummy)
d$Education = as.factor(d$Education)  # need to convert to factor first
dumEducation = dummy(d, int= TRUE)    # function dummy() automatically picks up all categorical columns
                                      # and create a separate dataframe for the dummies

View(dumEducation)    # Unlike regression, we will keep all dummies because it won't
                      # break the procedure.

d = data.frame(d, dumEducation)       # combine original data frame with dummies data frame

d$Education <- NULL   # no need for the original education column

# 3) Now let's normalize the predictor data

library(caret)

# Get the mean and sd using preProcess() function for the entire dataset
norm.values <- preProcess(d, method=c("center", "scale"))

# And then, create normalized data. This is done as if we are predicting.
d.norm <- predict(norm.values, d)  # normalize the entire dataset
View(d.norm)   # 99% of all values are now between -3 and +3

# 4) Now let's partition the data into training (60%) and validation (40%)

set.seed(2023)
total.rows = dim(d.norm)[1]
train.rows = sample( 1:total.rows, total.rows*0.6 )
valid.rows = setdiff(1:total.rows, train.rows)   # setdiff() function to subtract one set from the other
train.df = d.norm[train.rows, ]
valid.df = d.norm[-train.rows, ]

# 5) Create a knn model with k=1 using training set, apply it on
# the validation set and get the confusion matrix and accuracy numbers.

library(FNN)
# knn( train = training dataset predictor columns, test = new data columns, 
#   cl=true classification column, i.e. the outcome, k=number of neighbors)

# Creating and applying the model on the validation

knn.pred <- knn(train = train.df, test = valid.df, 
                cl = outcome[train.rows,], k = 1)     # outcome dataframe was created earlier
                                                      # outcome has all the data. We pick up only training ones

class(knn.pred)     # predictions are factor type

# Let's see the actual and predicted values side by side in a dataframe.

p=data.frame(Actual=outcome[valid.rows,], Predicted = knn.pred)
View(p)

table(p)

# Accuracy:
(130+1788)/(130+1788+18+64)


# Create the confusion matrix
a=confusionMatrix(knn.pred, outcome[valid.rows,], positive = "1")   # 1 means loan offer accepted
a


# Calculate Sensitivity manually from the matrix:

# Calculate Specificity manually from the matrix:


a$table       # confusion matrix
a$overall[1]  # accuracy
a$byClass[1]  # sensitivity
a$byClass[2]  # specificity 

# 6) Let's find the best k

# Initialize a data frame with three columns: k; accuracy and sensitivity, which are initially zero.
# Let's try k up to 20

mydata <- data.frame(k = seq(1, 20), accuracy = rep(0, 20), sensitivity = rep(0, 20), specificity = rep(0, 20))

myk = 1:20    # k values to be tried.

for(i in myk) {       # The looping function. i is the item to be picked up from myk list
  
  myknn <- knn(train.df, test = valid.df, 
               cl = outcome[train.rows,], k = i)                     # make classifications
  
  a = confusionMatrix(myknn, outcome[valid.rows,], positive = "1")    # see how good are the classifications
  
  # save the data at row t
  mydata[i, 1] = i
  mydata[i, 2] = a$overall[1]  # Accuracy
  mydata[i, 3] = a$byClass[1]  # Sensitivity
  mydata[i, 4] = a$byClass[2]  # Specificity
  
}

View(mydata)

# We can use k=1 assuming sensitivity is more important than specificity in this business situation.


# Sensitivity: The probability that the model predicts a positive outcome for an observation 
# when indeed the outcome is positive. This is also called the “true positive rate.”
# Specificity: The probability that the model predicts a negative outcome for an observation
# when indeed the outcome is negative. This is also called the “true negative rate.”

# In medical sciences:
# Sensitivity: detecting the presence of disease
# Sensitivity indicates a test's ability to detect disease. 
# With a high sensitivity, many people who are actually sick will get a positive test result.
# This is important, for example in the case of HIV or coronavirus.
# The more sensitive a test is, the fewer false negative results; this helps to prevent infections.

# Specificity: reducing the chance of false positives
# Specificity refers to the percentage of people who don’t actually have the disease and 
# are tested as negative. A diagnostic test with a high specificity therefore has few false positives;
# those who test positive are indeed mostly positive. A test with a high specificity is therefore
# particularly suitable for confirming disease in the event of a positive result. This is important
# in the case of rare disorders, for example. But it’s still possible for people to get a negative
# test result when they’re actually positive. The disadvantage is that high sensitivity doesn’t help
# to exclude false positives.

# If you use a test with a low specificity for, say, detecting cervical cancer, many women will
# get a false positive result. Not desirable, because it leads to concern and upset.
# It’s therefore always important to weigh up and keep a check on sensitivity and specificity
# when choosing a test.
# (reference: https://www.future-diagnostics.com/blog/sensitivity-and-specificity)


# Consider the following customer and predict his/her Personal Loan prospects.

# Age = 40, Experience = 10, Income = 84, Family = 2, 
# CCAvg = 2, Education_1= 0, Education_2 = 1, Education_3 = 0,
# Mortgage = 0, Securities Account = 0, CD Account = 0, 
# Online = 1, and Credit Card = 1.

new.customer = data.frame(
  Age = 40, Experience = 10, Income = 84, Family = 2, 
  CCAvg = 2, Education_1= 0, Education_2 = 1,Education_3 = 0,
  Mortgage = 0, Securities.Account = 0, CD.Account = 0, 
  Online = 1, CreditCard = 1
)

# Now create normalized new customer
new.customer.norm <- predict(norm.values, new.customer)
new.customer.norm

# Make the actual prediction using KNN:
knn.pred.new <- knn(train = train.df, test = new.customer.norm, 
                    cl = outcome[train.rows,], k = 1)

p=data.frame(Actual="Unknown", Predicted = knn.pred.new)
p

knn.pred.new <- knn(train = train.df, test = new.customer.norm, 
                    cl = outcome[train.rows,], k = 3)


p=data.frame(Actual="Unknown", Predicted = knn.pred.new)
p

# 0 is predicted: This person will likely reject personal loan offer



# Visualizing the performance of the classification

# Receiver Operating Characteristic (ROC) curve
# An ROC curve (receiver operating characteristic curve) is a graph 
# showing the performance of a classification model at all classification thresholds.

# Starting from the lower left, the ROC curve plots
# the pairs {sensitivity, specificity}.

# In a perfect accuracy, the shape is a rectangle and
# the area under the curve is 100%. Less than that
# indicates lower performance.
# If the model does not predict well, it wanders more or less diagonally
# from the bottom left to the top right. 

# Also, the area under the curve (AUC) is a numerical indicator of the
# goodness of fit, with 1 being the perfect fit.

# The ROC curve can be plotted in two similar ways:

# Y-Axis: True Positive Rate (Sensitivity)
# X-Axis: True Negative Rate (Specificity), or False Positive Rate (1 - Specificity)

#             Reference
# Prediction    0    1
#       0     1800   84
#       1       6   110

# True Positive Rate:
110/(110+84)     # 0.5670103
# False Positive Rate
6/(1800+6)
1-1800/(1800+6)    # alternatively, 1-specificity

# Let's create ROC curves for the following two situations:
knn1.pred <- knn(train.df, test = valid.df, 
                 cl = outcome[train.rows,], k = 1)   

knn3.pred <- knn(train.df, test = valid.df, 
                 cl = outcome[train.rows,], k = 3)  


library(pROC)
# roc(actual outcome, predicted outcome)

# k= 1
predictions = as.numeric(knn1.pred)

actual = outcome[valid.rows,]

r <- roc(actual, predictions )
plot.roc(r)

auc(r)   # Area under the ROC curve: 0.8301

# k= 3
predictions = as.numeric(knn3.pred)

actual = outcome[valid.rows,]

r <- roc(actual, predictions )
plot.roc(r)

auc(r)   # Area under the ROC curve: 0.7942





#---------- Propensities ----------------
# The first step in most classification algorithms is to estimate the probability that
# a record belongs to each of the classes. These probabilities are also called propensities.
# Propensities are typically used either as an interim step for generating
# predicted class membership (classification), or for rank-ordering the records by
# their probability of belonging to a class of interest

# For example, a customer is mapped to a neighborhood with k=5. In this neighborhood,
# 3 out 5 are owners. Therefore, this customer's propensity to be an owner can be estimated as 3/5.
# Since this value is greater than 0.5 (default cutoff in majority rule), we predict
# that this customer is an owner.
# However, this standard cutoff may not be desirable all the time. Sometimes,
# you may want to use a higher cutoff, sometimes a lower cutoff. 
# With higher cutoff, you are more cautious and imposing a higher standard for
# class membership. In other words, it might be more important to
# classify owners properly than nonowners. A misclassification here could be costly.
# For example, the company will send some gifts/promotional materials to really serious buyers only.
# On the other hand, lowering the cutoff, means many nonowners will pass incorrectly as owners.
# For example, you are trying to sell your product on the phone, and each call is not so expensive,
# If the call did not yield a sale, it is okay.

owner.df <- read_csv("C:/Users/Nahid/Desktop/R files/ownerExample.csv")
View(owner.df)
owner.df

# Class column shows the actual class
# Probability shows probability of belonging a class

# let's try a few different cutoff values

confusionMatrix(as.factor(ifelse(owner.df$Probability>0.5, 'owner', 'nonowner')), 
                as.factor(owner.df$Class), positive = 'owner')
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.25, 'owner', 'nonowner')), 
                as.factor(owner.df$Class), positive = 'owner')
confusionMatrix(as.factor(ifelse(owner.df$Probability>0.75, 'owner', 'nonowner')), 
                as.factor(owner.df$Class), positive = 'owner')



# Lift Charts (Gains Charts)

# The lift curve helps us determine how effectively we can
# select a relatively small number of records and get a
# relatively large portion of the responders (true positives).
# See the Excel spreadsheet for details.


# first option with 'caret' library:
library(caret)

owner.df$Actual = ifelse(owner.df$Class == "owner", 1, 0)

lift.example <- lift(relevel(as.factor(Actual), ref="1") ~ Probability, data = owner.df)
xyplot(lift.example, plot = "gain")

#smaller shaded area is incorrect classification. Pic on phone 

# Second option with 'gains' library:
library(gains)

gain <- gains(owner.df$Actual, owner.df$Probability, groups=dim(owner.df)[1])
plot(c(0, gain$cume.pct.of.total*sum(owner.df$Actual)) ~ c(0, gain$cume.obs),
     xlab = "# cases", ylab = "Cumulative", type="l")
lines(c(0,sum(owner.df$Actual))~c(0,dim(owner.df)[1]), col="gray", lty=2)


# Interpreting the lift chart What is considered good or bad performance?

# The ideal ranking performance would place all the 1’s at the beginning
# (the actual 1’s would have the highest propensities and be at the top of the table)
# and all the 0’s at the end. A lift chart corresponding to this ideal case would
# be a diagonal line with slope 1 which turns into a horizontal line (once all
# the 1’s were accumulated). In the example, the lift curve for the best possible
# classifier—a classifier that makes no errors—would overlap the existing curve at
# the start, continue with a slope of 1 until it reached all the 12 1’s, then continue
# horizontally to the right.
# In contrast, a useless model would be one that randomly assigns propensities
# (shuffling the 1’s and 0’s randomly in the Actual Class column).
# This is the diagonal line joining the points (0,0) to (24,12)

