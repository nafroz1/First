# Homework5
# The following dataset is about real estate properties.
# The analytics job is to create a regression model to predict "SalePrice".
# Metadata is attached to this homework as a separate text file.

d = read.csv("C:/Users/Nahid/Desktop/properties.csv")
str(d)

# Questions

# 1) Analyze the dataset for missing values using a heatmap approach.
#    Which column has missing values?

missing.mat = 1* is.na(d)
class(missing.mat)
melted.missing.mat <- melt(missing.mat)
View(melted.missing.mat)    


ggplot(melted.missing.mat, aes(x = X1, y = X2, fill = value) ) + geom_tile() + geom_tile(color = "white") +  scale_fill_gradient2(low = "blue", high = "blue", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Missing Val")


# 2) Assume that the missing values in BsmtCond means "basement does not exist".
#    Now, replace NA values in this column with categorical value of "DNE" .
#    HINT: you can use ifelse() and is.na() functions. When done, use table()
#    function to see the count of all categories.


d$BsmtCond = ifelse(is.na(d$BsmtCond),"DNE",d$BsmtCond)

table(d$BsmtCond)

# 3) Combine "Po" and "Fa" categories in a new category called "PoF" in the
#    "newBsmtCond" variable. Show category counts again using table().

b = d$BsmtCond %in% c("Po","Fa")
d$newBsmtCond = ifelse(b,"PoF",d$BsmtCond)

table(d$newBsmtCond)
View(d)

# 4) Now delete BsmtCond variable from the data frame.

d$BsmtCond <- NULL

# 5) Order the categories in newBsmtCond using factor() in the following order:
#    "DNE", "PoF", "TA", "GD".

d$newBsmtCond = factor(d$newBsmtCond, levels=c("DNE","PoF","TA","GD"))

# 6) Now get a boxplot between SalePrice and newBsmntCond. Comment on the chart.
#    Do you think these variables are associated?

boxplot(d$SalePrice ~ d$newBsmtCond)

# 7) Create heatmap for correlations between Numerical columns in the dataset.
#    Hint: Identify which columns (i.e., column numbers) are numeric first using str().
str(d)


library(reshape)

cor.mat = cor(d)

num.columns = c(2,3,4,10,11,12,15,17,18,19,21)
cor.mat = cor(d[num.columns])

cor.mat <- round(cor.mat, 2)
melted.cor.mat <- melt(cor.mat)

ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value) ) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "yellow", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(x = X1, y = X2, label = value),color="black")

# 8) Based on the heatmap in the previous question, you consider using ALL predictors
#   in a regression model, EXCEPT which variables? Why?:

#   We can consider all variables except LotArea, OverallCond, PoolArea
#   which are not correlated highly with SalesPrice.


# 9) Which two predictors can cause the "multicolinearity" problem?

#TotRmsAbvGrd and GrLivArea

# 10) First reorder categories in KitchenQual variable using factor() as shown below:
#     "Ex", "Gd", "TA", "Fa". Then, create a jitter plot with the following parameters:
#     y = SalePrice, x = GrLivArea, size= KitchenQual, color=YearRemodAdd, alpha = 0.30
#     height=2, width = 2. 
#     Comment on the chart.

d$KitchenQual = factor(d$KitchenQual,levels = c("Ex", "Gd", "TA", "Fa"))

table(d$KitchenQual)

ggplot(data = d) +
  geom_jitter( mapping = aes(x = GrLivArea, y = SalePrice, size= KitchenQual, color=YearRemodAdd), height=2, width = 2,alpha = 0.30 )



# new houses are more expensive than older houses with an increase in living area. Also, better kitchens are expensive due to larger living area


# 11) Create a scatterplot between SalePrice ~ YearBuilt. 
#     Which axis would you consider applying log transformation?
#     Rewrite the code by applying the log transformation.


plot(d$SalePrice,d$YearBuilt)

plot(d$SalePrice,d$YearBuilt, log = "y")

#X is crowded so apply log to y


       
                         

# 12) Create a multi window scatter plot betw#en GrLivArea and SalePrice with facet-Wrap variable
#     GarageCars. Comment on the chart.


ggplot(d) +  geom_point(aes(x = GrLivArea, y = SalePrice)) +   facet_wrap(~ d$GarageCars)

#If there is a a bigger living area, the price of the house with more garage space will increase more quickly than a house with less garage space.


# 13) Divide the plot screen into four windows. Create the following charts in each window.
# a) A histogram of SalePrice,
# b) Boxplot between SalePrice and KitchenQual
# c) Boxplot between SalePrice and FirePlaces
# d) Scatter plot between SalePrice and TotRmsAbvGrd



dev.off()
par(mfcol = c(2, 2))
hist(d$SalePrice)
boxplot(d$SalePrice ~ d$KitchenQual)
boxplot(d$SalePrice ~ d$Fireplaces)
plot(d$SalePrice ~ d$TotRmsAbvGrd)




