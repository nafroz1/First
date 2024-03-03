    
    # This is a dataset about houses in Boston. 
    # Our purpose here is to predict a new house's value using other houses
    # in the dataset.
    
    #we always start with regression. Oldest technique.
    
    # Supervised learning and prediction task: There is target and it is numerical.
    #for linear regression, target and predictor must be numeric
    #if not numeric, convert it or dont use linear regression
    
    
    
    housing.df <- read.csv("WestRoxbury.csv")    # load the data into memory
    
    housing.df=WestRoxbury <- read_csv("C:/Users/Nahid/Desktop/WestRoxbury.csv")
    View(housing.df)
    
    str(housing.df) # find the structure, how many numeric, how many character and where
    #If data is categorical non-numeric dont worry it will convert on its own
    #Chr here means categorical eg: Yes, No, NOne. Names are not categorical. How many categories can you split this data into?
    
    table(housing.df$REMODEL)  #how many each categories here
      
    housing.df$REMODEL = as.factor(housing.df$REMODEL) #overwrites the original remodel data to factors) - use this for nominal values
      
    housing.df$REMODEL = as.factor(housing.df$REMODEL, levels=c("None","Old","Recent"))
    - # use this code For ordinal values
    
    str(housing.df)
                                     
    #----------------------------------------------------------------------------
    # 1) Data understanding
    #----------------------------------------------------------------------------
    
    # Understand the data: Review each column and make changes if needed
    
    # If the text data is read as "chr" by read.csv, it means it is a text type.
    # Character is good for longer text (not categorical) such as first, last names
    # and feedback of customers.
    # For categorical data, the proper text type is "factor".
    # read.csv() function may read all text data as "chr" or "factor" depending on default settings.
    # If a categorical data is read as "chr" then you should convert it to factor type
    # using function as.factor()
    
    str(housing.df)     # show the type of all columns
    
    # remodel variable is read as character type while it is actually categorical
    
    
    # housing.df$REMODEL = as.factor(housing.df$REMODEL, levels=c("None", "Old", "Recent")).   # if it was ordinal
    
    # 2) Identify outliers, missing values and other irregularities
    #    use summary() function
    
    summary(housing.df)
    
    # If you want a visualization, use plot() or boxplot() function.
    
    plot(housing.df$LOT.SQFT)
    
    # Delete decision depends on the possible impact of these outliers in
    # the prediction model.
    
    
    
    #-----------------
    # We are concerned if tax is a mathematical function of total value.
    # We need to check this either visually, or numerically
    # 99, 100 percent correlation will indicate such relationship
    
    plot(housing.df$TAX,housing.df$TOTAL.VALUE) #whenever there is relationship like total value is dependent is tax, in that case one data is redundant, so we can remove tax and keep total  value.
    
    cor(housing.df$TAX,housing.df$TOTAL.VALUE) #correlation between TAX and Total Value
    
    # We identified that tax is a derived variable of total value.
    # We cannot use it for predicting total value. This column is out.
    
    housing.df$TAX <- NULL #this is the code for deleting an entire coloumn. In this case it is TAX since it is redundant.
    
    
    
    #-----------------
    
    
    # We spotted a zero for the YR.BUILT variable. What is the extent of this typo?
    
    #housing.df$YR.BUILT == 0 ( this will give us false, false, true etc but long process)
    
    sum(housing.df$YR.BUILT == 0) #this one is a count, we are counting the total number of zeros are here in Yr built.
    
    housing.df[housing.df$YR.BUILT==0, ] #exact row number: 1493
    
    # Delete or impute? Since we have a large dataset,
    
    
    
    # we decide to delete the entire row.
    
    housing.df = housing.df[housing.df$YR.BUILT !=0, ] #!=0 means not equal to. We will take all values except 0
    
    #-----------------
    # Check for missing values in the dataset
    
    sum(is.na(housing.df))
    
    
    #----------------------------------------------------------------------------
    # 2) Selecting the appropriate method
    #----------------------------------------------------------------------------
    
    # Our target (response) variable is Total Value
    # The remaining variables are possible predictors
    
    # Type of the outcome variable: Numeric
    # Type of the predictors : Numeric except remodel variable
    
    # Functions lm() and glm() will be used in our regression analysis and they handle
    # categorical variables by creating dummies automatically.
    #lm does same as glm but glm is more extensive
    
    
    
    
    # Why do we do regression analysis?
    # 1) Prediction purposes: Business analytics professionals use regression as a prediction method.
    # 2) Understanding relationship between predictors and the outcome variable. This is
    #    mostly used by scientists/economists doing research.
    #    The impact of the regressions coefficients (ie. average impact of each variable) 
    #    on the target variable is the focus rather than predictions using the model.
    
    #    For example, the following conclusions can be made after the regression
    #    model is run:
    #    - Total value increases by 0.01977 thousand dollars on average
    #    for every square foot increase in lot sqrft.
    #    - Total value decreases by 0.294 dollars on average
    #    in each year.
    
    
    # Typical multiple predictor linear regression model looks like this:
    
    # Y = B0 + B1X1 + B2X2 + B3X3 +... BnXn + e               #multiple regression equation
    
    # Y: Actual value of the numerical target variable
    # Bi: Regression coefficients that need to be calculated/estimated using Ordinary Least Squares method
    # Xi: Predictors
    # e : error term (actual - prediction)
    
    # Each row in the dataset is one point of Y and Xs
    # When all Bi are calculated, we have a prediction model.
    # To make a prediction, we plug in a new dataset of X values in the model
    # and calculate Y.
    
    
    #----------------------------------------------------------------------------
    # 3) Selecting the predictors
    #----------------------------------------------------------------------------
    
    # The next challenge is selecting the best predictors for the prediction job.
    # We tend to add ALL possible columns as predictors. However, there are
    # some problems with this approach, such as:
    # a) When we have too many predictors, we are required to collect future data 
    #    for each of these predictors which may be expensive or unavailable or
    #    may contain missing data.
    # b) Predictors may highly correlate with each other which leads to "multicolinearity"
    #    problem. If this is the case, coefficient estimations will unreliable.
    # c) The model becomes too complex to understand and explain. 
    
    
    # Predictor selection methods:
    # 1) We can get assistance from an expert in the domain of the dataset
    #    to tell us which predictors are most important to predict the outcome.
    # 2) Look at the correlations between the outcome variable and the predictors
    #    Include the ones that are highly correlated.(caution: correlation does not imply causation)
    #    Watch for extensive predictor to predictor correlations. This indicates multicolinearity problem.
    # 3) Also, review the scatter plot matrix to see relationships.
    # 4) You can use automated search procedures trying each predictor in and out of the model,
    #    such as "Stepwise Regression".
    # 5) You can use "Ridge Regression" to overcome multicolinearity and you can use 
    #    Principle Component Analysis (PCA) to reduce the number of predictors.
    
    
    # Examples:
    # Visualizations to see the pairwise relationships between Y and X
    
    # Let's consider the first 9 variables
    View((housing.df))
    
     plot(housing.df[ , 1:9]) #this is the scatterplot for each predictor with total value
     
    
    # When you examine these plots, consider two things:
    
    # 1) Outcome variable vs. predictors: Look for an upward
    #    downward trend. A plot that looks like flat indicates
    #    no or week relationship/association
    
    # 2) predictor vs Predictor: An upward or downward trend may indicate
    #    an undesired phenomenon called "Multicollinearity", which is 
    #    the occurrence of high intercorrelations among two or more independent variables
    #    in a multiple regression model. It will make your Bi coefficients and the model
    #    in general less reliable. In other words, you have two variables carrying
    #    almost the same information, one needs to go, or advanced techniques needed
    #    to overcome the problem (such as Ridge Regression).
    
    
    # The following code shows how to get single predictor charts, Y ~ X
    
    
    cor(housing.df$GROSS.AREA, housing.df$LIVING.AREA) #so we can use one either grossarea or livingarea
    
    # How about corelation coefficients to see the extents of association
    # between two variables?
    
    
    
    
    #  Almost 90% corelation indicates
    #  possible multicolinearity problem
    #  if we use GROSS.AREA and LIVING.AREA
    #  in the same regression model.
    #  We should use only one of them
    #  to avoid multicolinearity.
    
    
    
    # Let's start with the following initial regression model for prediction purposes
    # TOTAL VALUE = B0 + B1 * LOT SQRFT + B2 * YR BUILT + B3 * LIVING AREA + B4 * ROOMS
    
    reg1 = lm(TOTAL.VALUE ~ LOT.SQFT + YR.BUILT + LIVING.AREA + ROOMS, data = housing.df)
    
    #----------------------------------------------------------------------------
    # 4) Partition the dataset into "Training" and "Validation"
    #----------------------------------------------------------------------------
    
    # We need to decide the size of the these partitions. There is no fixed rule for this
    # but it is a general practice to use 70% or 60% for training and remaining for the validation.
    
    # The data in these partitions must be randomly selected. This random assignment should be
    # replicated when needed. It means the conclusions should be verified by others using the 
    # same random numbers. This can be done using same value in the function set.seed(value).
    
    # We decide that training partition will be 70%
    # and remaining 30% will be the validation partition
    
    set.seed(2023) #setting seed to 2023
    
    total.rows = dim(housing.df)[1] #total number of rows
    
    train.rows <- sample( c(1:total.rows), total.rows*0.7 ) #out of total I am taking 70% of them
    
    train.data <- housing.df[train.rows, ]
    valid.data <- housing.df[-train.rows, ]
    
    #----------------------------------------------------------------------------
    # 5) Create the model and run
    #----------------------------------------------------------------------------
    
    reg1 = lm(TOTAL.VALUE ~ LOT.SQFT + YR.BUILT + LIVING.AREA + ROOMS, data = train.data)
    
    summary(reg1)
    
    # We will use linear Model lm() function to create a linear regression model
    
    # modelname = lm(response variable ~ predictor variables separated by "+" sign, data=training partition data)
    
    
    
    # Check the normality condition of the residuals (ie. prediction errors)
    # residuals = errors = e = actual - predicted
    
    # Visually check if the distribution of the residuals are normally distributed
    # 1) scatter plot or 2) histogram
    
    plot(reg1$residuals)
    hist(reg1$residuals)
    
    #it seems the residuals are nearly normal. Normality assumption is confirmed
    # This rule is not very restrictive when it comes to predictions. It is important
    #when investigating impact of predictors on the target.
    
    #The report generated by the regression model shows us the significance of 
    #each predictor . We are looking for atleast 10% signifcane (p-values)
    #In reg1, all predictors are significant
    #----------------------------------------------------------------------------
    # 6) Validate the model
    #----------------------------------------------------------------------------
    
    # Since we have the prediction model, now it is time to 
    # validate it on the validation partition
    # Procedure: 
    # 1) use the model and predict TOTAL VALUE in the
    # validation partition. 
    # 2) Compare actual values in validation
    # to the predicted values. How different they are overall?
    # There are measures for this based on error = actual - predicted
    
    # ME = mean(actual - fitted)                          Mean Error
    # MSE = mean((actual - fitted)^2)                     Mean Squared Error
    # RMSE = SQRT(mean((actual - fitted)^2))             Root Mean Squared Error
    # MAE = mean(ABS(actual - fitted))                    Mean Absolute Error or Mean Absolute Deviation (MAD)
    # MPE = mean((actual - fitted) / actual)) x 100       Mean Percentage Error
    # MAPE = mean(ABS(actual - fitted) / actual)) x 100   Mean Absolute Percentage Error / residuals
    
    # RMSE - how much dollars on average you making error
    
    # ME:
    # Errors are sometimes positive and sometimes are negative. When you add them up
    # and get the mean, they cancel each other off. Normally, you should get a very small
    # ME value. If not -> It means you are systematically under or over predicting on average.
    
    
    # RMSE: This is one of the most important performance measures. It should be a small number.
    # In other words, choose the technique with the lowest RMSE.
    
    # Let's make predictions on the validation dataset.
    # Use predict()
    
    pred1 = predict(reg1, newdata = valid.data) #pred1 is the target. Predicting the TOTAL.VALUE using validation
    pred1
  
  
  
  # At this point, we need another package to calculate accuracy measures
  # It is called "forecast"
  
  # install.packages("forecast")
  library(forecast)
  
  # Now let's use function accuracy() from the "forecast" library
  
  # accuracy(predicted target values, actual target values)
  
  accuracy(pred1, valid.data$TOTAL.VALUE) #comparing with each row = like actual vs predicted
  
  # Accuracy of reg1 model on the validation dataset:
  #             ME       RMSE      MAE       MPE     MAPE
  # Test set 1.253675 48.04713 36.61889 -1.241814 9.446921
  
  pred2 = predict(reg1, newdata = train.data)
  
  
  # Accuracy of reg1 model on the training dataset:
  
  accuracy(pred2, train.data$TOTAL.VALUE) #doing on training data now


#                  ME     RMSE      MAE       MPE     MAPE
# Test set 2.216193e-12 50.31835 37.74072 -1.603303 9.803901

#Compared to Test Set 1 vs Test Set 2 (should have too much differences)

# Compare the accuracy measures in the training and validation

#----------------------------------------------------------------------------
# 6) Scoring new data: Use the model to make prediction on a new data
#----------------------------------------------------------------------------

# A new property has arrived, with the following information:

#  LOT.SQFT = 10000, 
#  YR.BUILT = 1920,
#  GROSS.AREA = 2500,
#  LIVING.AREA = 1500,
#  FLOORS = 2,
#  ROOMS = 8,
#  BEDROOMS = 4,
#  FULL.BATH = 2,
#  HALF.BATH = 1,
#  KITCHEN = 1,
#  FIREPLACE = 1,
#  REMODEL = "Old"


# reg1 = lm(TOTAL.VALUE ~ LOT.SQFT + YR.BUILT + LIVING.AREA + ROOMS, data = train.data )

new.property = data.frame(
  
  LOT.SQFT = 10000, 
  YR.BUILT = 1920,
  GROSS.AREA = 2500,
  LIVING.AREA = 1500,
  FLOORS = 2,
  ROOMS = 8,
  BEDROOMS = 4,
  FULL.BATH = 2,
  HALF.BATH = 1,
  KITCHEN = 1,
  FIREPLACE = 1,
  REMODEL = "Old"
  
)

predict(reg1, newdata = new.property)
#the way to write a prediction is name of regression model, then newdata/valid data or training data and then put the name of the new data name, in this case it is new property)

#predicted TOTAL.VALUE is 405.2919 
#most important judgement is RMSE, ME ..


#----------------------------------------------------------------------------
# 7) Summary
#----------------------------------------------------------------------------

# Linear regression models are very popular tools, not only for explanatory modeling,
# but also for prediction. A good predictive model has high predictive accuracy
# (to a useful practical level). Predictive models are fit to training data, and 
# predictive accuracy is evaluated on a separate validation data set.
# Removing redundant predictors is key to achieving predictive accuracy and robustness.
# Trying different predictor subsets can help find “good” predictive models. 
# Each alternative model should be run and assessed. After selecting the best model,
# it is used for predicting new data.












