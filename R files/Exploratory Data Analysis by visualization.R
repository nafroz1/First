  # Exploratory Data Analysis using Visualization
  
  
  
  #-------------- Why visualization? ----------------
  # 1) Visualization can support data exploration and cleaning by finding incorrect
  # values (e.g., patients whose age is 999 or -1), missing values,
  # duplicate rows, columns with all the same value, and the like.
  
  # 2) Visualization techniques are useful for variable selection.
  # They can help determine which variables to include in the analysis.
  
  # 3) They can help with determining appropriate bin sizes (grouping size), 
  # should binning of numerical variables be needed.
  
  # 4) They can also play a role in combining categories as part of the 
  # data reduction process.
  
  
  #-------------- Data Visualization & Exploration with an Example ----------------
  
  # The Boston housing data contain information on census tracts in Boston for
  # which several measurements are taken (e.g., crime rate, pupil/teacher ratio).
  # collected in the 1970s.
  library(readr)
  BostonHousing <- read_csv("C:/Users/Nahid/Desktop/BostonHousing.csv")
  View(BostonHousing)
  
  boston.df <-read.csv("C:/Users/Nahid/Desktop/BostonHousing.csv")
  
  
  View(boston.df)
  
  # Size of the dataset            #how many rows and how many coloums use dim
  dim(boston.df)
  
  # The description of each variable:
  
  # CRIM Crime rate
  # ZN Percentage of residential land zoned for lots over 25,000 ft2
  # INDUS Percentage of land occupied by nonretail business
  # CHAS Does tract bound Charles River (= 1 if tract bounds river, = 0 otherwise)
  # NOX Nitric oxide concentration (parts per 10 million)
  # RM Average number of rooms per dwelling
  # AGE Percentage of owner-occupied units built prior to 1940
  # DIS Weighted distances to five Boston employment centers
  # RAD Index of accessibility to radial highways
  # TAX Full-value property tax rate per $10,000
  # PTRATIO Pupil-to-teacher ratio by town
  # LSTAT Percentage of lower status of the population
  # MEDV Median value of owner-occupied homes in $1000s
  # CAT.MEDV Is median value of owner-occupied homes in tract above $30,000 (CAT.MEDV = 1) or not
  # (CAT.MEDV = 0)
  
  
  #------------ What is the purpose here? ---------------
  # Our purpose in this analysis is to construct a prediction model
  # using linear regression.
  
  # The outcome variable could be: MEDV (median value of owner-occupied homes)
  # It is a numerical variable. Linear Regression approach is possible. 
  
  # Also, the outcome variable could be: CAT.MEDV that indicates whether the home value
  # is above or below $30,000. It is categorical binary variable (1/0).
  # A "classification" method is needed for this.
  
  # The predictors are: Possibly all the remaining variables
  
  
  #-------------- Explore & Visualize ----------------------------
  # Explore the target (outcome) variable
  # Explore the predictors
  # Explore the relationship between target and predictors
  #---------------------------------------------------------------
  
  # Are there any issues with the target variable?
  
  # Get a summary, histogram and boxplot
  
  summary(boston.df$MEDV)         # Max= 50, Min 5 = 45 units apart
  
  hist(boston.df$MEDV)              #slightly right skewed, mean is on the right side of median. - maybe indicating outliers on the right side
  
  boxplot(boston.df$MEDV)           #we have some outliers according to statistics
  
  
  # How many data points are outliers in the boxplot?
  
  IQR = Q3- Q1 = 8
  
  myIQR = 25 - 17.02
  mycutoff= 1.5*myIQR+25
  mycutoff
  
  sum(boston.df$MEDV > mycutoff)     #count of outliers
  
  # The rule says that a data point is an outlier if it is more than 1.5*IQR
  # above the third quartile or below the first quartile.
  
  # Write code for this:
  
  sum(boston.df$MEDV > mycutoff)/506    # percent of outliers relative to whole dataset
  #Why need to do this to find outliers whhy cant we just do boxplot
#  =7.5%
  
  #---------------------------------------------------------------
  
  # Let's review some interesting predictors:
  
  # RAD: Index of accessibility to radial highways
  # Looks like numeric but distinct values
  # It can also be treated ordinal categorical
  # Values change between 1-8, then jumps to 24
  # 1-8 distance to highways, then 24 may indicate "too far"
  # This is a reason we should treat this variable like categorical
  
    hist(boston.df$RAD)
  hist(boston.df$RAD, breaks = 40)     # a strange distribution with a big gap  #breaks = increasing the number of details) 
  
  # Generally, categories that contain very few observations are good candidates
  # for combining with other categories. Use only the categories that are most relevant
  # to the analysis and label the rest as “other.” If there is only one, treat this category like
  # "missing values".
  
  table(boston.df$RAD)
  table(boston.df$RAD)/dim(boston.df)[1]   # see if there is anything less than 1%? maybe dropped.
                                           # for example, if 7 is to be dropped, change its label to 24
                                           # who has the highest frequency (mod). OR, alternatively
                                           # distribute it to all other categories proportionally.
  
  boxplot(boston.df$MEDV ~ boston.df$RAD)
  
  # 9 different categories could be reduced without losing predictive power
  # Less number of categories means a more manageable variable -> a more compact model
  # because each category will be a column when dummies are created.
  
  # Looking at the boxplot, we can group these categories based on similarities.
  # Our criteria is "separation"
  
  # We can also use Tukey's HSD test to see any significant difference between the 
  # means of each category.
  
  test1 = aov(MEDV ~ as.factor(RAD), data=boston.df)
  summary(test1)
  a=TukeyHSD(test1, conf.level=.95) #when p value over 5% = there is no difference between each combinations. Eg" 24-5 shouldnt be combined.
  
  
  aa = as.data.frame(a$`as.factor(RAD)`)
  View(aa)    # convert to a table, view and sort by pvalue and review top and bottom parts
  
  # a)  1,2...8 is one group and 24 is another: two categories only.
  # b)  1+2+5, 4+6, 3+7+8, 24: Four categories
  # This way we can achieve a better "separation" of categories with respect to MEDV.
  
  # Write the code to insert variable newRAD
  
  #1 find category 1 or 2 or 5 and call them a category A, insert category A in newRAD
  
  #boston.df[boston.df$RAD== 1|boston.df$RAD== 2|boston.df$RAD== 5 , "RAD"] = "A" #this will overrrite the dataset
  
  #vertical bar is OR
  boston.df$newRAD [ boston.df$RAD== 1|boston.df$RAD== 2|boston.df$RAD== 5 ]= "A"  
  boston.df$newRAD [ boston.df$RAD== 4|boston.df$RAD== 6                   ] = "B"                    
  boston.df$newRAD [ boston.df$RAD== 3|boston.df$RAD== 7|boston.df$RAD== 8 ]= "C"  
  boston.df$newRAD [ boston.df$RAD== 24                                   ]= "D"  
  
  
  boxplot(boston.df$MEDV ~ boston.df$newRAD)
  
  
  # There were 9 categories, now there are 4 categories
  
  # Also, in regression, if one category is not significant, then it means
  # we can combine it with the reference category (the one that is not explicitly in the model)
  
  # Let's get back to the original dataset by removing newRad column.
  
  boston.df$newRAD = NULL #delete the whole new dataset/ overritten data
  
  # Example2
  # Assume our target variable is CAT.MEDV and ZN is a predictor.
  # However, ZN has too many numerical categories. Can we cut some of them?
  
  table(boston.df$ZN, boston.df$CAT..MEDV)
  
  
  # Identify similar categories
  # For example: Which categories are 100% CAT.MEDV = 1, or CAT.MEDV = 0
  
    a = table(boston.df$ZN, boston.df$CAT..MEDV)
    b = table(boston.df$ZN, boston.df$CAT..MEDV)/(a[,1]+a[,2])*100
    
    # Identify similar categories and label them
    
    b #combine the 100s together eg; 12, 12, 18, 21...) #same column same number
    
    #12.5, 18, 21, 25, 28, 30, 52.5, 70, 85 -> "A" # 100% 0 catMEDV     (categorizing into groups with same values)
    #17.5, 90, 95, 100                      -> "B" # 100% 1 catMEDV
    #34, 35, 55, 75, 80                     -> "C" # 67% 1 catMEDV
    #33, 82.5                               -> "D" # 67% 0 catMEDV
    #.....                                  -> "E" #other which includes categories with low frequencies 
    
    # How many ZN rows include the values 17.5, 90, 95, 100?
    
    sum(boston.df$ZN %in% c(12.5, 18, 21, 25, 28, 30, 52.5, 70, 85))
    
    sum(boston.df$ZN %in% c(17.5, 90, 95, 100)) #(%in% = are the values located in ZN?)
    
    sum(boston.df$ZN %in% c(34, 35, 55, 75, 80 ))
    
    sum(boston.df$ZN %in% c(33, 82.5   ))
    
    
    
    # Which rows?
    b=boston.df$ZN %in% c(12.5, 18, 21, 25, 28, 30, 52.5, 70, 85) # TRUE = one of these numbers, FALSE = none of #true rows are combines variables, false is not)
    
    boston.df$myNewZN = ifelse(b, "A", boston.df$ZN)                          # TRUE = one of these numbers, FALSE = none of
                                                                              #Put A if one of these
    b=boston.df$ZN %in% c(17.5, 90, 95, 100)
    
    boston.df$myNewZN = ifelse(b, "B", boston.df$myNewZN)
  
  
  b=boston.df$ZN %in% c(34, 35, 55, 75, 80 )
  
  boston.df$myNewZN = ifelse(b, "C", boston.df$myNewZN)
  
  
  b=boston.df$ZN %in% c(33, 82.5  )
  
  boston.df$myNewZN = ifelse(b, "D", boston.df$myNewZN)
  
  
  b=boston.df$ZN %in% c(0, 20, 22, 40, 45, 60)
  
  boston.df$myNewZN = ifelse(b, "E", boston.df$myNewZN)
  
  
  
  
  View(boston.df)
  table(boston.df$myNewZN)
  
  
  #---------------------------------------------------------------
  
  # How about MEDV and CHAS (ie. near Charles River)
  
  boxplot(boston.df$MEDV ~ boston.df$CHAS)
  
  # We see that not only is the average MEDV for river bounding homes
  # higher than the non-river-bounding homes, the entire distribution
  # is higher (median, quartiles, min, and max).
  # We can also see that all river bounding homes have MEDV above $10 thousand
  
  table(boston.df$CHAS)
  # Most places are away from the river
  
  
  #---------- Scaling and standardizing variables ---------
  # Sometimes variables' magnitudes are very different such as Age variable (0-100) and
  # Income variable (0-100,000). In some methods, including regression, such variables with large values
  # can get a larger weight unfairly just because of the magnitude of the values.
  # To avoid this problem, we may need to standardize all variable. Three ways to standardize:
  # a) (x - mean)/stddev   -> standardized around zero and number of std deviations
  # b) x/stddev            -> number of std deviations
  # c) x/(max - min)       -> fraction between [0,1]
  
  View(boston.df)
  
  # Two approaches: 
  # 1) Standardize ALL variables
  # 20 Standardize a few variables, such as CRIM (very small) and TAX (very big)
  
  boston.df$CRIM.std = 10*(boston.df$CRIM - mean(boston.df$CRIM)) / sd(boston.df$CRIM)
  boston.df$TAX.std = 10*(boston.df$TAX - mean(boston.df$TAX)) / sd(boston.df$TAX)
  
  boston.df$CRIM.std <- NULL
  boston.df$TAX.std <- NULL
  
  # Why 10? Multiplying by 10 to put the values in a similar magnitude with other columns
  
  View(boston.df)
  
  #---------- Binning a numerical variable ---------------
  
  # Continious numerical values can be converted to categorical values
  # when they are binned (ie grouped)
  
  library(reshape) #download package reshape if you dont have it already
  
  # create bins for variable Rm (ie average room size)
  summary(boston.df$RM)  # values are between 3 and 9
  hist(boston.df$RM)
  
  # Lets create bins like 1,2,3,...9 (some bins will be empty and will not show)
  
  boston.df$RM.bin <- .bincode(boston.df$RM, breaks=1:9)   # .bincode function in reshape library will do the job
  str(boston.df$RM)                                        # breaks are cutoff points for each bin 0-1, 1-2, 2-3,..
  summary(boston.df$RM)                                    # breaks must cover lowest value and highest value
                                                          # breaks=1:9 will look like this : 1-2, 2-3, 3-4, 4-5,5-6,6-7,7-8,8-9 computer will put the fractions in the categories, labels will be automatically decided
  str(boston.df$RM.bin)
  summary(boston.df$RM.bin)
  table(boston.df$RM.bin)
  
  # Lets create 3 bins only with new labels of 1,2,3
  # Include lowest and highest and in between desired number of bins
  
  boston.df$RM.bin2 <- .bincode(boston.df$RM, breaks = c(3,5,7,9) )  # lowest 3.5, highest 8.7
  str(boston.df$RM.bin2)
  summary(boston.df$RM.bin2)
  table(boston.df$RM.bin2)
  View(boston.df)
  
  #create 3 bins
  
  
  boston.df$RM.bin2 <- .bincode(boston.df$RM, breaks = c( 3,5,7,9))  #3-5, 5-7, 7-9 
  str(boston.df$RM.bin2)
  summary(boston.df$RM.bin2)
  table(boston.df$RM.bin2) 
  
  
  sum(boston.df$RM <=5)  # houses in 3-5 categry
  sum(boston.df$RM > 7)     #houses in 7-5 category
  
  
  
  #create 2 bins
  
  boston.df$RM.bin3 <- .bincode(boston.df$RM, breaks = c( 3,6,9))  #3-6, 6-9
  str(boston.df$RM.bin3)
  summary(boston.df$RM.bin3)
  table(boston.df$RM.bin3) 
  
  sum(boston.df$RM <=6)  # houses in 3-6 category
                         
  sum(boston.df$RM > 6)  # houses in 6-9 category
  
  
  
  #Exercise 2:
  #Age Using AGE varable, create a categorical (ie binned) AGE variable with 4 levels
  #Name it AGE.bin
  
  summary(boston.df$AGE)
  
  boston.df$AGE.bin <- .bincode(boston.df$AGE, breaks = c( 0, 25,50,75,101)) #0-25, 25-50 ,50- 75, 75-101 .....(divide into  four 25s), take 101 and 0, so it covers the whole  2.90 - 100 range
  table(boston.df$AGE.bin)
    
  
  #Another simple technique : rounding the number 
  boston.df$RM.bin4 = round(boston.df$RM)
  table(boston.df$RM.num)
  
  boston.df$RM.bin <-NULL
  boston.df$RM.bin2 <-NULL
  boston.df$RM.bin3 <-NULL
  boston.df$RM.bin4 <-NULL
  boston.df$RM.num <-NULL
  boston.df$AGE.bin <-NULL
  
  
  
  
  
  sum(boston.df$RM > 7)  # 64
  sum(boston.df$RM <= 5)
  sum(boston.df$RM > 5 & boston.df$RM <= 7)
  
  #---------- Converting a binned variable to numeric variable
  
  # We can use the midpoint to convert to a number
  table(boston.df$RM.bin)
  boston.df$RM.num = boston.df$RM.bin + 0.5
  table(boston.df$RM.num)
  
  
  
  
  # ----------- MULTI PANEL PLOTS ---------
  # Side-by-side boxplots: 
  # Use par() to split the plots into panels.
  # To create a matrix of nrows x ncols plots:
  
  # mfcol=c(nrows, ncols) fills in the matrix by columns.
  # mfcol = c(1, 4): One row, 4 plots
  
  dev.off()   # reset device just in case
  
  par(mfcol = c(1, 4))   
  
  # Let's create 4 boxplots
  # x-axis should be categorical and y-axis should be numerical
  # We put CAT.MEDV in the X-axis and create boxplots back to back
  # each time with a different variable in the y-axis
  
  
  par(mfcol = c(1, 4))
  boxplot(boston.df$NOX ~ boston.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "NOX")
  boxplot(boston.df$LSTAT ~ boston.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "LSTAT")
  boxplot(boston.df$PTRATIO ~ boston.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "PTRATIO")
  boxplot(boston.df$INDUS ~ boston.df$CAT..MEDV, xlab = "CAT.MEDV", ylab = "INDUS")
  
  
  # The pairs that are most separated (e.g., LSAT)
  # indicate potentially useful predictors.
  
  #------------ VISUALIZING DATA WITH MORE THAN 2 VARIABLES ---------------
  
  dev.off()   # reset device
  
  
  # Create a scatter plot between two numerical variables
  # plot(Y ~ X) alternatively plot(X,Y)
  
  # y-axis: MEDV, x-axis:LSTAT   (LSTAT: percentage of lower status)
  
  plot(boston.df$MEDV ~ boston.df$LSTAT)
  
  plot(boston.df$LSTAT,boston.df$MEDV) 
  
  # MEDV is response variable and AGE is predictor
  
  plot(boston.df$MEDV ~ boston.df$AGE, xlab="AGE", ylab="MEDIAN VALUE")
  
  # Standard R graphics are great and quick but sometimes you may want to use
  # a more professional package like ggplot2
  
  library(ggplot2)  # by hadley wickham
  
  
  # 1) Initiate the plot by ggplot()
  # Inside, write what data and what general properties
  # of the plot needed. This will be the first "layer"
  
  # 2) Plot the next layer by a "+" sign: Determine the graph 
  # type: For example: 
  #      geom_point() : scatter chart
  #      geom_bar()   : bar chart
  #      geom_smooth(): smooth line
  #      geom_jitter(): a jitter chart
  
  # 3) Inside the geom(), write an aes() i.e. details of the chart
  #    visual elements that you want to assign a variable
  #    for example: x-axis, y-axis, color, transperancy level, size of the element
  #    If they are outside of the aes(), it means they are constant, not depending on any variable.
  #    Example: geom_point(aes(shape=x, color=y, size=z))
  
  #---------- SCATTER PLOTS -----------------------------
  
  # x = LSTAT, y = MEDV
  # Create a scatter plot
  
  ggplot(data = boston.df) +
      geom_point(mapping = aes( x = LSTAT, y = MEDV), color = "navy", alpha = 0.30)
  
  # Alpha is the level of transparency. Lower numbers
  # indicate lighter dots, which helps us to see
  # which areas in the map are denser.
  
  # Another example: x = AGE, y = MEDV
  ggplot(data = boston.df) +
    geom_point( mapping = aes(x = AGE, y = MEDV) , color = "red", alpha = 0.30  )
  
  
  # Now a third variable introduced into the picture
  # We can use transparency level or color for the 3rd variable
  # CHAS will be the 3rd one
  
  # transparency is variable
    ggplot(data = boston.df) +
      geom_point(mapping = aes( x = LSTAT, y = MEDV, alpha = CHAS), color = "navy" )
    
    # color is variable
    ggplot(data = boston.df) +
      geom_point( mapping = aes(x = LSTAT, y = MEDV, color = CHAS ) , alpha = 0.70 )
    
    # size is variable
    
    ggplot(data = boston.df) +
      geom_point( mapping = aes(x = LSTAT, y = MEDV, size = AGE ) , alpha = 0.40 )
    
    # shape is variable
    ggplot(data = boston.df) +
      geom_point( mapping = aes(x = LSTAT, y = MEDV, shape = as.factor(CHAS) ) , size=6, alpha = 0.40 )
    
    
    # shape, size and color are variable
    ggplot(data = boston.df) +
      geom_point( mapping = aes(x = LSTAT, y = MEDV, shape = as.factor(CHAS), size=AGE, color=RAD ), alpha = 0.50 )
    
    
  #----------- Scatter plot matrix using a different package ------------
  library(GGally)
  ggpairs(boston.df[, c(1, 3, 12, 13)])
  
  # Observations:
  # 1) a nonlinear (exponential) relationship between MEDV and LSTAT
  # 2) the nature of the relationship between MEDV and CRIM is hard 
  #    to determine in the original scale, because too many of 
  #    the points are “crowded” near the y-axis
  
  # We need rescaling here using logarithm
  
  #--------- USING LOGARITHMIC SCALE ---------------------------------
  
  plot(boston.df[, c(1, 3, 12, 13)])
  plot(boston.df)
  
  # Some plots are very dense closer to the X axis or Y axis
  # We can open this up if we use log conversion in charts
  
  options(scipen=999) # avoid scientific notation  1423e-10
  
  # Create a plot between MEDV and CRIM in Y ~ X notation
  
  plot(boston.df$MEDV ~ boston.df$CRIM, xlab = "CRIM", ylab = "MEDV")
  
  
  # Data almost attached to the Y axis; Need to apply log on the other axis
  # To open it up, use logarithmic conversion on the X variable
  # in this case, it is CRIM
  
  plot(boston.df$MEDV ~ boston.df$CRIM, 
       xlab = "CRIM", ylab = "MEDV", log = 'x')
  
  # OR apply log transformation on both axes
  
  plot(boston.df$MEDV ~ boston.df$CRIM, 
       xlab = "CRIM", ylab = "MEDV", log = 'xy')
  
  # To use logarithmic scale set argument log = to either 'x', 'y', or 'xy'. 
  # xy means both X and Y
  
  plot(boston.df$MEDV ~ boston.df$LSTAT, 
       xlab = "CRIM", ylab = "MEDV")
  
  plot(boston.df$MEDV ~ boston.df$LSTAT, 
       xlab = "CRIM", ylab = "MEDV", log = 'x')
  
  
  
  # alternative log-scale plot with ggplot 
  # log conversion on X and Y
  
  ggplot(boston.df) + 
    geom_point(aes(x = CRIM, y = MEDV)) + 
    scale_x_log10()
  
  dev.off()
  
  ## boxplot: regular and log scale                                 #you see x is crowded then put log on Y
  boxplot(boston.df$CRIM ~ boston.df$CAT..MEDV, 
          xlab = "CAT.MEDV", ylab = "CRIM")
  
  # Data attached to X axis, apply log on the other axis, which is Y
  boxplot(boston.df$CRIM ~ boston.df$CAT..MEDV, 
          xlab = "CAT.MEDV", ylab = "CRIM", log = 'y')
  
  # Consider using LOG(CRIM) instead of just CRIM variable in the regression model 
  
  #------- JITTERPLOT ----------------------------------------------
  # Jitter plot is used to prevent points to be plotted on top of each other
  # Alternative to using transparency level alpha 
  
  ggplot(data = boston.df) +
    geom_jitter(mapping = aes(x = LSTAT, y = MEDV), height=2, width = 2, color = "navy")
 
  #------- BARPLOTS --------------------------------------------
  # Bar charts to get other insights
  
  # Now one categorical and one numeric variable
  # Put the categorical on the x-axis, numerical "summary" values on the y-axis
  # The height of the bars need to be calculate ahead of time
  # This calculation can be based on "aggregation" of data which
  # could be one of these: count, sum, average, standard deviation, etc.
  
  
  # This is a summary (i.e.pivot) table with means (instead of counts of table() function)
  # Use the function aggregate()
  
  # aggregate(data column, FUN = function to be applied on data col, by = a list of categorical variables)
  
  # Just like Excel pivot tables:
  # aggregate(value field, FUN = count, sum, average, etc., by = list(row variable, column variable) )
  
  
  # Create a barchart, mean of MEDV on the Y-axis, and CHAS on the x-axis
  
  data.for.plot = aggregate(boston.df$MEDV, FUN = mean, by = list(boston.df$CHAS))
  colnames(data.for.plot) = c("CHAS", "MeanMEDV")  #rename the levels to CHAS and MEANMEDV 
  
  # IN barplot x axis categorical, y axis numberical
  
  # Using standard R graphics:
  # barplot(height of the bars, xlabel, ylabel, names.arg = categorical variable's categories)
  
  barplot(data.for.plot$MeanMEDV, xlab = "CHAS", ylab = "Mean MEDV", names.arg = data.for.plot$CHAS)
  
  #------ Alternative plot with ggplot2
  
  ggplot(data.for.plot) +
    geom_bar(aes(x=CHAS, y=MeanMEDV), stat="identity")
  
  
  #---------- COMBO CHARTS --------------------------------
  # Create a scatter plot + a prediction line (trendline in Excel)
  # Gray area around the line is confidence level for the predictions 
  # (because we use a sample, not the population)
  
  ggplot(boston.df) +
    geom_point(aes(x = LSTAT, y = MEDV)) + 
    geom_smooth(aes(x = LSTAT, y = MEDV), method = "lm") +      # Linear regression line
    xlab("LSTAT") 
  
  
  
  ggplot(boston.df) +
    geom_point(aes(x = LSTAT, y = MEDV)) + 
    geom_smooth(aes(x = LSTAT, y = MEDV), method = "lm") +      # Linear regression line
    xlab("LSTAT")
  
  #blue line is confidence interval The fatness of the light blue color around the blue line tells you CI.  fatter towards the end could be 95
  
  ggplot(boston.df) +
    geom_point(aes(x = LSTAT, y = MEDV)) + 
    geom_smooth(aes(x = LSTAT, y = MEDV), method = "loess") +   # nonlinear, curve
    xlab("LSTAT")
  
  
  #---------- FACETS --------------------------
  # Similar to multipanel charts in standard R except
  # we make one panel (ie chart) for each category of a variable
  
  # Create a scatter plot for LSTAT & MEDV in each CHAS category
  
  
  ggplot(boston.df) +
    geom_point(aes(x = LSTAT, y = MEDV) ) + 
    facet_wrap(~ CHAS)  #useful for seperating two events / categories
  
  
  # Create a scatter plot for LSTAT & MEDV in each RAD category
  
  ggplot(boston.df) +
    geom_point(aes(x = LSTAT, y = MEDV) ) + 
    facet_wrap(~ RAD)
  
  
  #----------- HEATMAPS FOR CORELATIONS ---------
  # Correlations between variables can tell us which variable can be
  # used a predictors.
  
  # To get correlations between variables, we can use cor()
  # For example, I am interested to know what variables are correlated with MEDV
  # And also, correlations between predictors.
  
  
  cor.mat = cor(boston.df)    #higher value indicate different color than lower values
  
  class(cor.mat)     # matrix is just like a dataframe except it is either
  
  
  boston.df$myNewZN <- NULL   #Pay attention to the direction of the arrow 
  
  # numerical or text; cannot be mixed as in dataframe
  
  
  heatmap(cor.mat, Rowv = NA, Colv = NA)   # Not really nice looking, confusing

# Let's use ggplot

library(ggplot2)
#install.packages("reshape")
library(reshape) # to generate input for the plot

cor.mat = cor(boston.df)
cor.mat <- round(cor.mat, 2)    # rounded correlation matrix #after decimal point we will only keep 2, thats why there is 2)
View(cor.mat)                    # Hard to read, we need visual aids to understand it

# So, we will use ggplot2's heat map feature
# However, ggplot needs a dataframe, not a matrix. How can you convert
# a matrix to dataframe?

# Convert the corr matrix into dataframe
# Dataframe requires table form, not matrix form
# Such as a(1, 2) = 0.8 in matrix notation will be
# [1, 2, 0.8] in dataframe notation
# [X1, X2, corr value]

# We need to use melt() function to do this, converts matrix to dataframe

melted.cor.mat <- melt(cor.mat)
View(melted.cor.mat)


# [X1, X2, value]
# Heatmap: White positively correlated, dark negatively correlated
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value) ) + 
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value))


# A more advanced version:

ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value) ) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "pink", high = "navy", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(x = X1, y = X2, label = value),color="black")



#----------- HEATMAPS FOR MISSING VALUES ---------
# We can use heatmap approach for missing values as well.

library(readr)
census <- read_csv("C:/Users/Nahid/Desktop/census.csv")
View(census)
sum(is.na(census)) 

missing.mat = 1 * is.na(census)   # 1 * TRUE = 1  and 1 * FALSE = 0. This is a simple method  #0 is not missing, 1 is missing
                                  # to convert logical values to numeric
# is.na is false 
#na is true

class(missing.mat)
View(missing.mat)


# Same heatmap with ggplot

melted.missing.mat <- melt(missing.mat)
View(melted.missing.mat)    #use this code anytime with dataset for missing values

ggplot(melted.missing.mat, aes(x = X1, y = X2, fill = value) ) + 
  geom_tile() + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Missing Val") 


# Total family income and total personal income has missing data
# How do we treat it?

# One way to do this is "Imputation".
# It means replacing NAs with a reasonable value, such as median or average
# value of the variable. Median can be more preferable over average when 
# the variable is integer type because the average could be fractional, median will not be.
# For categorical values, you can use Mode (most frequent category) for imputation.

# The extent of the problem:
sum(is.na(census$total_family_income))/length(census$total_family_income)
sum(is.na(census$total_personal_income))/length(census$total_personal_income)


# Now get the median total_family_income. Make sure median is calculated
# by ignoring NAs.

medianIncome = median(census$total_family_income, na.rm = TRUE)
summary(census$total_family_income)  # before treatment

census[ is.na(census$total_family_income), "total_family_income" ] = medianIncome
summary(census$total_family_income)  # after treatment


# total_personal_income has 22% missing
# So, should we drop this column from our regression model?
# It is hard to answer this question. Some people
# choose to drop a variable if it has more than 30% missing. Others say more than 50%.
# If the variable is an important variable, we hesitate to drop it. Instead, we impute it.

# Let's impute the total_personal_income and total_family_income variable with their median.

medianTotPersIncome = median(census$total_personal_income, na.rm = TRUE)
census[ is.na(census$total_personal_income) , "total_personal_income" ] = medianTotPersIncome
summary(census$total_personal_income)  # after treatment


#Exercise 3

#Draw heatmap

library(readr)
WestRoxbury.csv <- read_csv("C:/Users/Nahid/Desktop/WestRoxbury.csv")
View(WestRoxbury.csv)

d = WestRoxbury.csv
View(d)


cor.mat = cor(WestRoxbury.csv)    #higher value indicate different color than lower values

class(cor.mat)     # matrix is just like a dataframe except it is either


WestRoxbury.csv$REMODEL <- NULL   #Pay attention to the direction of the arrow 

# numerical or text; cannot be mixed as in dataframe


heatmap(cor.mat, Rowv = NA, Colv = NA)   # Not really nice looking, confusing

# Let's use ggplot

library(ggplot2)
#install.packages("reshape")
library(reshape) # to generate input for the plot

cor.mat = cor(WestRoxbury.csv)
cor.mat <- round(cor.mat, 2)    # rounded correlation matrix #after decimal point we will only keep 2, thats why there is 2)
View(cor.mat)                    # Hard to read, we need visual aids to understand it



melted.cor.mat <- melt(cor.mat)
View(melted.cor.mat)


# [X1, X2, value]
# Heatmap: White positively correlated, dark negatively correlated
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value) ) + 
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value))


# A more advanced version:

ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value) ) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "yellow", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(x = X1, y = X2, label = value),color="black")



#heat map----------------------------------------------- MEMORIZE THIS --- COPY AND MODIFY AS NEEDED-----------------
  cor.mat = cor(d[, 1:13])     
  cor.mat <- round (cor.mat, 2)
  melted.cor.mat <- melt(cor.mat)
  
  
  ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value) ) + 
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "yellow", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Correlation") +
    geom_text(aes(x = X1, y = X2, label = value),color="black")
  
  #2 Create a scatter plot using ggplot2. Take y=total value, x=living area ----------------copy paste use-----------
  # size is rooms, color = full bath
  

  ggplot(data = d) +
    geom_point(mapping = aes( x = LIVING AREA, y = TOTAL.VALUE), size = ROOMS, color =FULL.BATH", alpha = 0.50) #color changes based on variable
  
  
