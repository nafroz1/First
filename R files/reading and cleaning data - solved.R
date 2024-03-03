    #-----------------------------------------
    # Identifying the business problem
    
    # Retail: Identifying high-value customers and personalizing marketing campaigns to increase sales.
    # Banking: Detecting fraudulent transactions by analyzing patterns in customer behavior.
    # Manufacturing: Optimizing production processes by identifying patterns in machine data to predict equipment failure.
    # Healthcare: Identifying patients at risk of readmission to the hospital by analyzing patient data and medical history.
    # e-commerce: Recommending products to customers based on their browsing and purchasing history.
    # Telecommunications: Identifying customers at risk of churning by analyzing call and data usage patterns.
    # Insurance: Identifying fraudulent claims by analyzing patterns in claims data.
    # Finance: Identifying potential credit risk by analyzing patterns in financial data.
    # Energy: Optimizing energy consumption by identifying patterns in usage data.
    # Transportation: Identifying inefficiencies in logistics and supply chain management by analyzing patterns in shipping and delivery data.
    
    #-----------------------------------------
    # Acquiring the Data
    
    # Reading datasets
    
    # Data can be posted on Internet websites in which case you can download and
    # read it to the Rstudio.
    
    # Data comes in many formats including excel, text, html, xml, json.
    # Most popular format is "csv", comma seperated values.
    # Use the right function to read the right format.
    
    # Base R provides read.csv() function for reading csv files. 
    # Also, Readr package of tidiverse provides many functions to read data.
    
    # For example:
    # library(readr)
    # read_csv()      # equivalent of read.csv() but with more features
    
    d = read.csv("RentalProperties.csv")
    d = read.csv("C:/Users/Nahid/Desktop/RentalProperties.csv")
    head(d)
    
    # Other times, data comes from a company's relational database spread over
    # many data tables. You need to have access credentials and knowledge of the table names,
    # and relationship keys (i.e. primary keys and foreign keys) between tables.
    # dplyr and dbplyr package provides functions to establish connection to a database and
    # link tables together using the keys.
    # (this topic is covered in your BANL6430 Database Management course)
    
    #-----------------------------------------
    # Tidying the Data
    
    # Tidying data refers to the process of structuring data in a consistent and organized manner,
    # so that it can be easily analyzed and understood. This typically involves organizing data
    # into a specific format, such as a table, and ensuring that each variable is in its own column
    # and each observation is in its own row.
    
    # Example
    # Untidy data:
    # Consider the following dataset that represents the results of a survey:
    
    # Respondent	Q1  	Q2	  Q3  	Q4  	Q5 
    # 1	          Yes	  No				
    # 2	          No	  	    Yes			
    # 3           				          Yes	  No	
    
    # To tidy this data, it would need to be reshaped so that each variable
    # has its own column and each observation has its own row.
    # Tidy data:
    
    # Respondent	Question    Answer
    # 1              1          Yes
    # 1              2          No
    # 1              3          NA
    # 1              4          NA
    # 1              5          NA
    # 2              1          No
    # 2              2          NA
    # 2              3          Yes
    # 2              4          NA
    # 2              5          NA
    # 3              1          NA
    # 3              2          NA
    # 3              3          NA
    # 3              4          Yes
    # 3              5          No
    
    
    # Additionally, tidying data may also involve cleaning and transforming the data to remove any errors
    # or inconsistencies. This can include removing missing or duplicate values, correcting data types,
    # and standardizing values.
    #-----------------------------------------
    # Hands-on Example
    
    # Rental Properties dataset
    
    # This dataset contains 6080 houses to rent in a country with 12 different features.
    # There could be some outliers, or typos, or wrong type (i.e. expecting a number but you get text),
    # or it is mixed with text and number.
    
    # Prepare and clean the dataset first. 
    # The data is real and came from Kaggle.com
    
    # 1) Check for missing values
    #    (scroll down)
    
    sum(is.na(d)) # No missing values now but we will check this one more time later
    
    # 2) Check for typos
    
    # View the dataset and inspect for irregularities, typos anything unusual.
    # Take notes.
    
    View(d)
    class(d$floor)
    
    table(d$floor)
   
    # a) floor: It has dashes meaning floor #zero. We need to replace them with zeros.
    # b) animal: There is a typo: acept -> accept, not acept -> not accept
    # c) hoa, rentAmount, propertyTax, fireInsurance:
    #    hoa: incluso -> zero, Sem info -> NA
    
    
    # a) Fixing floor column:
    # replace "-" with zero. 

d[ d$floor == "-" , "floor"] = 0
(d$floor)
dim(d$floor)

d$floor <- as.integer(d$floor)
# see if all is properly corrected
summary(d$floor)
str(d$floor)
table(d$floor)

d[ d$floor == 99 , ]

d = d[ -2042 , ]    # delete row number 2042 
d[d$floor == -2042,  ]

# b) Fixing animal column:
# replace "acept" with "accept". 
# First check if there are any other category
table(d$animal)

d[ d$animal == "acept" , "animal"]  = "accept"

d[ d$animal == "not acept" , "animal"]  = "not accept"

# alternatively:
d$animal[d$animal == "acept"] = "accept"
d$animal[d$animal == "not acept"] = "not accept"

# alternatively, in a single line of code:
# use ifelse() function. it works just like Excel if()

d$animal =ifelse( d$animal == "acept", "accept", "not accept")


# c) Fixinf hoa, rentAmount, propertyTax, fireInsurance : supposed to be currency
#        but numerical value preceded by with "R$"
# We need to extract the numerical part out of this mixed text
#  R$500 -> 500
# delete first 2 characters
# however we need to make sure all the data is in this structure: R$ + number

# go to the View and sort by hoa


# Before cleaning R$value, we need to decide what to do with these two values.
# They have to be converted to some numerical value.


# Step-1: Incluso -> R$0

d[ d$hoa == "Incluso", "hoa"] = "R$0"
 str(d$hoa)

View(d)

# Step-2:  Sem info -> NA

d[ d$hoa == "Sem info", "hoa"] = NA


# Step-3: Discard first 2 characters from hoa
# substr(text, start, stop) function picks up part of a text

d$hoa = substr(d$hoa, 3, 15)

hoaNoComma = gsub("," , "" , d$hoa)   # first, gsub function removes the comma

d$hoa = as.numeric(hoaNoComma)  # convert from text to numeric

# final check with summary()

summary(d$hoa)

# do the same thing for the other similar columns
# rentAmount, propertyTax, fireInsurance
# Do it now on your own!

# Here is my plan:
# rentAmount: remove "R$" using substr
# propertyTax: incluso -> "R$0", remove "R$" using substr
# fireInsurance: remove "R$" using substr

# rentAmount: remove "R$" using substr
d$rentAmount = substr(d$rentAmount, 3, 15)

d$rentAmount = as.numeric( gsub("," , "" , d$rentAmount) )  # remove comma and convert from text to numeric

# propertyTax: incluso -> "R$0", remove "R$" using substr
d[ d$propertyTax == "Incluso", "propertyTax"] = "R$0"
d$propertyTax = substr(d$propertyTax, 3, 15)
d$propertyTax = as.numeric( gsub("," , "" , d$propertyTax) )

# fireInsurance: remove "R$" using substr
d$fireInsurance = substr(d$fireInsurance, 3, 15)
d$fireInsurance = as.numeric( gsub("," , "" , d$fireInsurance) )

# 3) Check for missing values

sum(is.na(d))    

# find where they are located

a = lapply(d,  is.na )     # lapply() function applies a given function 
                           # on every column in the dataframe
                           
lapply(a,  sum )          # do the same thing but this time sum()    

# $hoa column: 119 missing value


# Packages are available in R to deal with missing values (search google)

# Solution-1: Delete the row or column with missing values (if too many missing values)
# Solution-2: Impute the missing values (if small amount of missing values)

# imputation using median

medhoa = median(d$hoa, na.rm = TRUE)    # get median without considering NAs

# imputate hoa's missing values using the medhoa # we are going  to find the median without NAs and then we will replaces the NAs with medians

d$hoa[ is.na(d$hoa) ] = medhoa

sum(is.na(d))   # check one more time - ZERO!


# 3) Check for outliers

# Numerical variables need "5 point summary" to see anything unusual

summary(d)

# hoa, tax and rent amount has outliers




d[ d$area > 5000, ] #anything beyond 5000 are outliers

d = d[ d$area < 5000, ]

boxplot(d$area)

plot(d$hoa)  # two outliers greater than a big number such as$150000, possibly typos: Will delete!


d[ d$hoa > 150000 ,  ]        # these two are outliers seen in the plot

d = d[ d$hoa < 150000 ,  ]   # I will take all values less than this

summary(d$hoa)

plot(d$hoa)    # That's better

plot(d$rentAmount)  # anything above 30000 outlier
plot(d$propertyTax) # anything above 200000 outlier
plot(d$fireInsurance) # anything above 600 outlier

d = d[ d$rentAmount < 30000,  ]
d = d[ d$propertyTax < 200000,  ]
d = d[ d$fireInsurance < 600,  ]

plot(d$rentAmount)  
plot(d$propertyTax) 
plot(d$fireInsurance) 

# Data cleaning is complete!!!

# Don't forget to save your cleaned data under a different name
# Don't overwrite the original data

write.csv(d, "RentalPropertiesCleaned.csv", row.names = FALSE) 
# row names false because otherwise it will create an extra ID column

  
# Now use this data for prediction or pattern identification purposes.







