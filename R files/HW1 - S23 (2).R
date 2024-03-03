# Homework

# Data Cleaning

# Read the LoanDefault dataset using the following code. The dataset
# is located on Canvas datasets folder.


d <- read_csv("C:/Users/Nahid/Desktop/LoanDefault.csv")
View(d)

# This dataset is about bank customers and their loan related records.

# The dataset needs cleaning. Complete the following cleaning tasks.

# Questions:

# 1) Gender column shows the gender of the account owner (f, or m), 
#    or a joint account. Replace "Sex Not Available" category with NAs.

d$Gender=gsub("Sex Not Available","NA",d$Gender)
table(d$Gender)

# 2) total_units column has text data but it should be converted to
#    numeric such as 1, 2, 3, and 4. 

table(d$total_units)

d$total_units=substr(d$total_units,0,1)

# 3) The values of "submission_of_application" column needs to be replaced with more
#    meaningful values. So, replace "to_inst" with "submitted", and "not_inst" with
#    "not_submitted". Use ifelse() function.

table(d$submission_of_application)

d$submission_of_application = ifelse(d$submission_of_application == "to_inst","submitted","not_submitted")


# 4) Region column has inconsistent use of capitalization. Correct it.

table(d$Region)
d$Region = gsub("central","Central",d$Region)
d$Region = gsub("south", "South", d$Region)

# 5) Age column needs to be converted to numerical values. Do this as shown below:
#    <25 becomes 1
#    25-34 becomes 2
#    ..........
#    65-74 becomes 7
#    >74 becomes 8


d$age=gsub("<25","1",d$age)
d$age=gsub("25-34","2",d$age)
d$age=gsub("35-44","3",d$age)
d$age=gsub("45-54","4",d$age)
d$age=gsub("55-64","5",d$age)
d$age=gsub("65-74","6",d$age)
d$age=gsub(">74","7",d$age)
table(d$age)



# 6) Security_type column has a typo. Correct it.

table(d$Security_Type)

d$Security_Type=gsub("Indriect","indirect",d$Security_Type)

# 7) Imputate NAs in property_value with median property_value.

medpv=median(d$property_value,na.rm=TRUE)

sum(is.na(d$property_value))

d$property_value[ is.na(d$property_value) ] = medpv

d$property_value <- ifelse(is.na(d$property_value) ,"medpv",d$property_value)


# 8) Save your cleaned dataset as "LoanDefaultClean1.csv"

write.csv(d,"practice1.csv",row.names = FALSE)

