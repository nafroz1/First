# Homework

# Data Cleaning

# Read the LoanDefault dataset using the following code. The dataset
# is located on Canvas datasets folder.

d = read.csv("LoanDefault.csv")
View(d)

# This dataset is about bank customers and their loan related records.

# The dataset needs cleaning. Complete the following cleaning tasks.

# Questions:

# 1) Gender column shows the gender of the account owner (f, or m), 
#    or a joint account. Replace "Sex Not Available" category with NAs.

d[ d$Gender == "Sex Not Available", "Gender"] = NA

table(d$Gender)

# 2) total_units column has text data but it should be converted to
#    numeric such as 1, 2, 3, and 4. 
summary(d$total_units)

d$total_units[d$total_units == "1U"]=1
d$total_units[d$total_units == "2U"]=2
d$total_units[d$total_units == "3U"]=3
d$total_units[d$total_units == "4U"]=4

d$total_units = substr(d$total_units, 0, 1) #alternate
d$total_units = substr(d$total_units, 1, 1)
table(d$total_units)

d$total_units=as.numeric((d$total_units))#alternate
d$total_units=as.numeric(gsub("U","",d$total_units)) #alternate


# 3) The values of "submission_of_application" column needs to be replaced with more
#    meaningful values. So, replace "to_inst" with "submitted", and "not_inst" with
#    "not_submitted". Use ifelse() function.

d$submission_of_application =ifelse (d$submission_of_application == "to_inst" , "submitted" , "not_submitted")

table(d$submission_of_application)

# 4) Region column has inconsistent use of capitalization. Correct it.

d$Region = gsub("south" , "South" , d$Region)


table(d$Region)

d$Region = gsub("central" , "Central" , d$Region)

#ALTERNATE

d$Region[d$Region == "south"] = "South"
d$Region[d$Region == "central"] = "Central"


# 5) Age column needs to be converted to numerical values. Do this as shown below:
#    <25 becomes 1
#    25-34 becomes 2
#    ..........
#    65-74 becomes 7
#    >74 becomes 8

table(d$age)

d$age = gsub("<25" , 1 , d$age)

d$age = gsub("25-34" , 2 , d$age)

d$age = gsub("35-44" , 3 , d$age)

d$age = gsub("45-54" , 4 , d$age)

d$age = gsub("55-64" , 5 , d$age)

d$age = gsub("65-74" , 6 , d$age)

d$age = gsub(">74" , 7 , d$age)

#ALTERNATE

d[d$age =="<25", "age"] =1
.
.
.
#--COPY PASTE---

# 6) Security_type column has a typo. Correct it.

table(d$Security_Type)

d$Security_Type =ifelse (d$Security_Type == "direct" , "Direct" , "Indirect")

#ALTERNATE

d$Security_Type [d$Security_Type=="Indriect"]="Indirect"

# 7) Imputate NAs in property_value with median property_value.

median(d$property_value)

d$property_value[ is.na(d$property_value) ] = medproperty_value

medproperty_value = median(d$property_value, na.rm = TRUE)

d$property_value[ is.na(d$property_value) ] = medproperty_value


# 8) Save your cleaned dataset as "LoanDefaultClean1.csv"


write.csv(d, "LoanDefaultClean1.csv", row.names = FALSE) 

ddd= read.csv("LoanDefaultClean1.csv")
View(ddd)
