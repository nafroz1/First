# Data Cleaning

d = read.csv("LoanDefault.csv")
View(d)

# Handling Missing Data

d[ d$Gender == "Sex Not Available", "Gender"] = NA

table(d$Gender)

# Converting text data to numeric

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


# Value Replacement

d$submission_of_application =ifelse (d$submission_of_application == "to_inst" , "submitted" , "not_submitted")

table(d$submission_of_application)

# 4) Correct inconsistent capitalization.

d$Region = gsub("south" , "South" , d$Region)


table(d$Region)

d$Region = gsub("central" , "Central" , d$Region)

#ALTERNATE

d$Region[d$Region == "south"] = "South"
d$Region[d$Region == "central"] = "Central"


# 5) Converting Age Ranges to Numerical Categories:
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
# Correct a typo.

table(d$Security_Type)

d$Security_Type =ifelse (d$Security_Type == "direct" , "Direct" , "Indirect")

#ALTERNATE

d$Security_Type [d$Security_Type=="Indriect"]="Indirect"

# 7) Imputate (Missing Value) NAs in property_value with median property_value.

median(d$property_value)

d$property_value[ is.na(d$property_value) ] = medproperty_value

medproperty_value = median(d$property_value, na.rm = TRUE)

d$property_value[ is.na(d$property_value) ] = medproperty_value


# Saving the cleaned dataset as "LoanDefaultClean1.csv"

write.csv(d, "LoanDefaultClean1.csv", row.names = FALSE) 

ddd= read.csv("LoanDefaultClean1.csv")
View(ddd)
