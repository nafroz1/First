# Data Preparation and Visualization for Used Car Prices

d = read.csv("datasets/usedcars.csv")
View(d)
str(d)

# Step 1: Examine the relationship between price and mileage
d$mileage = factor(d$mileage, levels = c("5","10","15","20","25","30","35","40","45","50","55","60","65","70","75","80","85","90","95","100","100+"))
boxplot(d$price~d$mileage)

# Step 2: Create a new variable for combined mileage categories
b = d$mileage %in% c(5,10)
d$newMileage = ifelse(b, "5:10", d$mileage)

b1 = d$mileage %in% c(15,20,25)
d$newMileage = ifelse(b1, "15:25", d$newMileage)

b2 = d$mileage %in% c(30,35,40,45,50)
d$newMileage = ifelse(b2, "30:50", d$newMileage)

b3 = d$mileage %in% c(55,60,65,70,75,80,85,90)
d$newMileage = ifelse(b3, "55:90", d$newMileage)

b4 = d$mileage %in% c(95,100,"100+")
d$newMileage = ifelse(b4, "95:100+", d$newMileage)

table(d$newMileage)

# Step 3: Convert newMileage variable to factor with ordered levels
d$newMileage = factor(d$newMileage, levels = c("5:10","15:25","30:50","55:90","95:100+"))

# Step 4: Create a boxplot for price and newMileage
boxplot(d$price~d$newMileage)

# Step 5: Create a table for price and newMileage
table(d$price,d$newMileage)
