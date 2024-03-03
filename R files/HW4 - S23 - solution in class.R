# Homework
# The dataset below is from a car dealership's database about their used cars.
# Suppose your target variable in this dataset is "price"

d = read.csv("datasets/usedcars.csv")
View(d)
str(d)


# Answer the following questions.

# Questions

# 1) Examine the relationship between price and mileage. As you notice, mileage is given as
#    a categorical variable. For example, a car with 5 mileage means this car's actual mileage is somewhere
#    between 0-5000. Similarly, a car with 100 mileage means this car's mileage is between 95000 and 100000.
#    First, convert mileage to a factor variable with ordered levels by running the following code. Then, create
#    a boxplot for price and mileage. Which mileage categories can be combined to reduce the number of mileage categories?


d$mileage = factor(d$mileage, levels = c("5","10","15","20","25","30","35","40","45","50","55","60","65","70","75","80","85","90","95","100","100+"))

boxplot(d$price~d$mileage)


# 2) Based on your answer to Question1, create a new variable called newMileage which will hold
#    combined categories. The new category labels should be like: "5:10", "15:25", etc...
#    HINT: Use %in% operator as shown in class.

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


# 3) Convert newMileage variable to factor with ordered levels.
#    Hint: Write a similar code that is given in Question1.

d$newMileage = factor(d$newMileage, levels = c("5:10","15:25","30:50","55:90","95:100+"))



# 4) Create a boxplot showing price and newMileage. Comment on the boxplot.

boxplot(d$price~d$newMileage)

# 5) Create a table using price and newMileage. Comment on the resulting table.

table(d$price,d$newMileage)




