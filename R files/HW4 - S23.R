 # Homework
# The dataset below is from a car dealership's database about their used cars.
# Suppose your target variable in this dataset is "price"

d = read.csv("C:/Users/Nahid/Desktop/usedcars.csv")
View(d)
str(d)

summary(d$mileage)


# Answer the following questions.

# Questions

# 1) Examine the relationship between price and mileage. As you notice, mileage is given as
#    a categorical variable. For example, a car with 5 mileage means this car's actual mileage is somewhere
#    between 0-5000. Similarly, a car with 100 mileage means this car's mileage is between 95000 and 100000.
#    First, convert mileage to a factor variable with ordered levels by running the following code. Then, create
#    a boxplot for price and mileage. Which mileage categories can be combined to reduce the number of mileage categories?


d$mileage = factor(d$mileage, levels = c("5","10","15","20","25","30","35","40","45","50","55","60","65","70","75","80","85","90","95","100","100+"))

boxplot(d$price ~ d$mileage)



#Based on the boxplot, the mileage categories that can be combined to reduce the number of categories are 5:10, 15 : 20, 25: 30, 35 :40, 45 and 50, 55 and 60, 65 and 70, 75 and 80, 85 and 90, and 95 and 100+.



# 2) Based on your answer to Question1, create a new variable called newMileage which will hold
#    combined categories. The new category labels should be like: "5:10", "15:25", etc...
#    HINT: Use %in% operator as shown in class.

# Create new variable with combined categories



b= d$mileage%in% c(5)

d$newMileage = ifelse(b, "A", d$mileage)

#ALTERNATIVELY 


#b= d$mileage%in% c("5":"10") ------ mileage is categorical 

#d$newMileage = ifelse(b, "5:10", d$mileage)


##1b= d$mileage%in% c("10";"15") ------ mileage is categorical 

#d$newMileage = ifelse(b1, "5:10", d$mileage)



b= d$mileage%in% c(10:15)

d$newMileage = ifelse(b, "B", d$newMileage)


b= d$mileage%in% c(20:50)

d$newMileage = ifelse(b, "C", d$newMileage)


b= d$mileage%in% c(55:80,90)

d$newMileage = ifelse(b, "D", d$newMileage)


b= d$mileage%in% c(85,95,100,"100+")

d$newMileage = ifelse(b, "E", d$newMileage)

table(d$mileage)



# 3) Convert newMileage variable to factor with ordered levels.
#    Hint: Write a similar code that is given in Question1.

d$newMileage = factor(d$newMileage,levels = c("A","B","C","D","E"))
str(d$newMileage)




# 4) Create a boxplot showing price and newMileage. Comment on the boxplot.


boxplot(d$price~d$newMileage)

#There is an inverse relationship between price and mileage
#When the mileage increases, the avg value of the car will decrease

# 5) Create a table using price and newMileage. Comment on the resulting table.


t=table(d$price,d$newMileage)
View(t)

#Highest frequency in C category = 1433
