# Intro to R and objects
5+3

a <- 5
b <- 3

c <- a+b

typeof(a)
str(a)

# Data Types

# Vectors
d <- c(a,b,c)

typeof(d)
str(d)

e <- c(21 , d)

f <- c('h' , e)

typeof(f)
str(f)

e[1] + e[4]

f[2] + f[5]


cars1 <- mtcars
str(cars1)

# convert am to a factor and call it am_factor
cars1$am_factor <- as.factor(cars1$am)
str(cars1$am_factor)


# convert am to a character and call it am_char
cars1$am_char <- as.character(cars1$am)
str(cars1$am_char)


summary(cars1)

hist(cars1$mpg , 
     main = 'MPG Histogram' ,
     xlab = '' ,
     col = '#e1a015')
