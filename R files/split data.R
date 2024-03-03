
# How to split data set into training and validation

d = read.csv("C:/Users/Nahid/Desktop/census.csv")

View(d)


# Divide the dataset randomly into two parts: Training 70% and validation %30. Sometimes, 80 to 20, 60 to 40 (you decide)

set.seed(2023)          # setting the seed is important to generate
                        # the same random sequence in each run

total.rows = dim(d)[1]  # what is the size of the dataset? How many rows? In this case, it is 500 rows.

train.rows <- sample( c(1:total.rows), total.rows*0.7 )  # Generate 350 (70% of 500 rows) random numbers between 1 and 500
train.data <- d[train.rows, ]                            # get the actual training data using row numbers
valid.data <- d[-train.rows, ]                           # get the actual validation data using row numbers

View(train.data)    # Show training partition data
View(valid.data)    # Show validation partition data


dim(d)[2]
dim(d) 
