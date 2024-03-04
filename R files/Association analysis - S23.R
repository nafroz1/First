# Install and load necessary package
# install.packages("arules")
library(arules)

# Example of Market Basket Analysis using the Apriori algorithm

# Reading the dataset
fp.df <- read.csv("datasets/Faceplate.csv")

# Convert dataframe to matrix, excluding the transaction ID column
fp.mat <- as.matrix(fp.df[ , -1])

# Convert binary matrix to transactions item set
fp.trans <- as(fp.mat, "transactions")

# Generate association rules with minimum support and confidence thresholds
rules <- apriori(fp.trans, parameter = list(supp = 0.2, conf = 0.5, target = "rules"))

# Print and inspect the generated rules
print(rules)
inspect(rules)

# Sorting and inspecting rules by confidence
inspect(sort(rules, by="confidence"))

# Example of finding rules with a specific criteria (e.g., support >= 0.2 and confidence > 0.70)
rules.tbl <- inspect(rules)
rules.tbl[rules.tbl$support >= 0.2 & rules.tbl$confidence >= 0.7 , ]

# Example with a different dataset: Charles Book Club
all.books.df <- read.csv("datasets/CharlesBookClub.csv")

# Preprocess data to create binary incidence matrix
incid.books.df <- ifelse(all.books.df[, 8:18] > 0, 1, 0)

# Convert to transactions set format
books.trans <- as(as.matrix(incid.books.df[, -1]), "transactions")

# Generate rules with specified minimum support and confidence
rules <- apriori(books.trans, parameter = list(supp= 200/4000, conf = 0.5, target = "rules"))

# Inspect rules sorted by lift and confidence
inspect(sort(rules, by = "lift"))
inspect(sort(rules, by = "confidence"))

