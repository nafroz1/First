# Association Rules

# This is an "unsupervised" method, meaning there is no target 
# outcome to be predicted. The aim in this method is to discover
# which groups of products tend to be purchased together. In marketing,
# it is called "market basket analysis” or “affinity analysis”.
# When discovered, these items can then be displayed together, 
# offered in post-transaction coupons, recommended in online shopping,
# store layouts and item placement, cross-selling, cross promotions, 
# catalog design, and identifying customer segments based on
# buying patterns.

# We will use a procedure called "apriori" to generate association rules.
# The rules look like this:

# IF A is bought then B is bought as well (with a good chance.)

# Example1:
# When we examine all the purchase transactions in a database of 1000 transactions,
# we might be seeing that 100 people bought product A, and out of this 100 people,
# 40 people also bought B. 

# Support: How often an item (or sets of items together) bought in the entire
# database. It is simply a count or percentage.
# In this example: 

#      Support for A (People who bought A) : 100  (or 100/1000 = 10%)
#      Support for A & B: (People who bought A & B) : 40 (or 40/1000 = 4%)

# Association rules are given in this form: IF A then B

# A and B are a set of products. A is called "antecedent" which are
# the products that are considered first. B is called "consequent" which
# are the products considered only if A happens first.

# Example2:
# You are planning to buy a laptop, Amazon shows you
# products that go with a laptop such as a mouse and a USB drive.

# However, it does not show you a laptop when you are buying a USB drive.
# Buying a USB first (antecedent) does not lead to buying a laptop (consequent).
# How do we know which products should come first? (See below - "apriori" algorithm)

# For example:
# Support for laptop: 100
# Support for USB drive: 10000
# Support for laptop and USB: 60 (cannot be greater than 100)

# Confidence: It is a measure to express the degree of uncertainty about
# the association rule. It can be considered as a "conditional probability".

#      P(B | A) = What are the chances of purchasing B 
#                 when we knew A is already purchased.

#      Confidence = P(B | A) = Support(A & B)/Support(A)
#                            = Support(antecedent & consequent)/Support(antecedent)

# For example:
# Support for laptop: 100
# Support for USB drive: 10000
# Support for laptop and USB: 60

# Confidence = P( USB | Laptop ) = 60 / 100 = 0.6

# Confidence = P( Laptop | USB ) = 60 / 10000 = 0.006

# RULE1: If a laptop is bought then USB drive is also bought
# Confidence of this rule: P( USB | Laptop) = 60 / 100 = 60%

# RULE2: If a USB drive is bought then a laptop is also bought
# Confidence of this rule: P( Laptop | USB) = 60 / 10000 = 0.6%

# Apriori algorithm goes through the entire transactions database
# and finds the rules with higher confidence. Any rule with a confidence level
# lower than a threshold are not considered or reported.

# Minimum support and minimum confidence

# Example3:
# Out of 100 people who bought A, 5 people bought item C as well, and 3 people
# bought item D also. Since these numbers (5 and 3) are relatively small, we consider
# them insignificant and we won't create rules for them. 

# Therefore we set "apriori" (i.e. ahead of time, or by theory) the lowest number for support
# and lowest number for confidence. This way, computer will not run
# to produce too many rules. It will just skip insignificant rules.

# Also, consider the following possibilities: A, B, C, D, (A+B), (A+C), (A+D), (B+C), (B+D),..(A+B+C),...
# Apriori algorithm will start counting numbers of single items, then pairs, then three products, etc.
# Some combinations are very frequent, and some are less frequent. When a count
# is less than the minimum support, computer will not pursue it further.

# For example, if total count of C is less than the minimum support, then computer
# will not even consider any combinations that include C, such as A & C, or C & A, or C & D.
# This is because count of C is the highest number these combinations can give.

# Example4:
# A store that sells accessories for cellular phones runs a promotion on phone cases.
# Customers who purchase multiple cases from a choice of six different colors
# get a discount. The store managers would like to know what colors of
# cases customers are likely to purchase together.

# Read this small dataset below:

fp.df <- read.csv("datasets/Faceplate.csv")
View(fp.df)

# we will use "arules" package for association analysis.
# install if you don't have it.
# install.packages("arules")

library(arules)

# This package requires data to be given as "transaction item sets"
# with each row representing a list of items purchased in a single transaction
# For example:
# The original dataframe could be given below in binary format:

# red white blue
# 1     0    0
# 1     0    1
# 0     1    1

# However, for the arules package to work, we need to re-organize it 
# as a "set" notation seen below:

# {red}
# {red, blue}
# {white, blue}

# First, we remove the first column (transaction ID) and 
# convert the dataframe to matrix

fp.mat <- as.matrix(fp.df[ , -1])
fp.mat

# Now we convert this binary matrix to a "transactions" item set
# using as() function 

fp.trans <- as(fp.mat, "transactions")

# See what's inside

inspect(fp.trans)

# transactions set is ready to go.

# Generate the rules!

# When running apriori() function, we need to include the minimum support, 
# minimum confidence, and target as arguments using parameter = list()
# Assume minimum support 20%, minimum confidence is 50% and resulting output
# should write the "rules".

# IF A THEN B  -> P(A & B) >=  0.20 of all transactions
# P(B | A) >= 0.50

rules <- apriori(fp.trans, parameter = list(supp = 0.2, conf = 0.5, target = "rules"))

# How many rules are generated?
print(rules)

# Let's take a loot at them
inspect(rules)

# For example:
# Rule4: If Orange then White
# Support(Rule4) = 
nrow(fp.df[fp.df$Orange == 1 & fp.df$White == 1, ])

# Rule7: If Blue then Red
# Confidence(Rule9) = P(White | Blue) = 4/6
nrow(fp.df[fp.df$Blue == 1 & fp.df$White == 1, ])/nrow(fp.df[fp.df$Blue == 1, ])


# Selecting Strong Rules

# One way of selecting strong rules is to sort the rules
# by their confidence (probabilities to happen) 

inspect(sort(rules, by="confidence"))

# Another way to do this is to use the "lift" ratio.
# The lift value is a measure of importance of a rule.

# Lift ratio = confidence of a rule / confidence if antecedent and consequent are unrelated

# For two products, A and B,  if the rule is A then B: 
# lift = P(B|A) / P(B)

# If B is independent of A, i.e. no association/unrelated, P(B|A) becomes P(B) and
# lift ratio becomes 1.
# So, any rule with a lift ratio greater than 1 could be important and useful.

# Independent events: Flip a coin two times. Seeing that the first one is Tail,
# what are the chances to get a tail again?
# 50% because previous event has no effect on the next event.

# RULE: {red, white} => {green}
inspect(rules)

# lift = confidence of the rule/ benchmark (i.e. no association) confidence
# lift = P(B|A) / P(B)
# lift = P( {green} | {red, white} ) / P({green})
# lift = 50% / 20% = 2.5

# Now inspect the first six rules, sorted by their lift

inspect(head(sort(rules, by = "lift"), n = 6))

# How to read a rule?

# If orange is purchased, then with confidence of 100%, white will also be
# purchased. This rule has a lift ratio of 1.43.

# If red and white are purchased, then with confidence of 50%, green will also be
# purchased. This rule has a lift ratio of 2.5.

# The rule suggests that purchase of green item is 2.5 times as likely
# when items red and white are purchased than if the green item
# was not associated with the items {red and white}.

# Interpreting these three measures:

# The support for the rule indicates its impact in terms of overall size.
# How many transactions are affected? 

# The lift ratio indicates how efficient the rule is in finding consequent,
# compared to random selection.

# The confidence tells us at what rate consequent will be found, and is useful
# in determining the business or operational usefulness of a rule.

# Listing rules that comply with a criteria, such as support >= 0.2
# and confidence > 0.70

rules.tbl <- inspect(rules)
rules.tbl[rules.tbl$support >= 0.2 & rules.tbl$confidence >= 0.7 , ]

# The more records (the rule is based on), the more solid the conclusion.

# Consider rules from the top-down in terms of business or operational applicability,
# and not consider more than what can reasonably be incorporated in a human
# decision-making process. Rules should make sense.


# Example-2

# Finding Rules for Similar Book Purchases
# A bookstore would like to know which types of books are sold
# together. Purchase transactions database is provided.
# The database includes 4000 transactions, and there are 
# 11 different types of books.

# read the dataset

all.books.df <- read.csv("datasets/CharlesBookClub.csv")
View(all.books.df)

# Only columns 8 through 18 are relevant, the rest is who purchased when.
# Also, the values indicate number of books purchased.
# We don't need this. 
# What we want is: If purchased 1, otherwise 0.
# This is called "incidence matrix". if happened -> 1, if not -> 0.

# Create a binary incidence dataframe
count.books.df <- all.books.df[, 8:18]
incid.books.df <- ifelse(count.books.df > 0, 1, 0)
View(incid.books.df)

# from dataframe to matrix. Also drop children's books assuming
# we focus on youths and adults in this analysis

incid.books.mat <- as.matrix(incid.books.df[, -1])

#  convert the binary incidence matrix into a transactions set format
books.trans <- as(incid.books.mat, "transactions")

# see what it looks like
inspect(books.trans)

# plot a histogram of the data
itemFrequencyPlot(books.trans)

# Run apriori function with
# minimum support 200 out of total 4000 purchase records
# minimum confidence 50%.
# Any rules that don't satisfy these conditions will be discarded

rules <- apriori(books.trans, parameter = list(supp= 200/4000, conf = 0.5, target = "rules"))

print(rules)
# inspect rules sorted by lift ratio
inspect(sort(rules, by = "lift"))

# inspect rules sorted by confidence ratio
inspect(sort(rules, by = "confidence"))

# write the first rule in a sentence (sorted by lift):

# If DoItYBks and GeogBks are purchased, then with confidence of 54%,
# YouthBks will also be purchased. This rule has a lift ratio of 2.26.

# The rule suggests that purchase of YouthBks is 2.26 times as likely
# when DoItYBks and GeogBks are purchased than if the YouthBks 
# was not associated with these books {DoItYBks and GeogBks}.

# The rule {DoItYBks, GeogBks, YouthBks} appeared 218 times in the database.

# Practical applications: Put do-it-yourself, geography and youthbooks
# on the same shelf so that customers conveniently pick them up.
#  
# On a website, if someone adds do-it-yourself and geography books
# in the cart, show an ad that lists best selling youthbooks.

