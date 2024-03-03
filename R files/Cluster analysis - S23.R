# Cluster Analysis

# The goal in Cluster Analysis is to segment the data into a set of 
# homogeneous clusters of records for the purpose of generating insight.
# One popular use of cluster analysis in marketing is for market segmentation:
# Customers are segmented based on demographic and transaction history,
# and a marketing strategy is tailored for each segment.

# Some companies grouped neighborhoods into clusters using various measures
# of consumer expenditure and demographics. Examining the clusters enabled them
# to come up with interesting names, such as "Bohemian Mix," "Furs and Station
# Wagons," and "Money and Brains," for the groups that captured the dominant
# lifestyles. Knowledge of lifestyles can be used to estimate the potential 
# demand for products (such as sports utility vehicles) and services
# (such as pleasure cruises).

# Typically, the basic data used to form clusters are a table of measurements
# on several variables, where each column represents a variable and a row
# represents a record. Our goal is to form groups of records so that similar
# records are in the same group.

# We can think of cluster analysis as an algorithm that
# measures the distance between records, and according to these distances
# (here, two-dimensional distances), clusters are formed.

# Two general types of clustering algorithms for a dataset of n records are
# a) hierarchical and b) non-hierarchical clustering:

# Hierarchical: Begins with n clusters and sequentially merge similar clusters
# until a single cluster is obtained. In another variant, algorithm begins 
# in the opposite direction, starting with one cluster that includes all records.
# Hierarchical methods are especially useful when the goal is to arrange
# the clusters into a natural hierarchy.

# Non-hierarchical clustering: Such as "k-means" method. Using a prespecified
# number of clusters, the method assigns records to each cluster. These methods
# are generally less computationally intensive and are therefore preferred with
# very large datasets.

# We consider two types of distances: distance between two records and,
# distance between two clusters.
# We will use "Euclidean" distance like we used in Knn approach.

# Also, distance calculations are influenced by the scale of the variables.
# So, the variables are usually normalized first.

# Example-1: Public utilities.

# Data on 22 public utility organizations in the United States are collected. 
# We are interested in forming groups of similar utilities. 
# The clustering will be based on the eight measurements on each utility.

# We will save a considerable amount of time and effort if we could cluster
# similar types of utilities and work on just one "typical" utility 
# in each cluster

#Investigate a nameless dataset for patterns, for clusters.
#SImilarity is based on distance
#Big numbers or too small numbers, change everything, so get rid of it. Normalize it. 
#Scale means take each number in data set and divide by column std dev. Everything in one scale, that is same range of numbers. 

utilities.df <- read.csv("C:/Users/Nahid/Desktop/Utilities.csv")
View(utilities.df)

# Prepare the dataset

# Normalize all variables (except company name)
# using scale() function. Normalization : z = (x - mean)/stdev
# scale(x, center = TRUE, scale = TRUE)

# for example, normalize Fixed_charge variable
scale(utilities.df$Fixed_charge, scale = TRUE)

# Need to apply on all numeric variables
# sapply(dataframe, function) applies function to all columns 

# exclude company name -1

utilities.df.norm <- sapply(utilities.df[-1], scale)      #actual normalization step is here.X means return as matrix.
class(utilities.df.norm) #dataset can be a mix of numerical , character etc. but matrix has to be just one type.

# This is a matrix not a dataframe, be careful, $ notation does not work with it
# use [row,column] notation to access entries

# For a better data presentation,
# set row names to the name of the utilities

row.names(utilities.df.norm) <- utilities.df$Company 
View(utilities.df.norm)

# As an example, compute Euclidean distance between records (i.e.utility companies) 
# using only sales and fuel cost variables

distSaleFuel <- dist(utilities.df.norm[ , c("Sales","Fuel_Cost")], method = "euclidean")
distSaleFuel

# This is a matrix of 22 X 22 showing distances between all companies (upper triangle not showing)

# Procedure
# 1. Start with n clusters (each record = cluster).
# 2. The two closest records are merged into one cluster.
# 3. At every step, the two clusters with the smallest distance are
#    merged. This means that either single records are added 
#    to existing clusters or two existing clusters are combined.
#    This is called "agglomeration"

# Let's plot the normalized companies and try to guess clusters

plot(utilities.df.norm[,"Fuel_Cost"] ~ utilities.df.norm[,"Sales"])
text(utilities.df.norm[,"Fuel_Cost"] ~ utilities.df.norm[,"Sales"], labels=utilities.df$Company,cex=0.7, font=2)


# Now apply the hierarchical clustering procedure using hclust()
# method is "single", meaning "single linkage" strategy
# In single linkage clustering, the distance measure that we use
# is the minimum distance (the distance between the nearest pair
# of records in the two clusters, one record in each cluster).
# This method results in clusters that have elongated 
# sausage-like shapes when visualized as objects in space.

hc1 <- hclust(distSaleFuel, method = "single")
plot(hc1, hang = -1, ann = FALSE)

# This is a dendrogram.
# A dendrogram is a treelike diagram that summarizes the process
# of clustering. On the x-axis are the records. Similar records
# are joined by lines whose vertical length reflects the distance
# between the records.

# By choosing a cutoff distance on the y-axis, a set of clusters
# is created. Visually, this means drawing a horizontal line on a
# dendrogram.

# Records with connections below the horizontal line (that is, 
# their distance is smaller than the cutoff distance) belong to 
# the same cluster. For example, setting the cutoff distance to
# 0.7 on the dendrogram results in five clusters.

# Other strategies:
# 1) In complete linkage clustering, the distance between two clusters
#    is the maximum distance (between the farthest pair of records).
# 2) In average linkage clustering, the procedure uses the average
#    distance between clusters (between all possible pairs of records).
# 3) In centroid linkage clustering, clusters are represented
#    by their mean values for each variable, and distances are based on
#    these group means.


# Now let's use all 8 variables

dist.all <- dist(utilities.df.norm, method = "euclidean")

# Apply different methods to built clusters

hc1 <- hclust(dist.all, method = "single")
plot(hc1, hang = -1, ann = FALSE)

hc2 <- hclust(dist.all, method = "complete")
plot(hc2, hang = -1, ann = FALSE)

hc3 <- hclust(dist.all, method = "average")
plot(hc3, hang = -1, ann = FALSE)

hc4 <- hclust(dist.all, method = "ward.D")
plot(hc4, hang = -1, ann = FALSE)

hc5 <- hclust(dist.all, method = "median")
plot(hc5, hang = -1, ann = FALSE)

hc6 <- hclust(dist.all, method = "centroid")
plot(hc6, hang = -1, ann = FALSE)


# Show me which companies belong to which cluster
# if I want to have only 4 clusters
# use cutree() function and sort the resulting list

memb <- cutree(hc2, k = 4)
sort(memb)

# A heatmap can show what each cluster is characterized by.

# set labels as cluster membership and utility name

row.names(utilities.df.norm) <- paste(memb, ": ", row.names(utilities.df.norm), sep = "")

# plot heatmap 
# rev() reverses the color mapping to large = dark
heatmap(as.matrix(utilities.df.norm), Colv = NA, hclustfun = hclust, 
        col=rev(paste("gray",1:99,sep="")))

heatmap(as.matrix(utilities.df.norm), Colv = NA, hclustfun = hclust, 
        col = rev(colorRampPalette(c("white", "pink"))(100)))       #JUST FOR FUN LEARNING


# We see that 
# cluster 2 is characterized by utilities with a high percent of nuclear power,
# cluster 1 is characterized by high fixed charge, rate of return (RoR) and Sales;
# cluster 3 has high fuel costs & load factor.



# ------------- K-MEANS Method ---------------

# In k-means method, we tell the algorithm how many clusters
# we want to see. 
# K-means uses a different approach than hierarchical and based on
# creating k centers (centroids) and assigning all the data to the closest center.
# We provide the k value, which is the number of clusters desired.

# In this example, we want to see 6 clusters and 10 iterations to be completed.
# Each iteration generates a slightly different solution and
# the best solution (out of 10) is presented.

# run kmeans algorithm 
set.seed(2)
km <- kmeans(utilities.df.norm, 6, nstart=10)

# show cluster membership
km$cluster



