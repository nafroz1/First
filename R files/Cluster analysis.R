# Cluster Analysis

# Read dataset of public utilities
utilities.df <- read.csv("C:/Users/Nahid/Desktop/Utilities.csv")

# Normalize variables excluding non-numeric columns
utilities.df.norm <- sapply(utilities.df[-1], scale) # Apply normalization
row.names(utilities.df.norm) <- utilities.df$Company # Set row names

# Compute Euclidean distance using sales and fuel cost variables
distSaleFuel <- dist(utilities.df.norm[, c("Sales","Fuel_Cost")], method = "euclidean")

# Plot to guess clusters
plot(utilities.df.norm["Fuel_Cost"] ~ utilities.df.norm["Sales"])
text(utilities.df.norm["Fuel_Cost"] ~ utilities.df.norm["Sales"], labels=utilities.df$Company, cex=0.7, font=2)

# Hierarchical clustering using single linkage strategy
hc1 <- hclust(distSaleFuel, method = "single")
plot(hc1, hang = -1, ann = FALSE)

# Use all variables for a comprehensive clustering
dist.all <- dist(utilities.df.norm, method = "euclidean")

# Apply different clustering methods
hc2 <- hclust(dist.all, method = "complete")
plot(hc2, hang = -1, ann = FALSE)

# Determine cluster membership for a specified number of clusters
memb <- cutree(hc2, k = 4) # For 4 clusters
sort(memb)

# Heatmap to visualize cluster characteristics
row.names(utilities.df.norm) <- paste(memb, ": ", row.names(utilities.df.norm), sep = "")
heatmap(as.matrix(utilities.df.norm), Colv = NA, hclustfun = hclust, col = rev(colorRampPalette(c("white", "pink"))(100)))

# K-Means Clustering
set.seed(2) # For reproducibility
km <- kmeans(utilities.df.norm, 6, nstart=10) # 6 clusters, 10 iterations
print(km$cluster) # Show cluster membership
