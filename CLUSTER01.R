########################
# CLUSTER ANALYSIS 01 
########################

# prepare data: create vectors 
# and join as columns in a data frame

x <- c(0, 7, 1, 4)
y <- c(4, 0, 2, 2)
dat <- cbind.data.frame(x, y)
dat

# scale data: subtract mean and divide by sd
scale(dat)

# create distance matrix, 
# euclidean distance by default
dist(scale(dat))
?dist

# change to Manhattan distance
# and save the distance matrix

D <- dist(scale(dat), method = "manhattan")
D

# realise hierachical cluster analysis,
# complete linkage by default
hc <- hclust(D, method = "complete")
hc

# plot a dendrogram
plot(hc, main = "Complete linkage method")

# dendrogram and 2 clusters
# add rectangle for every cluster
plot(hc, main = "Complete linkage method")
rect.hclust(hc, k = 2, border = "red")

# explore hclust object
hc$height
hc$order
hc$labels

