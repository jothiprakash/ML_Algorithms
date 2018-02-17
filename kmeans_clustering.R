setwd("D:/Backup/Term - 2/Machine Learning/Algorithms/")

# Import the dataset
movie_metadata <- read.csv("movie_metadata.csv")
dim(movie_metadata)
str(movie_metadata)
colSums(is.na(movie_metadata))

# Removing all NA values
movie_metadata <- na.omit(movie_metadata)

# Selecting 500 samples
smple <- movie_metadata[sample(nrow(movie_metadata), 500), ]
View(smple)

# Selecting facebook likes for c9, c23 for cluster analysis
smple_short <- smple[, c(9, 23)]
View(smple_short)

smple_matrix <- data.matrix(smple_short)

# Elbow curve
wss <- (nrow(smple_matrix) - 1) * sum(apply(smple_matrix, 2, var))
for(i in 2:15) {
  wss[i] = sum(kmeans(smple_matrix, centers = i)$withinss)
}

plot(1:15,
     wss,
     type = "b",
     xlab = "Number of Clusters",
     ylab = "Within Sum of Squares")

# K-Means clustering
cl <- kmeans(smple_matrix,
             centers = 3, 
             nstart = 25)
plot(smple_matrix,
     col = (cl$cluster + 1),
     main = "K-Means result with 2 clusters",
     pch = 1,
     cex = 1,
     las = 1)
points(cl$cluster,
       col = 12,
       pch = 8,
       cex = 2)
