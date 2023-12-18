#install.packages(c("factoextra", "dbscan", "fpc"))
library(dplyr)
library(psych)
library(cluster)
library(factoextra)
library(dbscan)
library(fpc)

setwd("C:/Users/ACER/Desktop/Desktop/IIT_Stuff/CS 422/myHws/HW8")
# Data that we're going to use
df <- read.table("file19.txt", skip = 20, header = TRUE)
# Show part of data 
head(df)
# Find correlation between the attribute
cor(df[,-1])
# Drop c,p,m since they are strongly correlated with C,P,M - we don't need both
df$c = NULL
df$p = NULL
df$m = NULL

summary(df)

#Standardize the data
df.scale <- scale(df[,-1])
summary(df.scale)
?scale()

# In this case standardization might not be necessary since the difference between min and max is not too big.
# However, for the sake of making the work easier for the computer (algorithm is easier to use), I will scale.
write.csv(df,"file19.csv")


# B) Clustering
# i) Determine how many clusters are needed
?fviz_nbclust()
fviz_nbclust(df.scale[, -1], kmeans, method = "wss")
fviz_nbclust(df.scale[, -1], kmeans, method = "silhouette")

? fviz_cluster()
clst <- kmeans(df.scale[, -1], centers = 8, nstart = 25)
fviz_cluster(clst, df.scale[, -1], main = "Cluster")

# I chose to have 8 clusters because from the wss graph, we can notice that after 8, there isn't a big change 
# in the numbers. Perhaps, ideally we would use 10.

# (iii) How many observations are in each cluster?


clst$size
# Number of observations in each cluster: 1, 8, 7, 11, 6, 8, 13, 12


# (iv) What is the total SSE of the clusters?
# totss = tot.withinss + betweenss
clst$totss
clst$tot.withinss
# Total SSE of the clusters = 260
# Total within-clusters = 17.57414 

# (v) What is the SSE of each cluster?
clst$withinss
#  0.000000e+00 5.908626e-01 3.882675e-31 1.841650e+00 5.083201e+00 1.471193e+00 1.869983e+00 6.717249e+00

df[which(clst$cluster == 1),]
df[which(clst$cluster == 2),]
df[which(clst$cluster == 3),]
df[which(clst$cluster == 4),]
df[which(clst$cluster == 5),]
df[which(clst$cluster == 6),]
df[which(clst$cluster == 7),]
df[which(clst$cluster == 8),]

# Cluster 1 makes sense since there is only 1 member in it - Armadillo.
# Cluster 2 makes sense. The attributes remain the same in all observations. Moreover, all of the animals are rodents
# in this cluster
# In cluster 3 all attributes remain the same in the observations. They are all from cervidae family.
# Cluster 4 makes sense. All attributes except for attribute P remain the same in the observations.
# The animals are rodents.
# Cluster 5 contains attributes that remain more or less the same in the observations. The cluster definitely makes sense
# when viewed from the perspective that all of these sea animals look similar to each other (especially their tusks).
# Even though in cluster 6 the attributes are the same for the most part in the observations, the cluster doesn't make
# sense to me when it comes to finding similarities between the animals in it.
# Cluster 7 makes sense. Attributes remain the same for the most part. The animals in it are similar. 
# They have many cat characteristics. 
# Cluster 8 doesn't make sense to me.