
#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&
#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&
# Week 4 - Supervised classification (I)
#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&
#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&

##################################################################################################################
# Open R or R-studio.
##################################################################################################################

##################################################################################################################
# Install and load ISLR package

install.packages("ISLR")
library("ISLR")

##################################################################################################################
# The NCI60 data set

?NCI60

# Load the data set into memory

data(NCI60)

# Define the data matrix

X.NCI60 <- NCI60$data

# Sample size and dimension of the NCI60 data set

n.NCI60 <- nrow(X.NCI60)
n.NCI60
p.NCI60 <- ncol(X.NCI60)
p.NCI60

##################################################################################################################
# Remember: PCA for the NCI60 data set

PCS.NCI60 <- prcomp(X.NCI60)

##################################################################################################################
# Make a plot of the first two PCs

plot(PCS.NCI60$x[,1:2],pch=20,col="deepskyblue4")

# This plot suggests that there migth be some heterogeneity, i.e., at least two clusters

# Remember that around 17 (or 21) PCs where needed to explain almost the 70% of the total variability of the dataset
# Thus, the number of clusters might be more than 2

##################################################################################################################
##################################################################################################################
# Partitional clustering analysis for the NCI60 data set
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Perform k-means clustering for the NCI60 data set
##################################################################################################################

##################################################################################################################
# Try K=2 and 100 initial random solutions

kmeans.X.NCI60 <- kmeans(X.NCI60,centers=2,iter.max=1000,nstart=100)

# The function returns a large list of objects. 

summary(kmeans.X.NCI60)

# The final partition is given in cluster

kmeans.X.NCI60$cluster

# The number of observations in each cluster

kmeans.X.NCI60$size

# The sample mean vectors of the clusters are in centers
# Not run in this case

# kmeans.X.NCI60$centers

# Total within-cluster sum of squares

kmeans.X.NCI60$tot.withinss 

# Make a plot of the first two PCs split in these four clusters

colors.kmeans.X.NCI60 <- c("deepskyblue4","firebrick4")[kmeans.X.NCI60$cluster]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.kmeans.X.NCI60)

# It does not appear to be a good solution

##################################################################################################################
# Compare several values of K and select the most appropriate one

# Run K-means for K ranging from K=2 to K=10 and compute the ratio WSS/BSS

WSS <- matrix(NA,nrow=9,ncol=1)
BSS <- matrix(NA,nrow=9,ncol=1)
Ratio.WSS.BSS <- matrix(NA,nrow=9,ncol=1)
for (k in 1 : 9){
  print(k)
  kmeans.X.NCI60 <- kmeans(X.NCI60,centers=k+1,iter.max=1000,nstart=100)
  WSS[k] <- kmeans.X.NCI60$tot.withinss
  BSS[k] <- kmeans.X.NCI60$betweenss
  Ratio.WSS.BSS[k] <- WSS[k]/BSS[k]
}

# Plot the ratios from K=2 to K=10

plot(2:10,Ratio.WSS.BSS,col="deepskyblue4",pch=20,main="Ratio for different values of K",xlab="K",ylab="WSS/BSS")
segments(x0=2:10,y0=Ratio.WSS.BSS,y1=0,col="deepskyblue4")

# Plot the differences to see whether the decrease in the ratio tends to 0

plot(3:10,diff(Ratio.WSS.BSS),col="deepskyblue4",pch=20,main="Differences of the ratios for different values of K",xlab="K",ylab="Differences between WSS/BSS")

# The plot appears to suggests the presence of 4 clusters, although 5 clusters can be also reasonable
# Let see the solution for K=4

kmeans.X.NCI60 <- kmeans(X.NCI60,centers=4,iter.max=1000,nstart=100)

# The cluster solution

kmeans.X.NCI60$cluster

# Make a plot of the first two PCs split in these four clusters

colors.kmeans.X <- c("deepskyblue4","firebrick4","orange","chartreuse")[kmeans.X.NCI60$cluster]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.kmeans.X)

##################################################################################################################
# Silhouette plot for the solution

# Install and load ISLR package

install.packages("cluster")
library(cluster)

# Compute the silhouette

sil.kmeans.X.NCI60 <- silhouette(kmeans.X.NCI60$cluster,dist(X.NCI60,"euclidean"))
plot(sil.kmeans.X.NCI60,col="deepskyblue4")

##################################################################################################################
# Perform k-means clustering for the principal components of the NCI60 data set
##################################################################################################################

# We take 21 PCs as in Topic 2 and K=4

kmeans.PCS.X.NCI60 <- kmeans(PCS.NCI60$x[,1:21],centers=4,iter.max=1000,nstart=100)

# Make a plot of the first two PCs split in these four clusters

colors.kmeans.PCS.X.NCI60 <- c("deepskyblue4","firebrick4","orange","chartreuse")[kmeans.PCS.X.NCI60$cluster]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.kmeans.PCS.X.NCI60)

# Compute the silhouette

sil.PCS.X.NCI60 <- silhouette(kmeans.PCS.X.NCI60$cluster,dist(X.NCI60,"euclidean"))
plot(sil.PCS.X.NCI60,col="deepskyblue4")

# Thus, the solution given with the PCs is similar than the one with the original data
# Note that the original data set uses 6830 variables while the PCs only uses 21 variables!!!

##################################################################################################################
# Perform k-medoids clustering for the NCI60 data set
##################################################################################################################

##################################################################################################################
# Fix K=4 clusters as in Kmeans

kmedoids.X.NCI60 <- pam(X.NCI60,k=4,metric="manhattan",stand=FALSE)

# Make a plot of the first two PCs split in these four clusters

colors.kmedoids.X.NCI60 <- c("deepskyblue4","firebrick4","orange","chartreuse")[kmedoids.X.NCI60$cluster]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.kmedoids.X.NCI60)

# Have a look at the silhouette

sil.kmedoids.X.NCI60 <- silhouette(kmedoids.X.NCI60$cluster,dist(X.NCI60,method="manhattan"))
plot(sil.kmedoids.X.NCI60,col="deepskyblue4")

# The solution is not very different than the one with Kmeans

##################################################################################################################
# Perform CLARA clustering for the NCI60 data set
##################################################################################################################

##################################################################################################################
# Fix K=4 clusters as in Kmeans

clara.X.NCI60 <- clara(X.NCI60,k=4,metric="manhattan",stand=FALSE,samples=100,sampsize=32)

# Make a plot of the first two PCs split in these four clusters

colors.clara.X.NCI60 <- c("deepskyblue4","firebrick4","orange","chartreuse")[clara.X.NCI60$cluster]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.clara.X.NCI60)

# Have a look at the silhouette

sil.clara.X.NCI60 <- silhouette(clara.X.NCI60$cluster,dist(X.NCI60,method="manhattan"))
plot(sil.clara.X.NCI60,col="deepskyblue4")

# The solution is similar to the one with Kmedoids

##################################################################################################################
# Perform k-medoids clustering with mixed variables for the Credit data set
##################################################################################################################

##################################################################################################################
# The Credit data set

# Load the data set into memory

data(Credit)

# Have a look at the first rows

head(Credit)

# The first column is just the identification number that we can just skip from the data matrix

Credit.X <- Credit[,2:ncol(Credit)]
head(Credit.X)

# Add the identification number as the name of the rows of the matrix (not necesary here because the identification
# number ranges from 1 to 400, but can be important if the identification is more complex)

rownames(Credit.X) <- Credit[,1]
head(Credit.X)

# Obtain the sample size and the dimension of the data matrix

n.Credit <- nrow(Credit.X)
n.Credit

p.Credit <- ncol(Credit.X)
p.Credit

##################################################################################################################
# Compute the Gower distance for the observations in the data set

dist.Gower.Credit <- daisy(Credit.X,metric="gower")
summary(dist.Gower.Credit)

# I means quantiative variable while N means qualitative variable

# Find the closest clients with the Gower distance

mat.dist.Gower.Credit <- as.matrix(dist.Gower.Credit)
Credit.X[which(mat.dist.Gower.Credit==min(mat.dist.Gower.Credit[mat.dist.Gower.Credit!=min(mat.dist.Gower.Credit)]),arr.ind = TRUE)[1,],]

# Find the most distant clients with the Gower distance

Credit.X[which(mat.dist.Gower.Credit==max(mat.dist.Gower.Credit[mat.dist.Gower.Credit!=max(mat.dist.Gower.Credit)]),arr.ind = TRUE)[1,],]

##################################################################################################################
# Consider K=2:20, run PAM and select K using the silhouette

K.Credit <- matrix(NA,nrow=1,ncol=19)
for (i in 1:19){
  kmedoids.X.Credit <- pam(mat.dist.Gower.Credit,k=i+1,diss=TRUE)
  K.Credit[i] <- kmedoids.X.Credit$silinfo$avg.width
}
plot(2:20,K.Credit,pch=20,col=colors.kmedoids.X.NCI60)

##################################################################################################################
# Run the algorithm for K=12 and get some information from the results

kmedoids.X.Credit <- pam(mat.dist.Gower.Credit,k=12,diss=TRUE)

# Medoids

Credit.X[kmedoids.X.Credit$medoids,]

# Have a look at the silhouette

sil.kmedoids.X.Credit <- silhouette(kmedoids.X.Credit$cluster,mat.dist.Gower.Credit)
plot(sil.kmedoids.X.Credit,col="deepskyblue4")
summary(sil.kmedoids.X.Credit)

##################################################################################################################
##################################################################################################################
# Hierarchical clustering analysis for the NCI60 data set
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Agglomerative hierarchical clustering analysis for the NCI60 data set
##################################################################################################################

##################################################################################################################
# Compute the distance matrix between the observations in the sample

dist.X.NCI60 <- daisy(X.NCI60,metric="manhattan",stand=FALSE)

##################################################################################################################
# Single linkage

single.X.NCI60 <- hclust(dist.X.NCI60,method="single")

# Plot dendogram of the solution and take k=4 as with Kmeans

plot(single.X.NCI60,main="Single linkage",cex=0.8)
rect.hclust(single.X.NCI60,k=4,border="deepskyblue4")

# See the assignment

cutree(single.X.NCI60,4)
table(cutree(single.X.NCI60,4))

# Alternative dendograms

install.packages("ape")
library(ape)

plot(as.phylo(single.X.NCI60),cex=0.8,label.offset=1)
plot(as.phylo(single.X),type="fan")

# Make a plot of the first two PCs split in these four clusters

colors.single.X.NCI60 <- c("deepskyblue4","firebrick4","orange","chartreuse")[cutree(single.X.NCI60,4)]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.single.X.NCI60)

# Have a look at the silhouette

sil.single.X.NCI60 <- silhouette(cutree(single.X.NCI60,4),dist.X.NCI60)
plot(sil.single.X.NCI60)

# This solution is awful

##################################################################################################################
# Complete linkage

complete.X.NCI60 <- hclust(dist.X.NCI60,method="complete")

# Plot dendogram of the solution and take k=4 as with Kmeans

plot(complete.X.NCI60,main="Complete linkage",cex=0.8)
rect.hclust(complete.X.NCI60,k=4,border="deepskyblue4")

# See the assignment

cutree(complete.X.NCI60,4)
table(cutree(complete.X.NCI60,4))

# Make a plot of the first two PCs split in these four clusters

colors.complete.X.NCI60 <- c("deepskyblue4","firebrick4","orange","chartreuse")[cutree(complete.X.NCI60,4)]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.complete.X.NCI60)

# Have a look at the silhouette

sil.complete.X.NCI60 <- silhouette(cutree(complete.X.NCI60,4),dist.X.NCI60)
plot(sil.complete.X.NCI60)

# This solution is not good either

##################################################################################################################
# Average linkage

average.X.NCI60 <- hclust(dist.X.NCI60,method="average")

# Plot dendogram of the solution and take k=4 as with Kmeans

plot(average.X.NCI60,main="Average linkage",cex=0.8)
rect.hclust(average.X.NCI60,k=4,border="deepskyblue4")

# See the assignment

cutree(average.X.NCI60,4)
table(cutree(average.X.NCI60,4))

# Make a plot of the first two PCs split in these four clusters

colors.average.X.NCI60 <- c("deepskyblue4","firebrick4","orange","chartreuse")[cutree(average.X.NCI60,4)]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.average.X.NCI60)

# Have a look at the silhouette

sil.average.X.NCI60 <- silhouette(cutree(average.X.NCI60,4),dist.X.NCI60)
plot(sil.average.X.NCI60)

# This solution is not good either

##################################################################################################################
# Ward method

ward.X.NCI60 <- hclust(dist.X.NCI60,method="ward.D2")

# Plot dendogram of the solution and take k=4 as with Kmeans

plot(ward.X.NCI60,main="Ward linkage",cex=0.8)
rect.hclust(ward.X.NCI60,k=4,border="deepskyblue4")

# See the assignment

cutree(ward.X.NCI60,4)
table(cutree(ward.X.NCI60,4))

# Make a plot of the first two PCs split in these four clusters

colors.ward.X.NCI60 <- c("deepskyblue4","firebrick4","orange","chartreuse")[cutree(ward.X.NCI60,4)]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.ward.X.NCI60)

# Have a look at the silhouette

sil.ward.X.NCI60 <- silhouette(cutree(ward.X.NCI60,4),dist.X.NCI60)
plot(sil.ward.X.NCI60)

# This solution is slightly better

##################################################################################################################
# Agglomerative hierarchical clustering analysis for the Credit data set
##################################################################################################################

# Use Complete method

complete.X.Credit <- hclust(dist.Gower.Credit,method="complete")

# Plot dendogram of the solution and take k=4

plot(complete.X.Credit,main="Complete linkage",cex=0.8)
rect.hclust(complete.X.Credit,k=4,border="deepskyblue4")

# See the assignment

cutree(complete.X.Credit,4)
table(cutree(complete.X.Credit,4))

# Have a look at the silhouette

sil.complete.X.Credit <- silhouette(cutree(complete.X.Credit,4),dist.Gower.Credit)
plot(sil.complete.X.Credit)

# What happen if we take k=5

sil.complete.X.Credit <- silhouette(cutree(complete.X.Credit,5),dist.Gower.Credit)
plot(sil.complete.X.Credit)

# What happen if we take k=6

sil.complete.X.Credit <- silhouette(cutree(complete.X.Credit,6),dist.Gower.Credit)
plot(sil.complete.X.Credit)

# What happen if we take k=12

sil.complete.X.Credit <- silhouette(cutree(complete.X.Credit,12),dist.Gower.Credit)
plot(sil.complete.X.Credit)

summary(sil.complete.X.Credit)

##################################################################################################################
# Divisive hierarchical clustering analysis for the NCI60 data set
##################################################################################################################

Diana.X.NCI60 <- diana(X.NCI60,metric="manhattan")

# Plot dendogram of the solution

plot(Diana.X.NCI60,main="DIANA")

# Hit two times Return to see the dendogram
# Take k=4

rect.hclust(Diana.X.NCI60,k=4,border="deepskyblue4")

# Make a plot of the first two PCs split in these four clusters

colors.Diana.X.NCI60 <- c("deepskyblue4","firebrick4","orange","chartreuse")[cutree(Diana.X.NCI60,4)]
plot(PCS.NCI60$x[,1:2],pch=20,col=colors.Diana.X.NCI60)

# Have a look at the silhouette

sil.Diana.X.NCI60 <- silhouette(cutree(Diana.X.NCI60,4),dist.X.NCI60)
plot(sil.Diana.X.NCI60)

# The solution is not very different from previous
