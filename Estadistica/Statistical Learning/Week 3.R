#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&
#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&
# Week 3 - Dimension reduction techniques
#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&
#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&

##################################################################################################################
# Open R or R-studio.
##################################################################################################################

##################################################################################################################
##################################################################################################################
# Principal Component Analysis
##################################################################################################################
##################################################################################################################

##################################################################################################################
# PCA for the NCI60 data set
##################################################################################################################

##################################################################################################################
# Install and load ISLR package

install.packages("ISLR")
library("ISLR")

# See the help page

?NCI60

# Load the data set into memory

data(NCI60)

# Remember that NCI60 is a list with two objects

summary(NCI60)

# The data matrix is the object called data

X <- NCI60$data

##################################################################################################################
# Sample sizes and dimensions of the NCI60 data set

n.NCI60 <- nrow(X)
n.NCI60
p.NCI60 <- ncol(X)
p.NCI60

##################################################################################################################
# There are many different functions in R to perform PCA 
# Next, we use the function prcomp
# All the variables in the NCI60 data set have the same units of measurement. Therefore, we use the sample 
# covariance matrix

PCS.NCI60 <- prcomp(X)

# Have a look at the outputs

names(PCS.NCI60)

##################################################################################################################
# The eigenvectors of the sample covariance matrix are given in rotation
# The output is a matrix of size 6830x64, so we only have a look at the first few rows

dim(PCS.NCI60$rotation)
head(PCS.NCI60$rotation)

# Note that only 64 eigenvectors (corresponding to 64 PCs) appear
# This is because Sx has n<p and then only 64 eigenvalues can be different than 0

##################################################################################################################
# The PC scores is are in x. In this case, the scores are a matrix of size 64x64

dim(PCS.NCI60$x)
head(PCS.NCI60$x)

##################################################################################################################
# Make a plot of the first two PCs

plot(PCS.NCI60$x[,1:2],pch=20,col="deepskyblue4")

# Remember that we are interested in make groups of different cancer types
# The first two PCs suggest the presence of at least two different groups
# This information is completely hidden if we consider the 6830 variables

# We can add the names of the cancer

text(PCS.NCI60$x[,1:2],labels=NCI60$labs,pos = 1,col="firebrick4",cex=0.5)

##################################################################################################################
# Interpretation of the first PC: The first eigenvector has 6830 values
# Each value represent the weight of the associated variable (the measurement on a particular gen)
# Have a look at the important variables (genes) in the first PC

plot(PCS.NCI60$rotation[,1],pch=20,col="deepskyblue4",main="Weights for the first PC")
abline(h=0)

# Have a look at the important variables (genes) in the second PC

plot(PCS.NCI60$rotation[,2],pch=20,col="deepskyblue4",main="Weights for the second PC")
abline(h=0)

# Have a look at the important variables (genes) in the first two PCs

plot(PCS.NCI60$rotation[,1:2],pch=20,col="deepskyblue4",main="Weights for the first two PCs")
abline(h=0,v=0)
text(PCS.NCI60$rotation[,1:2],labels=colnames(NCI60$data),pos = 1,col="firebrick4",cex=0.5)

# Those genes that appear to be outliers are those genes that creates the largest variability in
# the first two PCs, which are the most important ones

##################################################################################################################
# Make a plot of the first three PCs

install.packages("scatterplot3d")
library("scatterplot3d")

# Three-variate scatterplot

s3d.NCI60 <- scatterplot3d(PCS.NCI60$x[,1:3],pch=20,color="deepskyblue4",main="3D Scatterplot",type="h")
text(s3d.NCI60$xyz.convert(PCS.NCI60$x[,1:3]),labels=NCI60$labs,pos = 1,col="firebrick4",cex=0.5)

# Three-dimensional scatterplot of the Weights

scatterplot3d(PCS.NCI60$rotation[,1:3],pch=20,color="deepskyblue4",main="3D Scatterplot",type="h")

##################################################################################################################
# How many PCs?

# Screeplot with the 64 eigenvalues

screeplot(PCS.NCI60,npcs=64,main="Screeplot",col="deepskyblue4",type="lines",pch=20)

# Have a look at the proportion of explained variance and the cumulative proportion of explained variance

summary(PCS.NCI60)

# We need 21 PCs to have the 70% of the total variability of the data set.
# The mean of the eigenvalues can be obtained as follows

EVAL.NCI60 <- PCS.NCI60$sdev^2
mean(EVAL.NCI60)

# The number of eigenvalues larger than the mean of them is 

sum(EVAL.NCI60>mean(EVAL.NCI60))

# In any case, we reduce the dimension of the data set from 6830 to 21 or 17. That is we consider
# either the 0.30% or the 0.24% of the number of variables in the data set keeping around the 70%
# of the information inside

##################################################################################################################
# PCA for the College data set
##################################################################################################################

##################################################################################################################
# Load the College data set

# College data set

?College

# Load data set

data(College)

# Have a look at the first rows

head(College)

# Size of the matrix

dim(College)

# We have 777 colleges and 18 variables

##################################################################################################################
# We define the data matrix with the quantitative variables and the indicator vector with the qualitative variable (Private)

X <- College[,2:18]
head(X)

Y <- College[,1]
head(Y)
table(Y)

##################################################################################################################
# Define sample size and the dimension

n.X <- nrow(X)
n.X
p.X <- ncol(X)
p.X

##################################################################################################################
# Plot the original variables

library(MASS)

colors.X <- c("deepskyblue4","firebrick4")[Y]
parcoord(X,col=colors.X,var.label=TRUE)

##################################################################################################################
# Two possibilities: we can make PCA on the whole data set or on the two groups
# See what happens if we make PCA on the whole data set
# Use the sample correlation matrix

PCS.X <- prcomp(X,scale = TRUE)

##################################################################################################################
# Eigenvectors of the sample correlation matrix

dim(PCS.X$rotation)
PCS.X$rotation

##################################################################################################################
# PC scores

dim(PCS.X$x)
head(PCS.X$x)

##################################################################################################################
# Make a plot of the first two PCs

plot(PCS.X$x[,1:2],pch=19,col=colors.X)

# The first two PCs show the presence of the two different groups
# This information was not clear in a plot of the 17 variables

##################################################################################################################
# Interpretation of the first PC: Weights for the first PC

plot(1:p.X,PCS.X$rotation[,1],pch=19,col="deepskyblue4",main="Weights for the first PC")
abline(h=0)
text(1:p.X,PCS.X$rotation[,1],labels=colnames(X),pos=1,col="firebrick4",cex=0.5)

##################################################################################################################
# Interpretation of the second PC: Weights for the second PC

plot(1:p.X,PCS.X$rotation[,2],pch=19,col="deepskyblue4",main="Weights for the second PC")
abline(h=0)
text(1:p.X,PCS.X$rotation[,2],labels=colnames(X),pos=1,col="firebrick4",cex=0.5)

##################################################################################################################
# Have a look at the important variables in the first two PCs
# Note the different groups in the data

plot(PCS.X$rotation[,1:2],pch=19,col="deepskyblue4",main="Weights for the first two PCs")
abline(h=0,v=0)
text(PCS.X$rotation[,1:2],labels=colnames(X),pos=1,col="firebrick4",cex=0.5)

##################################################################################################################
# How many PCs?

# Screeplot with the 17 eigenvalues

screeplot(PCS.X,npcs=17,main="Screeplot",col="deepskyblue4",type="lines",pch=19)

# Have a look at the proportion of explained variance and the cumulative proportion of explained variance

summary(PCS.X)

# We need 4 PCs to have the 70% of the total variability of the data set.
# As we are using the sample correlation matrix, the mean of the eigenvalues 
# is equal to 1. Check it

EVAL.X <- PCS.X$sdev^2
mean(EVAL.X)

# The number of eigenvalues larger than the mean of them is 

sum(EVAL.X>mean(EVAL.X))

# In this case, we reduce the dimension of the data set from 17 to 4. That is we consider
# either the 23.53% of the number of variables in the data set keeping around the 70%
# of the information inside

##################################################################################################################
# Plot the scores (the four PCs)

pairs(PCS.X$x[,1:4],col=colors.X,pch=19,main="The first four PCs")

# The PCs show that the variables have different behavior in terms of the two groups

##################################################################################################################
# Detect outliers with the PCs
##################################################################################################################

##################################################################################################################
# If we look for outliers in a data set like this, it is better to look in the two groups separately
# Here, we consider only the private colleges

##################################################################################################################
# Define the subset of private colleges

X.priv <- X[Y=="Yes",]
head(X.priv)

n.priv <- nrow(X.priv)
n.priv
p.priv <- ncol(X.priv)
p.priv

##################################################################################################################
# Obtain PCs in the private colleges

PCS.X.priv <- prcomp(X.priv,scale = TRUE)

##################################################################################################################
# Select the number of PCs

screeplot(PCS.X.priv,npcs=17,main="Screeplot",col="deepskyblue4",type="lines",pch=19)

# Three appears to be ok

##################################################################################################################
# Install and load robustbase package

install.packages("robustbase")
require("robustbase")

##################################################################################################################
# Compute robust estimates of mean vector and covariance matrix

mcd.X.priv <- covMcd(PCS.X.priv$x[,1:3])

rob.mv.X.priv <- mcd.X.priv$center
rob.mv.X.priv

rob.CM.X.priv <- mcd.X.priv$cov
rob.CM.X.priv

# Compute robust Mahalanobis distances for the PCs of private colleges

rob.mah.X.priv <- mahalanobis(PCS.X.priv$x[,1:3],rob.mv.X.priv,rob.CM.X.priv)
rob.mah.X.priv

# Sort the robust Mahalanobis distances

sort.rob.mah.X.priv <- sort(rob.mah.X.priv,index.return=TRUE)$x
sort.rob.mah.X.priv

# Plot the sorted robust distances

plot(sort.rob.mah.X.priv,pch=19,col="deepskyblue",xlab="",ylab="",main="Robust Mahalanobis distances for the PCs of private Colleges")

# Plot the PCs and the outliers detected

colors.X.priv.outliers <- c("deepskyblue4","firebrick4")[1 * (rob.mah.X.priv>150) + 1]
pairs(PCS.X.priv$x[,1:3],col=colors.X.priv.outliers,pch=19,main="The first three PCs with outliers")

##################################################################################################################
##################################################################################################################
# Sparse Principal component Analysis with the College data set
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Install packages PMA 

install.packages("PMA")

##################################################################################################################
# Install package impute from this webpage:
# When asked about to update some packages say n

source("https://bioconductor.org/biocLite.R")
biocLite("impute")

##################################################################################################################
# Load library PMA that loads library impute

library("PMA")

##################################################################################################################
# Define X as a matrix object

is(X)
X.mat <- as.matrix(X)
head(X.mat)

# Then, obtain the scaled variables

Y <- scale(X.mat)
head(Y)

##################################################################################################################
# To perform sparse principal component analysis, we run the function SPC
# For that, we need to fix the number k in the L1 norm restriction of the weights. 
# The results will depend on this selection: 
#   1. If k is taken too large, then, the standard and the sparse PCs will be the same.
#   2. If k is taken too small, then, there will be many 0s in the weights

# This value should be tuned. In this case, we take k=2.5

# It is important to note that sometimes we can obtain errors using this function
# because the optimizer does not converge or find some problems for numerical reasons.
# In these cases, the option is to reduce the number of PCs used

SPC.s <- SPC(Y,sumabsv=2.5,K=p.X,orth=TRUE)

# Have a look at the results

# First, we see the proportion of variance explained by the SPCs.
# The formulas are different than those from the standard PCs because the SPC
# are not obtained from eigenvectors and eigenvalues (not entering into details)

prop.var.expl <- SPC.s$prop.var.explained
prop.var.expl

# As you can see, the variance explained by the first 4 SPCs is smaller than the corresponding 
# to the first 4 PCs obtained previously. Anyway, for comparison purposes, we take 4 SPCs

# The weights are given by

V <- SPC.s$v[,1:4]
V

# As it can be seen, most of the weights are equal to 0.
# Therefore, PCs are associated only to some of the variables favouring the interpretation.
# Check the important variables

# Obtain the scores for these 4 SPCs

Z.S <- Y %*% V
head(Z.S)
colnames(Z.S) <- c("SPC1","SPC2","SPC3","SPC4")

# Note that, altough we do impose orthogonality in the optimization problem, we do not obtain 
# orthogonal sparse PCs as in the case of the standard PCs

cor(Z.S)

# Plot the scores

pairs(Z.S,col=colors.X,pch=19,main="The first four sparse PCs")

# Compare the PC scores with the sparse PC scores

par(mfrow=c(2,2))

plot(PCS.X$x[,1],Z.S[,1],col=colors.X,pch=19,main="Comparison of first scores",xlab="Fist PC",ylab="First SPC")
plot(PCS.X$x[,2],Z.S[,2],col=colors.X,pch=19,main="Comparison of second scores",xlab="Second PC",ylab="Second SPC")
plot(PCS.X$x[,3],Z.S[,3],col=colors.X,pch=19,main="Comparison of third scores",xlab="Third PC",ylab="Third SPC")
plot(PCS.X$x[,4],Z.S[,4],col=colors.X,pch=19,main="Comparison of fourth scores",xlab="Fourth PC",ylab="Fourth SPC")

##################################################################################################################
##################################################################################################################
# Independent Components Analysis with the College data set
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Install and load fastICA package

install.packages("fastICA")
require("fastICA")

##################################################################################################################
# Obtain the ICs with the College data set. We use 4 ICs as in PCA and standarize the data

FICA.X <- fastICA(X,n.comp=4,row.norm=TRUE)

# The ICA weights can be found here

FICA.X$K

# Compare it with the PCA weights

PCS.X$rotation[,1:4]

# The ICA scores can be found here

head(FICA.X$S)

# Have a look at the ICA scores obtained

pairs(FICA.X$S,col=colors.X,pch=19,main="Four ICA components")

# Note that the ICs scores have zero mean and identity covariance matrix

colMeans(FICA.X$S)
cov(FICA.X$S)

# Compare PCA scores with ICA scores

par(mfrow=c(2,2))
plot(PCS.X$x[,1],FICA.X$S[,1],col=colors.X,pch=19,xlab="PC",ylab="IC",main="First")
plot(PCS.X$x[,2],FICA.X$S[,2],col=colors.X,pch=19,xlab="PC",ylab="IC",main="Second")
plot(PCS.X$x[,3],FICA.X$S[,3],col=colors.X,pch=19,xlab="PC",ylab="IC",main="Third")
plot(PCS.X$x[,4],FICA.X$S[,4],col=colors.X,pch=19,xlab="PC",ylab="IC",main="Fourth")

# The PCs and the ICs are quite different
