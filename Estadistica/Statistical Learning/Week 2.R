#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&
#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&
# Week 2 - Multidimensional data (II)
#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&
#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&

##################################################################################################################
# Open R or R-studio.
##################################################################################################################

##################################################################################################################
##################################################################################################################
# Visualizing multidimensional data sets
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Plots for single qualitative variables
##################################################################################################################

##################################################################################################################
# Barplots and piecharts of the variable spam of the spam data set

# Load the library kernlab and the spam data set, and define the data matrix and the indicator vector

library("kernlab")
data(spam)
head(spam)
spam.X <- spam[,1:(ncol(spam)-1)]
head(spam.X)
spam.Y <- spam[,ncol(spam)]
head(spam.Y)

# Obtain absolute frequencies of the indicator variable

abs.freq.spam.Y <- table(spam.Y)
abs.freq.spam.Y

# Create a graphical window with two parts

par(mfrow=c(1,2))

# Barplot and piechart of the spam variable of the spam data set

barplot(abs.freq.spam.Y,col="deepskyblue4",main="Barplot of the variable spam",ylab="Absolute frequencies",xlab="Spam values")
pie(abs.freq.spam.Y,col=c("deepskyblue4","firebrick4"),main="Piechart of the variable spam",radius=2)

##################################################################################################################
# Barplots and piecharts of the variable DMEDUC of the births2006 data set

# Load the library nutshell and the births2006 data set, and define the data matrix

library("nutshell")
data(births2006.smpl)
births2006.X <- births2006.smpl
head(births2006.X)

# Obtain absolute frequencies

abs.freq.births2006.DMEDUC <- table(births2006.X$DMEDUC)
abs.freq.births2006.DMEDUC

# Barplot and piechart of the DMEDUC variable of the births2006 data set

par(mfrow=c(1,2))
barplot(abs.freq.births2006.DMEDUC,col="deepskyblue4",main="Barplot of the variable DMEDUC",ylab="Absolute frequencies",xlab="DMEDUC values")
pie(abs.freq.births2006.DMEDUC,main="Piechart of the variable DMEDUC",radius=2)

# Join classes of the DMEDUC variable

levels(births2006.smpl$DMEDUC)

DMEDUC.JOIN <- vector(mode="character",length=length(births2006.X$DMEDUC))
DMEDUC.JOIN[births2006.X$DMEDUC=="1 year of college"] <- "College"
DMEDUC.JOIN[births2006.X$DMEDUC=="1 year of high school"] <- "High school"
DMEDUC.JOIN[births2006.X$DMEDUC=="1 Years of elementary school"] <- "Elementary school"
DMEDUC.JOIN[births2006.X$DMEDUC=="2 years of college"] <- "College"
DMEDUC.JOIN[births2006.X$DMEDUC=="2 Years of elementary school"] <- "Elementary school"
DMEDUC.JOIN[births2006.X$DMEDUC=="2 years of high school"] <- "High school"
DMEDUC.JOIN[births2006.X$DMEDUC=="3 years of college"] <- "College"
DMEDUC.JOIN[births2006.X$DMEDUC=="3 Years of elementary school"] <- "Elementary school"
DMEDUC.JOIN[births2006.X$DMEDUC=="3 years of high school"] <- "High school"
DMEDUC.JOIN[births2006.X$DMEDUC=="4 Years of elementary school"] <- "Elementary school"
DMEDUC.JOIN[births2006.X$DMEDUC=="4 years of high school"] <- "High school"
DMEDUC.JOIN[births2006.X$DMEDUC=="5 Years of elementary school"] <- "Elementary school"
DMEDUC.JOIN[births2006.X$DMEDUC=="6 Years of elementary school"] <- "Elementary school"
DMEDUC.JOIN[births2006.X$DMEDUC=="7 Years of elementary school"] <- "Elementary school"
DMEDUC.JOIN[births2006.X$DMEDUC=="8 Years of elementary school"] <- "Elementary school"
DMEDUC.JOIN[births2006.X$DMEDUC=="No formal education"] <- "No education"
DMEDUC.JOIN[births2006.X$DMEDUC=="Not on certificate"] <- NA
DMEDUC.JOIN[births2006.X$DMEDUC=="NULL"] <- NA

head(births2006.X)
colnames(births2006.X)[colnames(births2006.X)=="DMEDUC"] <- "DMEDUC.JOIN"
head(births2006.X)
births2006.X[,colnames(births2006.X)=="DMEDUC.JOIN"] <- DMEDUC.JOIN
head(births2006.X)

# Barplot and piechart of the DMEDUC.JOIN variable of the births2006 data set, excluding NAs

abs.freq.births2006.DMEDUC.JOIN <- table(births2006.X$DMEDUC.JOIN)
abs.freq.births2006.DMEDUC.JOIN

barplot(abs.freq.births2006.DMEDUC.JOIN,col="deepskyblue4",main="Barplot of the variable DMEDUC.JOIN",ylab="Absolute frequencies",xlab="DMEDUC.JOIN values")
pie(abs.freq.births2006.DMEDUC.JOIN,main="Piechart of the variable DMEDUC.JOIN",radius=2)

##################################################################################################################
# Plots for two qualitative variables
##################################################################################################################

##################################################################################################################
# Join barplots for the variables SEX abd DMEDUC.JOIN of the births2006 data set

table(births2006.X$DMEDUC.JOIN,births2006.X$SEX)
par(mfrow=c(1,1))
plot(table(births2006.X$DMEDUC.JOIN,births2006.X$SEX),xlab="DMEDUC.JOIN",ylab="SEX",col=c("deepskyblue4","firebrick4"))

##################################################################################################################
# Plots for quantitative variables
##################################################################################################################

##################################################################################################################
# Barplot of the variable capitalLong of the spam data set

# Obtain absolute frequencies

abs.freq.spam.capitalLong <- table(spam.X$capitalLong)
abs.freq.spam.capitalLong

# Barplot of the variable capitalLong of the spam data set

barplot(abs.freq.spam.capitalLong,col="deepskyblue4",main="Barplot of the variable capitalLong",ylab="Absolute frequencies",xlab="capitalLong values")

##################################################################################################################
# Boxplot for the second gene of the NCI60 data set

# Load the data set

library("ISLR")
data(NCI60)
NCI60.X <- NCI60$data

# Plot the boxplot

boxplot(NCI60.X[,2],main=colnames(NCI60$data)[2],xlab="",col="deepskyblue4")

##################################################################################################################
# Boxplot of the variable capitalAve of the spam data set

# Plot the boxplot

boxplot(spam.X$capitalAve,main="Boxplot of capitalAve",xlab="",col="deepskyblue4")

# Show high skewness, so that, transform with logarithm after checking that there are no 0s

sum(spam.X$capitalAve==0)
boxplot(log(spam.X$capitalAve),main="Boxplot of logarithm of capitalAve",xlab="",col="deepskyblue4")

# Plot boxplots of the logarithm of capitalAve in terms of the variable spam

boxplot(log(spam.X$capitalAve)~spam.Y,main="Boxplot of logarithm of capitalAve",xlab="",ylab="Logarithm of capitalAve",col="deepskyblue4")

##################################################################################################################
# Histogram of the second gene of the NCI60 data set

hist(NCI60$data[,2],main=colnames(NCI60$data)[2],xlab="",freq=FALSE,col="deepskyblue4")

##################################################################################################################
# Histogram of the variable capitalAve of the spam data set

# Plot the histogram

hist(spam.X$capitalAve,main="Histogram of capitalAve",xlab="",col="deepskyblue4",freq=FALSE)

# Transform with logarithm

hist(log(spam.X$capitalAve),main="Boxplot of logarithm of capitalAve",xlab="",col="deepskyblue4",freq=FALSE)

# Plot histograms of the logarithm of capitalAve in terms of the variable spam

# Create two histogram objects

hist.spam <- hist(log(spam.X$capitalAve[spam.Y=="spam"]),plot=FALSE)
hist.nonspam <- hist(log(spam.X$capitalAve[spam.Y=="nonspam"]),plot=FALSE)

# Plot the histograms together

plot(hist.spam,col=rgb(0,0,1,1/4),xlim=c(0,max(log(spam.X$capitalAve))),ylim=c(0,1),freq=FALSE,xlab="",main="Histogram of capitalAve in terms of spam")
plot(hist.nonspam,col=rgb(1,0,0,1/4),xlim=c(0,max(log(spam.X$capitalAve))),freq=FALSE,add=T)

##################################################################################################################
# Kernel density of the second gene of the NCI60 data set

plot(density(NCI60$data[,2],kernel="gaussian"),main=colnames(NCI60$data)[2],xlab="",col="deepskyblue4",lwd=2)

##################################################################################################################
# Kernel density of the variable capitalAve of the spam data set

# Plot the kernel densities

plot(density(spam.X$capitalAve,kernel="gaussian"),main="Kernel density of capitalAve",xlab="",col="deepskyblue4",lwd=2)

# Transform with logarithm

plot(density(log(spam.X$capitalAve),kernel="gaussian"),main="Kernel density of logarithm of capitalAve",xlab="",col="deepskyblue4",lwd=2)

# Plot kernel densities of the logarithm of capitalAve in terms of the variable spam

d.spam <- density(log(spam.X$capitalAve[spam.Y=="spam"]),kernel="gaussian")
min.x.d.spam <- min(d.spam$x)
max.x.d.spam <- max(d.spam$x)
min.y.d.spam <- min(d.spam$y)
max.y.d.spam <- max(d.spam$y)

d.nonspam <- density(log(spam.X$capitalAve[spam.Y=="nonspam"]),kernel="gaussian")
min.x.d.nonspam <- min(d.nonspam$x)
max.x.d.nonspam <- max(d.nonspam$x)
min.y.d.nonspam <- min(d.nonspam$y)
max.y.d.nonspam <- max(d.nonspam$y)

min.x <- min(c(min.x.d.spam,min.x.d.nonspam))
max.x <- max(c(max.x.d.spam,max.x.d.nonspam))
min.y <- min(c(min.y.d.spam,min.y.d.nonspam))
max.y <- max(c(max.y.d.spam,max.y.d.nonspam))

plot(c(min.x,max.x),c(min.y,max.y),xlab="",ylab="",main="Kernel density of logarithm of capitalAve in terms of spam",type="n")
lines(d.spam$x,d.spam$y,col="deepskyblue4",lwd=2)
lines(d.nonspam$x,d.nonspam$y,col="firebrick4",lwd=2)

##################################################################################################################
# Scatterplots
##################################################################################################################

##################################################################################################################
# Scatterplot of the variables Income and Rating in the Credit data set

# Load Credit data set

data(Credit)
Credit.X <- Credit[,2:ncol(Credit)]
head(Credit.X)
rownames(Credit.X) <- Credit[,1]
head(Credit.X)

# Scatterplot

plot(Credit.X$Income,Credit.X$Rating,pch=20,col="deepskyblue4",xlab="First variable",ylab="Second variable",main="Scatterplot")

# Scatterplot with variable Married

colors.Credit <- c("deepskyblue4","firebrick4","green")[Credit.X$Married]
plot(Credit.X$Income,Credit.X$Rating,pch=20,col=colors.Credit,xlab="First variable",ylab="Second variable",main="Scatterplot")

##################################################################################################################
# 3D-Scatterplots of the variables Income, Limit and Rating in the Credit data set

# Install and load library plotly

install.packages("plotly")
library(plotly)

# 3D-Scatterplot

plot_ly(as.data.frame(Credit.X),x=~Credit.X$Income,y=~Credit.X$Limit,z=~Credit.X$Rating)

##################################################################################################################
# Scatterplot matrix for the quantitative variables in the Credit data set

Credit.X.quan <- Credit.X[,c(1,2,3,4,5,6,11)] 
pairs(Credit.X.quan,pch=19,col="deepskyblue4")
pairs(Credit.X.quan,pch=19,col=colors.Credit)

##################################################################################################################
# Scatterplot matrix for the quantitative variables in the Credit data set with density estimates

install.packages("car")
library(car)
scatterplotMatrix(Credit.X.quan,pch=19,col="deepskyblue4",smoother=FALSE,reg.line=FALSE)

##################################################################################################################
# Parallel coordinates plots for the quantitative variables in the Credit data set

# Install and load library MASS

install.packages("MASS")
library("MASS")

parcoord(Credit.X.quan,col="deepskyblue4",var.label=TRUE)
parcoord(Credit.X.quan,col=colors.Credit,var.label=TRUE)

##################################################################################################################
##################################################################################################################
# Standard descriptive measures for multivariate data sets
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Sample mean vector
##################################################################################################################

##################################################################################################################
# Sample mean vector for the quantitative variables of the Default data set

data(Default)
Default.X <- Default[,2:ncol(Default)]
head(Default.X)
Default.Y <- Default[,1]
head(Default.Y)

smv.spam.X <- colMeans(Default.X[,2:3])
smv.spam.X

plot(Default.X$balance,Default.X$income,pch=20,col="deepskyblue4",xlab="Balance",ylab="Income")
points(smv.spam.X[1],smv.spam.X[2],pch=19,col="firebrick4")

##################################################################################################################
# Correlation between a quantative variable and a binary variable

cor(Default.X$income,as.numeric(Default.X$student))
plot(Default.X$income,Default.X$student,pch=20,col="deepskyblue4",xlab="Income",ylab="Student")

##################################################################################################################
# Sample covariance matrix
##################################################################################################################

##################################################################################################################
# Sample covariance matrix for the spam data set

# Obtain sample size and dimension

head(spam.X)
n.spam <- nrow(spam.X)
n.spam
p.spam <- ncol(spam.X)
p.spam

# Obtain sample covariance matrix

S.spam.X <- cov(spam.X)
S.spam.X

# Obtain eigenvectors and eigenvalues of the sample covariance matrix

eig.S.spam.X <- eigen(S.spam.X)

eig.S.spam.X$vectors
dim(eig.S.spam.X$vectors)

eig.S.spam.X$values

# Plot the eigenvalues of the sample covariance matrix

plot(1:p.spam,eig.S.spam.X$values,col="deepskyblue4",type="b",xlab="Number",ylab="Eigenvalues",pch=19)

##################################################################################################################
# For large dimensions, cov can be very slow. It is better to use the function cov.shrink from library
# corpcor

install.packages("corpcor")
library("corpcor")

# Example with the NCI60 data set

S.NCI60.X <- cov.shrink(NCI60.X,lambda=0,lambda.var=0,verbose=FALSE)
dim(S.NCI60.X)
S.NCI60.X[1:10,1:10]

##################################################################################################################
# Sample correlation matrix
##################################################################################################################

##################################################################################################################
# Sample correlation matrix for the spam data set

R.spam.X <- cor(spam.X)
R.spam.X
dim(R.spam.X)

# Obtain a plot of the correlation matrix

install.packages("corrplot")
library("corrplot")

corrplot(R.spam.X)
corrplot(R.spam.X,order="hclust")

# Obtain eigenvectors and eigenvalues of the sample correlation matrix

eig.R.spam.X <- eigen(R.spam.X)

eig.R.spam.X$vectors
dim(eig.R.spam.X$vectors)

eig.R.spam.X$values

# Plot the eigenvalues of the sample correlation matrix

plot(1:p.spam,eig.R.spam.X$values,col="deepskyblue4",type="b",xlab="Number",ylab="Eigenvalues",pch=19)

##################################################################################################################
# Compute the sample correlation matrix from the sample covariance matrix for the NCI data set

R.NCI60.X <- cov2cor(S.NCI60.X)
dim(R.NCI60.X)
R.NCI60.X[1:10,1:10]

##################################################################################################################
##################################################################################################################
# Multidimensional distributions and inference
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Outlier detection with Mahalanobis distances
##################################################################################################################

# See the help page

?College

# Load the data set into memory

data(College)

# Get some information

str(College)

# have a look at the first rows

head(College)

# Define the data matrix and the indicator vector

College.X <- College[,2:ncol(College)]
head(College.X)
College.Y <- College[,1]
head(College.Y)

# Try to plot the data matrix

colors.College <- c("deepskyblue4","firebrick4")[College.Y]
pairs(College.X,pch=19,col=colors.College)
parcoord(College.X,col=colors.College,var.label=TRUE)

# The indicator vector is an important factor to discriminate between Colleges
# Split the data matrix in two groups

College.priv <- College.X[College.Y=="Yes",]
head(College.priv)

College.nonpriv <- College.X[College.Y=="No",]
head(College.nonpriv)

# Plot the data matrices

pairs(College.priv,pch=19,col="deepskyblue4")
parcoord(College.priv,col="deepskyblue4",var.label=TRUE)

pairs(College.nonpriv,pch=19,col="deepskyblue4")
parcoord(College.nonpriv,col="deepskyblue4",var.label=TRUE)

##################################################################################################################
# Obtain the (squared) Mahalanobis distances with respect to the sample mean vector for the private colleges

smv.College.priv <- colMeans(College.priv)
smv.College.priv

S.College.priv <- cov(College.priv)
S.College.priv

mah.College.priv <- mahalanobis(College.priv,smv.College.priv,S.College.priv)
mah.College.priv

# Sort the Mahalanobis distances

sort.mah.College.priv <- sort(mah.College.priv,index.return=TRUE)$x
sort.mah.College.priv

# Plot the sorted distances

plot(sort.mah.College.priv,pch=19,col="deepskyblue",xlab="",ylab="",main="Mahalanobis distances for the private Colleges")

##################################################################################################################
# Obtain the (squared) Mahalanobis distances with respect to the sample mean vector for the nonprivate colleges

smv.College.nonpriv <- colMeans(College.nonpriv)
smv.College.nonpriv

S.College.nonpriv <- cov(College.nonpriv)
S.College.nonpriv

mah.College.nonpriv <- mahalanobis(College.nonpriv,smv.College.nonpriv,S.College.nonpriv)
mah.College.nonpriv

# Sort the Mahalanobis distances

sort.mah.College.nonpriv <- sort(mah.College.nonpriv,index.return=TRUE)$x
sort.mah.College.nonpriv

# Plot the sorted distances

plot(sort.mah.College.nonpriv,pch=19,col="deepskyblue",xlab="",ylab="",main="Mahalanobis distances for the nonprivate Colleges")

##################################################################################################################
# Use robust estimates of mean vector and covariance matrix

install.packages("robustbase")
require("robustbase")

##################################################################################################################
# Private colleges

# Obtain the Minimum Covariance Determinant (MCD) estimators

mcd.College.priv <- covMcd(College.priv)

rob.mv.College.priv <- mcd.College.priv$center
rob.mv.College.priv

rob.CM.College.priv <- mcd.College.priv$cov
rob.CM.College.priv

# Compute robust Mahalanobis distances for the private colleges

rob.mah.College.priv <- mahalanobis(College.priv,rob.mv.College.priv,rob.CM.College.priv)
rob.mah.College.priv

# Sort the robust Mahalanobis distances

sort.rob.mah.College.priv <- sort(rob.mah.College.priv,index.return=TRUE)$x
sort.rob.mah.College.priv

# Plot the sorted robust distances

plot(sort.rob.mah.College.priv,pch=19,col="deepskyblue",xlab="",ylab="",main="Robust Mahalanobis distances for the private Colleges")

##################################################################################################################
# NonPrivate colleges

# Obtain the Minimum Covariance Determinant (MCD) estimators

mcd.College.nonpriv <- covMcd(College.nonpriv)

rob.mv.College.nonpriv <- mcd.College.nonpriv$center
rob.mv.College.nonpriv

rob.CM.College.nonpriv <- mcd.College.nonpriv$cov
rob.CM.College.nonpriv

# Compute robust Mahalanobis distances for the nonprivate colleges

rob.mah.College.nonpriv <- mahalanobis(College.nonpriv,rob.mv.College.nonpriv,rob.CM.College.nonpriv)
rob.mah.College.nonpriv

# Sort the robust Mahalanobis distances

sort.rob.mah.College.nonpriv <- sort(rob.mah.College.nonpriv,index.return=TRUE)$x
sort.rob.mah.College.nonpriv

# Plot the sorted robust distances

plot(sort.rob.mah.College.nonpriv,pch=19,col="deepskyblue",xlab="",ylab="",main="Robust Mahalanobis distances for the nonprivate Colleges")

##################################################################################################################
# Filling missing values
##################################################################################################################

install.packages("mice")
library(mice)

# Remember the births2006 data set

?births2006.smpl

births2006 <- births2006.smpl
head(births2006)

# Brief summary of the data set

summary(births2006)

# See all the values of the variable DMEDUC

summary(births2006$DMEDUC)

# There are missing values in TBO_REC, WTGAIN, APGAR5, DBWT, and DMEDUC
# Moreover, in DMETH_REC appears the class Unknown that can be seen also as missing values

##################################################################################################################
# Sample mean imputation

imp.mean.births2006 <- mice(births2006,m=1,method="mean",maxit=1)
imp.mean.births2006.data <- complete(imp.births2006)
head(imp.mean.births2006.data)
summary(imp.mean.births2006.data)

# Note that for discrete variables it is required to round the imputed values

##################################################################################################################
# Conditional approach 

births2006.1000 <- births2006[1:1000,]
head(births2006.1000)
summary(births2006.1000)

imp.pmm.births2006.1000 <- mice(births2006.1000,m=1,method="pmm",maxit=1)
imp.pmm.births2006.1000.data <- complete(imp.pmm.births2006.1000)
head(imp.pmm.births2006.1000.data)
summary(imp.pmm.births2006.1000.data)

##################################################################################################################
##################################################################################################################
# Sparse covariance matrix estimation for Spam dataset
##################################################################################################################
##################################################################################################################

# Install and load the library spcov if it is not already installed

install.packages("spcov")
library(spcov)

# Remember the sample covariance matrix of the spam data set

S.spam.X
R.spam.X
corrplot(R.spam.X)

# Define the matrix P as the matrix of 1's of dimension p but with 0's in the main diagonal

P <- matrix(1,p.spam,p.spam)
diag(P) <- 0

# lam is the penalty parameter and step.size is the step size to use in generalized gradient descent
# See what happens if we take a small value of lam

lam <- 0.01
step.size <- 100

# Sigma is an initial guess for the covariance matrix and S is the sample covariance matrix
# Fix both as the sample covariance matrix

Sparse.S.spam.X <- spcov(Sigma=S.spam.X,S=S.spam.X,lambda=lam*P,step.size=step.size)
summary(Sparse.S.spam.X)

Sparse.Cov.spam.X <- Sparse.S.spam.X$Sigma
Sparse.Cov.spam.X

# See that the difference between both matrices is very small

sqrt(mean((Sparse.Cov.spam.X - S.spam.X)^2))

# Consider several values of lam

lam <- seq(0.1,1,by=0.1)
lam
dif.Covs <- matrix(NA,nrow=10,ncol=1)
for (i in 1:10){
  Sparse.S.spam.X <- spcov(Sigma=S.spam.X,S=S.spam.X,lambda=lam[i]*P,step.size=step.size)
  Sparse.Cov.spam.X <- Sparse.S.spam.X$Sigma
  dif.Covs[i] <- sqrt(mean((Sparse.Cov.spam.X - S.spam.X)^2))
}    
plot(lam,dif.Covs,pch=19,main="Differences between covariances",col="deepskyblue")

# Refine the search

lam <- seq(0.31,0.4,by=0.01)
lam
dif.Covs <- matrix(NA,nrow=10,ncol=1)
for (i in 1:10){
  Sparse.S.spam.X <- spcov(Sigma=S.spam.X,S=S.spam.X,lambda=lam[i]*P,step.size=step.size)
  Sparse.Cov.spam.X <- Sparse.S.spam.X$Sigma
  dif.Covs[i] <- sqrt(mean((Sparse.Cov.spam.X - S.spam.X)^2))
}    
plot(lam,dif.Covs,pch=19,main="Differences between covariances",col="deepskyblue")

# Take lam=0.33 for simplicity, although this can be refined

lam <- 0.33
Sparse.S.spam.X <- spcov(Sigma=S.spam.X,S=S.spam.X,lambda=lam*P,step.size=step.size)
Sparse.Cov.spam.X <- Sparse.S.spam.X$Sigma
Sparse.Cov.spam.X

# Sparse correlation matrix

Sparse.Cor.spam.X <- cov2cor(Sparse.Cov.spam.X)
Sparse.Cor.spam.X

par(mfrow=c(1,2))
corrplot(R.spam.X)
corrplot(Sparse.Cor.spam.X)
