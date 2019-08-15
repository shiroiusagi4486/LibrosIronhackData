
#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&
#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&
# Week 1 - Multidimensional data (I)
#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&
#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&

##################################################################################################################
# Open R or R-studio.
##################################################################################################################

##################################################################################################################
##################################################################################################################
# Examples of well structured data sets
##################################################################################################################
##################################################################################################################

##################################################################################################################
# The Spam data set
##################################################################################################################

# Install and load the library kernlab, if it is not already installed

install.packages("kernlab")
library("kernlab")

# See the help page corresponding to the spam data set

?spam

# Load the data set into memory

data(spam)

# Obtain information about the object called spam

str(spam)

# Have a look at the first rows

head(spam)

##################################################################################################################
# The Default data set
##################################################################################################################

# Install and load the library ISLR, if it is not already installed

install.packages("ISLR")
library("ISLR")

# See the help page corresponding to the Default data set

?Default

# Load the data set into memory

data(Default)

# Obtain information about the object called Default

str(Default)

# Have a look at the first rows

head(Default)

##################################################################################################################
# The Credit data set
##################################################################################################################

# See the help page corresponding to the Credit data set

?Credit

# Load the data set into memory

data(Credit)

# Obtain information about the object called Credit

str(Credit)

# Have a look at the first rows

head(Credit)

##################################################################################################################
# The NCI60 data set
##################################################################################################################

# See the help page corresponding to the NCI60 data set

?NCI60

# Load the data set into memory

data(NCI60)

# Obtain information about the object called NCI60

str(NCI60)

# Note that NCI60 is a list of two objects: the first is called data and the second is called labs

# Try to get information about the object called data

str(NCI60$data)

# Check the dimension of the data matrix

dim(NCI60$data)

# Have a look at the first rows and columns

NCI60$data[1:10,1:20]

# Have a look at labs

NCI60$labs

##################################################################################################################
# The Canadian weather data set
##################################################################################################################

# Install and load the library fda, if it is not already installed

install.packages("fda")
library("fda")

# See the help page corresponding to the CanadianWeather data set

?CanadianWeather

# Load the data set into memory

data(CanadianWeather)

# Obtain information about the object called CanadianWeather

str(CanadianWeather)

# Note that CanadianWeather is a list of eight objects

# Focus on dailyAv

str(CanadianWeather$dailyAv)

# Have a look at the three different matrices (Temperature in ºC, Precipitation in mm, log10precipitation)

head(CanadianWeather$dailyAv[,,1])

head(CanadianWeather$dailyAv[,,2])

head(CanadianWeather$dailyAv[,,3])

# Have a look at place

CanadianWeather$place

# Have a look at province

CanadianWeather$province
CanadianWeather$province[1]

# Have a look at monthlyTemp

CanadianWeather$monthlyTemp

# Have a look at monthlyPrecip

CanadianWeather$monthlyPrecip

##################################################################################################################
##################################################################################################################
# Data matrices and indicator vectors
##################################################################################################################
##################################################################################################################

##################################################################################################################
# The Spam data set
##################################################################################################################

# Have a look at the first rows

head(spam)

# The last column contains the indicator variable, i.e., whether the email is spam or not

# Define the data matrix and the indicator vector

spam.X <- spam[,1:(ncol(spam)-1)]
head(spam.X)

spam.Y <- spam[,ncol(spam)]
head(spam.Y)

# Obtain the sample size and the dimension from the data matrix

n.spam <- nrow(spam.X)
n.spam

p.spam <- ncol(spam.X)
p.spam

##################################################################################################################
# The Default data set
##################################################################################################################

# Have a look at the first rows

head(Default)

# The first column contains the indicator variable, i.e., whether the customer default or not on their credit card debt

# Define the data matrix and the indicator vector

Default.X <- Default[,2:ncol(Default)]
head(Default.X)

Default.Y <- Default[,1]
head(Default.Y)

# Obtain the sample size and the dimension from the data matrix

n.Default <- nrow(Default.X)
n.Default

p.Default <- ncol(Default.X)
p.Default

##################################################################################################################
# The Credit data set
##################################################################################################################

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
# The NCI60 data set
##################################################################################################################

# Define the data matrix

NCI60.X <- NCI60$data

# Obtain the sample size and the dimension of the data matrix

n.NCI60 <- nrow(NCI60.X)
n.NCI60

p.NCI60 <- ncol(NCI60.X)
p.NCI60

##################################################################################################################
##################################################################################################################
# Missing data example
##################################################################################################################
##################################################################################################################

# Install and load the library nutshell, if it is not already installed

install.packages("nutshell")
library("nutshell")

# See the help page for the data set called births2006.smpl

?births2006.smpl

# Load the data set into memory

data(births2006.smpl)

# Obtain information about the object called NCI60

str(births2006.smpl)

# Have a look a it

head(births2006.smpl)

# Check that there are missing values labeled as NA

# How many NA's

colSums(is.na(births2006.smpl))

##################################################################################################################
##################################################################################################################
# Outliers example
##################################################################################################################
##################################################################################################################

# Consider the variables 34 and 38 of the NCI60 data set

plot(NCI60.X[,c(34,38)],pch=19,col="deepskyblue",xlab="Variable 34",ylab="Variable 38")

# There are a few observations that could be considered as outliers.
