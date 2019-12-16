# Part 2 Question 3

#Uncomment to install package
#install.packages("ggplot2")
#install.packages("gtools")
library(ggplot2)
library(gtools)
# Change the working directory accordingly
setwd("/home/aashish683/Desktop/Dev/ML-Assignment")

# Reading the data
data <- read.csv("binary_dataset.csv")

# Remove NAs
data <- data[complete.cases(data),]

#Filter out the rows which are only 1
data <- data[data$column_n == 1,]

# Treat data as numeric
data <- transform(data,column_b = as.numeric(column_b), column_f = as.numeric(column_f), column_i = as.numeric(column_i))

calculateAccuracy <- function(t,total_objects) {
  rows <- nrow(t)
  count <- 0
  for(i in 1:rows) {
    if(i <= ncol(t))
      count = count + t[i,i]
  }
  
  return ((as.double(count)*100)/total_objects)
}

# Apply kmeans on the data
model <- kmeans(data,centers = 4,nstart = 100)

#Get data from the original dataset
origData <- read.csv("dataset.csv")
#Remove NAs
origData <- origData[complete.cases(origData),]
#Filter out only those columns which are not zero
origData <- origData[origData$column_n != 0,]

#Create confusion matrix
t <- table(model$cluster,origData$column_n)
accuracy <- calculateAccuracy(t,length(origData$column_n))

#Permuting each cluster with 1:4 and finding which assignment yields maximum accuracy
perms <- permutations(n=4,r=4,v=1:4)
accuracyMax <- 0
tablMax <- 0
ta <- table(model$cluster,origData$column_n)
for(i in (1:nrow(perms))) {
  newCluster <- c(t[perms[i,1],],t[perms[i,2],],t[perms[i,3],],t[perms[i,4],])
  row1 <- t[perms[i,1],]
  row2 <- t[perms[i,2],]
  row3 <- t[perms[i,3],]
  row4 <- t[perms[i,4],]
  ta[1,] = row1
  ta[2,] = row2
  ta[3,] = row3
  ta[4,] = row4
  
  accuracy <- calculateAccuracy(ta,length(origData$column_n))
  
  if(accuracy > accuracyMax) {
    accuracyMax = accuracy
    tableMax <- ta
  }
}


