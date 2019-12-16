# Group 4 Part 2 Question 1

# Change the working directory accordingly
setwd("/home/aashish683/Desktop/Dev/ML-Assignment")

# Reading the data
data <- read.csv("dataset.csv")

# Removing the NAs 
data<- data[complete.cases(data),]
# Extracting the nth column of the data
n_column <- data$column_n

for(i in 1:length(n_column)) {
  if(n_column[i] >= 2)
    n_column[i] = 1
}

newData <- data
newData$column_n = n_column

write.csv(newData,file="binary_dataset.csv")
