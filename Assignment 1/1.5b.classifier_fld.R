#Uncomment to install package MASS
#install.packages("MASS")
library(MASS)

# Change the working directory accordingly
setwd("/home/aashish683/Desktop/Dev/ML-Assignment")

data <- read.csv("dataset.csv")
print(data)

#Removing the NAs in the database
data <- data[complete.cases(data),]

#Treat data as numeric
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

#Splitting the dataset
rows_for_training = sample(1:nrow(data),as.integer(0.8*nrow(data)),replace = FALSE)
train_data = data[rows_for_training,]
test_data = data[-rows_for_training,]

# Use lda to create model
model <- lda(column_n~.,data=data)

# Use the model to test the testing data
test_predictions <- predict(model,test_data)$class

# Create confusion matrix (Use the column n of test data)
t <- table(test_predictions,test_data$column_n)
accuracy <- calculateAccuracy(t,length(test_predictions))