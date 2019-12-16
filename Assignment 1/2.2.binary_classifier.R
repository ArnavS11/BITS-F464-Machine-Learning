# Group 4 Part 2 Question 2

# Change the working directory accordingly
setwd("/home/aashish683/Desktop/Dev/ML-Assignment")

data <- read.csv("binary_dataset.csv")

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

#using svm to apply linear margin svm
model <- svm(column_n~.,data = train_data,cost = 0.5,kernel = "linear")

# Use the model to test the testing data
test_predictions <- predict(model,test_data)

# Rounding the predictions
test_predictions <- round(test_predictions)

# Create confusion matrix (Use the column n of test data)
t <- table(test_predictions,test_data$column_n)
accuracy <- calculateAccuracy(t,length(test_predictions))
