# Change the working directory accordingly
# Uncomment below line to install package
# install.packages("rpart")
library(rpart)

setwd("/home/aashish683/Desktop/Dev/ML-Assignment")


data <- read.csv("dataset.csv")

# Removing the NAs
data <- data[complete.cases(data),]

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

# Use rpart to create decision tree
model <- rpart(as.factor(column_n)~.,train_data,method="class")

# Use the decision tree to test the testing data
test_predictions <- predict(model,test_data,type="class")

# Create confusion matrix (Use the column n of test data)
t <- table(test_predictions,test_data$column_n)
accuracy <- calculateAccuracy(t,length(test_predictions))
