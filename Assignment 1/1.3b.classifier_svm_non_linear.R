# Change the working directory accordingly
setwd("/home/aashish683/Desktop/Dev/ML-Assignment")

data <- read.csv("dataset.csv")
print(data)

# Removing rows having NA
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

fitInRange <- function(x) {
  if(x < 1) 
    return  (0)
  else if(x >= 1 && x < 2)
    return (1)
  else if(x >= 2 && x < 3)
    return (2)
  else if(x >= 3 && x < 4)
    return (3)
  else
    return (4)
}


#Splitting the dataset
rows_for_training = sample(1:nrow(data),as.integer(0.8*nrow(data)),replace = FALSE)
train_data = data[rows_for_training,]
test_data = data[-rows_for_training,]


#using svm to apply non linear margin svm
model <- svm(column_n~.,data = train_data,cost = 0.5,kernel = "polynomial")

# Use the model to test the testing data
test_predictions <- predict(model,test_data)

# Make the predictions fit in the range [0,4]
for(i in 1:length(test_predictions)) {
  test_predictions[i] <- fitInRange(test_predictions[i])  
}



# Create confusion matrix (Use the column n of test data)
t <- table(test_predictions,test_data$column_n)
accuracy <- calculateAccuracy(t,length(test_predictions))