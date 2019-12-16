# Change the working directory accordingly
setwd("/home/aashish683/Desktop/Dev/ML-Assignment")

#Reading the data
data <- read.csv("dataset.csv")

# Remove NAs
data <- data[complete.cases(data),]

# Treat data as numeric
data <- transform(data,column_b = as.numeric(column_b), column_f = as.numeric(column_f), column_i = as.numeric(column_i))


# Function to normalise column values
normalise <- function(x) {
  return (x - min(x))/(max(x)-min(x))
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


calculateAccuracy <- function(t,total_objects) {
  rows <- nrow(t)
  count <- 0
  for(i in 1:rows) {
    if(i <= ncol(t))
      count = count + t[i,i]
  }
  
  return ((as.double(count)*100)/total_objects)
}

# Normalising the dataset
data <- as.data.frame(lapply(data,normalise))


#Splitting the dataset
rows_for_training = sample(1:nrow(data),as.integer(0.8*nrow(data)),replace = FALSE)
train_data = data[rows_for_training,]
test_data = data[-rows_for_training,]

# Use linear regression to get the best fit line
model <- lm(column_n~.,data = train_data)

# Use the model to test the testing data
test_predictions <- predict(model,test_data)

# Make the predictions fit in the range [0,4]
for(i in 1:length(test_predictions)) {
  test_predictions[i] <- fitInRange(test_predictions[i])  
}

# Create confusion matrix (Use the column n of test data)
t <- table(test_predictions,test_data$column_n)
accuracy <- calculateAccuracy(t,length(test_predictions))