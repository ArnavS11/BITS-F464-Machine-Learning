# Uncomment below line to install
#install.packages("neuralnet")
library(neuralnet)


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

# Function to calculate accuracy
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
  if(x < 0.20) 
    return  (0)
  else if(x >= 0.20 && x < 0.40)
    return (1)
  else if(x >= 0.40 && x < 0.60)
    return (2)
  else if(x >= 0.60 && x < 0.80)
    return (3)
  else
    return (4)
}

softplus <- function(x) log(1 + exp(x))

# Normalising the dataset
data <- as.data.frame(lapply(data,normalise))



#Splitting the dataset
rows_for_training = sample(1:nrow(data),as.integer(0.8*nrow(data)),replace = FALSE)
train_data = data[rows_for_training,]
test_data = data[-rows_for_training,]

# Make a neural network corresponding to the dataset
model <- neuralnet(column_n~.,data = train_data,linear.output = FALSE)

# Use the model to test the testing data
test_predictions <- predict(model,test_data)

# Classifying the results
# Make the predictions fit in the range [0,4]
for(i in 1:length(test_predictions)) {
  test_predictions[i] <- fitInRange(test_predictions[i])  
}

# Create confusion matrix (Use the column n of test data)
t <- table(test_predictions,test_data$column_n)
accuracy <- calculateAccuracy(t,length(test_predictions))


