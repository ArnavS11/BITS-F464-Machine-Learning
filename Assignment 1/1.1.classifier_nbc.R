# Takeaways
# NBC needs categorical classification, hence as.factor is needed in the func call, otherwise it treats it as numeric and gives 0 predicitons 


# Uncomment below line to install
# install.packages("e1071")
library(e1071)


# Change the working directory accordingly
setwd("/home/aashish683/Desktop/Dev/ML-Assignment")

data <- read.csv("dataset.csv")
print(data)

data <- data[complete.cases(data),]

data <- as.data.frame(lapply(data,as.factor))

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

# Use naiveBayes with Laplace smoothing to create model
model <- naiveBayes(as.factor(column_n) ~ .,data = train_data,laplace = 1)

# Use the model to test the testing data
test_predictions <- predict(model,test_data)

# Create confusion matrix (Use the column n of test data)
t <- table(test_predictions,test_data$column_n)
accuracy <- calculateAccuracy(t,length(test_predictions))

