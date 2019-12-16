# Change the working directory accordingly
setwd("/home/aashish683/Desktop/Dev/ML-Assignment")
install.packages("binr")
library(binr)

data <- read.csv("dataset.csv")
print(data)

# Convert numerical to factor
covert_to_factor <- function(arr){
  bins_col <- bins.quantiles(arr,5,5, verbose = FALSE)
  print(bins.getvals(bins_col))
  col_factors <- cut(arr,bins.getvals(bins_col),labels= c(0,1,2,3,4) )
  col_factors
}
#adehj
data$column_a <-covert_to_factor(data$column_a)
data$column_d <-covert_to_factor(data$column_d)
data$column_e <-covert_to_factor(data$column_e)
data$column_h <-covert_to_factor(data$column_h)
data$column_j <-covert_to_factor(data$column_j)


# Removing rows having NA
data <- data[complete.cases(data),]

n_column <- data$column_n

for(i in 1:length(n_column)) {
  if(n_column[i] >= 2)
    n_column[i] = 1
}

newData <- data
newData$column_o = n_column
#newData

ndata <- data
ndata$column_n = n_column
#ndata


calculateAccuracy <- function(t,total_objects) {
  rows <- nrow(t)
  count <- 0
  for(i in 1:rows) {
    if(i <= ncol(t)) {
      count = count + t[i,i]
    }
  }
  
  return ((as.double(count)*100)/total_objects)
}

#Splitting the dataset
rows_for_training = sample(1:nrow(ndata),as.integer(0.8*nrow(ndata)),replace = FALSE)
train_data = ndata[rows_for_training,]
test_data = ndata[-rows_for_training,]

test_data2=newData[-rows_for_training,]
test_data2
#install.packages("smbinning")
#library(smbinning)


#Using glm to apply Logistic Regression
model <- glm(column_n~.,family = binomial,data = train_data)

# Use the model to test the testing data
#test_predictions <- predict(model,test_data)
#test_predictions
tt_preci <- predict(model,test_data,type="response")
tt_preci
typeof(tt_preci)

for(i in 1:length(tt_preci)){
  if (tt_preci[i]<.2) {
    tt_preci[i]=0
  } else if (tt_preci[i]<.4){
    tt_preci[i]=1
} else if (tt_preci[i]<.6)
{  tt_preci[i]=2
}
else if (tt_preci[i]<.8)
 { tt_preci[i]=3
}
  else if (tt_preci[i]<1)
  { tt_preci[i]=4
  }
}  
tt_preci
# Rounding the predictions
test_predictions <- round(test_predictions)
test_data2
# Create confusion matrix (Use the column n of test data)
t <- table(test_predictions,test_data2$column_n)
t2 <- table(tt_preci,test_data2$column_n)
t2
accuracy<- calculateAccuracy(t2,length(tt_preci))
data

