library(fastAdaboost);

data <- read.csv("dataset.csv")


data <- data[complete.cases(data),]
nrow <- length(data$column_n)
datt <- cut(data$column_n, breaks=c(-1,0,4), labels=c("0","1"))
d3 <- data[,c(1,2,3,4,5,6,7,8)]



d3<-cbind(d3,datt)

#data<- transform(data,column_b = as.numeric(column_b), column_f = as.numeric(column_f), column_i = as.numeric(column_i))
#Splitting Dataset
set.seed(100)

idxs = sample(1:nrow(d3),as.integer(0.8*nrow(d3)),replace = FALSE)
traindata = d3[idxs,]
testdata = d3[-idxs,]

ada<-adaboost(datt~.,data=traindata, 10)
pred <- predict( ada, newdata=testdata)
table(pred$class,testdata$datt)
