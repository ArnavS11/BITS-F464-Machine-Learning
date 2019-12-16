# To generate the same pseudo-randoms if required
set.seed(20)
 
# Note - mtcars is of type data.frame, not a vector or a table
#1 mtcars[,1] , mtcars[,2] will return a vector
#2 mtcars[1,], mtcars[2,] will return data.frame objects
#3 Any other type of operation except #1 will produce data.frame objects again
#Dropping the two categorical features
d01 <- mtcars[,c(1:7,10,11)]

plot(d01)

d01.pca <- princomp(d01,cor=TRUE,scores=TRUE)

summary(d01.pca)

plot(d01.pca)

d02 <- d01.pca$scores