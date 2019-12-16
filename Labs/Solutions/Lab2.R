# To generate the same random numbers
set.seed(20)

# First example 
x <- c(1,3,5,8)
y <- c(43,98,111,2323)

relation <- lm(y~x)

a <- data.frame(x = 9)
  
  
  
# print(summary(relation))
print(predict(relation,a))

# Second example

# Make a random predictor value set
q <- seq(from = 0, to = 20, by = 0.1)

# Prepare the actual response set
# Third order polynomial wrt the predictor variable
y <- 500 + 0.4*(q-10)^3

# Prepare a noise vector which consists of points of normal distro
noise <- rnorm(length(q),mean = 10, sd = 80)

# Add noise to the actual vector
noisy.y <- y + noise
plot(q,noisy.y,col="deepskyblue")
lines(q,y,"brickred")

#Prepare a relation between the noisy.y and the predictor q, assume you know it's a third degree relation
relationThird <- lm(noisy.y ~ poly(q,3))

#Prepare a 12 degree polynomial, should highly overfit
relationTwelve <- lm(noisy.y ~ poly(q,12))

predictedThird <- c()
predictedTwelve <- c()
for(i in 1:length(q)) {
  x <- data.frame(q = q[i])
  predictedThird <- c(predictedThird,predict(relationThird,x))
  predictedTwelve <- c(predictedTwelve,predict(relationTwelve,x))
}

lines(q,predictedThird,"purple",lwd = 3)
lines(q,predictedTwelve,"black",lwd = 1)
