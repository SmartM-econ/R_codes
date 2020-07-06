data(gpa1, package='wooldridge')

# Just obtain parameter estimates:
lm(colGPA ~ hsGPA+ACT, data=gpa1)

# Store results under "GPAres" and display full table:
GPAres <- lm(colGPA ~ hsGPA+ACT, data=gpa1) #data option!
s=summary(GPAres)

hist(s$residuals)
confint(GPAres) # 95%
confint(GPAres,level=.99) # 99%
myH0 <- c("ACT")
linearHypothesis(GPAres, myH0)

### Benchmark parallel case
# Note: We'll have to set the sample size first, e.g. by uncommenting:
# n <- 100
# Set the random seed
library(tictoc)
tic()
set.seed(1234567)
# set true parameters: intercept & slope 
b0 <- 1; b1 <- 0.5
# initialize b1hat to store 10000 results:
n<-1000
b1hat <- numeric(10000)
n<-length(b1hat)
# Draw a sample of x, fixed over replications:
x <- rnorm(n,4,1)


# repeat r times:
for(j in 1:10000) {
  # Draw a sample of u (std. normal):
  # Draw a sample of y:
  u<-rnorm(n)
  y <- b0 + b1*x + u
  
  # regress y on x and store slope estimate at position j
  bhat <- coef( lm(y~x) )
  b1hat[j] <- bhat["x"]
}
toc()

#### parallel ver. save 50% time!!!!!!!!!!

b0 <- 1; b1 <- 0.5
# initialize b1hat to store 10000 results:
n<-1000
b1hat <- numeric(10000)
n<-length(b1hat)
# Draw a sample of x, fixed over replications:
x <- rnorm(n,4,1)

library(foreach)
library(doParallel)
library(tictoc)
#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]) #not to overload your computer
registerDoParallel(cl)
tic()
final <- foreach(j=1:10000,.combine=cbind) %dopar% {
  u<-rnorm(n)
  y <- b0 + b1*x + u
  # regress y on x and store slope estimate at position j
  bhat <- coef( lm(y~x) )
  b1hat[j] <- bhat["x"]
  #do other things if you want
  
}
#stop cluster
stopCluster(cl)

toc()
