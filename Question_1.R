## Question 1

# Import the data set
antibiotics <- read.csv("antibiotics.csv")

# Since we're testing the difference between the influence of each individual antibiotic,
# we add these columns to our antibiotics data set to show the influence of ab1, 2, or 3 indicated with a 1
# while the rest of the antibiotics have a 0
ab1 <- c(0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0)
ab2 <- c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0)
ab3 <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1)
xvalues <- matrix(c(ab1,ab2,ab3),nrow=length(ab1),ncol=3)

# This is a matrix that will hold the negative log likelihood results of the null and linear models, and the
# results of the likelihood ratio test
results <- matrix(0,1,3)
colnames(results) <- c("null", "linear", "chisq")

#Null model function
nllnull <- function(p,y){
  B0=p[1]
  sig=exp(p[2])
  expected=B0
  nll=-sum(dnorm(x=y, mean=expected, sd=sig,log=TRUE))
  return(nll)
}

#Linear Model function
nlllinear <- function(p,x,y){
  B0=p[1]
  B1=p[2]
  B2=p[3]
  B3=p[4]
  sig=exp(p[5])
  expected=B0+B1*x[,1]+B2*x[,2]+B3*x[,3]
  nll=-sum(dnorm(x=y, mean=expected, sd=sig,log=TRUE))
  return(nll)
}

# I found the mean of each antibiotic and the controls to make the intial guesses closer
mean(antibiotics$growth[antibiotics$trt=='control'])
mean(antibiotics$growth[antibiotics$trt=='ab1'])
mean(antibiotics$growth[antibiotics$trt=='ab2'])
mean(antibiotics$growth[antibiotics$trt=='ab3'])

# Antibiotic null model
initialGuess <- c(20,1)
fit <- optim(par=initialGuess,fn=nllnull,y=antibiotics$growth)
results[1,1] <- fit$value

# Antibiotic 1 linear model results 
initialGuess <- c(20,4,17,8,1)
fit <- optim(par=initialGuess,fn=nlllinear,x=xvalues,y=antibiotics$growth)
results[1,2] <- fit$value

#likelihood ratio test results
A <- 2*(results[1,1]-results[1,2])
results[1,3] <- pchisq(q=A, df=1, lower.tail=FALSE) 
results
