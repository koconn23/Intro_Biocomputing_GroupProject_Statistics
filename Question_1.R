## Question 1

# Import the data set
antibiotics <- read.csv("antibiotics.csv")

# Since we're testing the difference between the influence of each individual antibiotic
# 
antibiotics$ab1 <- c(0,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0)
antibiotics$ab2 <- c(0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0)
antibiotics$ab3 <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1)


# This is a matrix that will hold the negative log likelihood results of the null and linear models, and the
# results of the likelihood ratio test
results <- matrix(0,3,3)
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
  expected=B0+B1*x[5:8]+B2*x[9:12]+B3*x[13:16]
  nll=-sum(dnorm(x=y, mean=expected, sd=sig,log=TRUE))
  return(nll)
}

# Antibiotic null model
initialGuess <- c(1,1)
fit <- optim(par=initialGuess,fn=nllnull,y=antibiotics$growth)
results[1,1] <- fit$value
results[2,1] <- fit$value
results[3,1] <- fit$value

# Antibiotic 1 linear model results 
initialGuess <- c(1,1,1,1,1)
fit <- optim(par=initialGuess,fn=nlllinear,x=antibiotics$ab1,y=antibiotics$growth)
results[1,2] <- fit$value

# Antibiotic 2 linear model results 
initialGuess <- c(1,1,1,1,1)
fit <- optim(par=initialGuess,fn=nlllinear,x=antibiotics$ab2,y=antibiotics$growth)
results[2,2] <- fit$value

# Antibiotic 3 linear model results
initialGuess <- c(1,1,1,1,1)
fit <- optim(par=initialGuess,fn=nlllinear,x=antibiotics$ab3,y=antibiotics$growth)
results[3,2] <- fit$value

#likelihood ratio test results
A <- 2*(results[1,1]-results[1,2])
B <- 2*(results[2,1]-results[2,2])
C <- 2*(results[3,1]-results[3,2])
results[1,3] <- pchisq(q=A, df=1, lower.tail=FALSE) 
results[2,3] <- pchisq(q=B, df=1, lower.tail=FALSE) 
results[3,3] <- pchisq(q=C, df=1, lower.tail=FALSE)
results
