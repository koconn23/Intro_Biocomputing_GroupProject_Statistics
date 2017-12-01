## Question 1

# Import the data set
antibiotics <- read.csv("antibiotics.csv")

# Since we're testing the difference between the presence of an antibiotic or not, this is a for-loop
# that will put a zero for control, or no antibiotic, or a 1 for antibiotic in a new column in the data table
# called 'aborct'
for (i in 1:nrow(antibiotics)){
  if (antibiotics$trt[i] == 'control'){
    antibiotics$aborct[i] <- 0
  }
  else{
    antibiotics$aborct[i] <- 1
  }
}

# These are all subsets of the data that contain the control and one of the antibiotics and the growth
# associated with each treatment
control <- antibiotics[antibiotics$trt == 'control',]
ab1 <- antibiotics[antibiotics$trt == 'control',]
ab1[5:8,] <- antibiotics[antibiotics$trt == 'ab1',]
ab2 <- antibiotics[antibiotics$trt == 'control',]
ab2[5:8,] <- antibiotics[antibiotics$trt == 'ab2',]
ab3 <- antibiotics[antibiotics$trt == 'control',]
ab3[5:8,] <- antibiotics[antibiotics$trt == 'ab3',]

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
  sig=exp(p[3])
  expected=B0+B1*x
  nll=-sum(dnorm(x=y, mean=expected, sd=sig,log=TRUE))
  return(nll)
}

# Antibiotic 1 null model and linear model results
initialGuess <- c(1,1)
fit <- optim(par=initialGuess,fn=nllnull,y=ab1$growth)
results[1,1] <- fit$value

initialGuess <- c(1,1,1)
fit <- optim(par=initialGuess,fn=nlllinear,x=ab1$aborct,y=ab1$growth)
results[1,2] <- fit$value

# Antibiotic 2 null model and linear model results 
initialGuess <- c(1,1)
fit <- optim(par=initialGuess,fn=nllnull,y=ab2$growth)
results[2,1] <- fit$value

initialGuess <- c(1,1,1)
fit <- optim(par=initialGuess,fn=nlllinear,x=ab2$aborct,y=ab2$growth)
results[2,2] <- fit$value

# Antibiotic 3 null model and linear model results
initialGuess <- c(1,1)
fit <- optim(par=initialGuess,fn=nllnull,y=ab3$growth)
results[3,1] <- fit$value

initialGuess <- c(1,1,1)
fit <- optim(par=initialGuess,fn=nlllinear,x=ab3$aborct,y=ab3$growth)
results[3,2] <- fit$value

#likelihood ratio test results
A <- 2*(results[1,1]-results[1,2])
B <- 2*(results[2,1]-results[2,2])
C <- 2*(results[3,1]-results[3,2])
results[1,3] <- pchisq(q=A, df=1, lower.tail=FALSE) 
results[2,3] <- pchisq(q=B, df=1, lower.tail=FALSE) 
results[3,3] <- pchisq(q=C, df=1, lower.tail=FALSE)
results