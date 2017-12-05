#### A statistical power analysis ####
library(ggplot2)

num_sims <- 10 # Simulate the data 10 times 
n_per_sim <- 24 # Each simulation will have 24 values
set.seed(24) # Seed is necessary for reproducibility 
sigma_num <- 8
sigma <- c(1,2,4,6,8,12,16,24)

outputx<-as.data.frame(matrix(0, ncol=10, nrow=24))
outputy<-as.data.frame(matrix(0, ncol=10, nrow=24))
youtput <-list()

for (sim_number in 1:num_sims){ # This starts your for loop
  x = sample(x = 0:50, size = 24) # Creates a random generation of 24 x-values between 0 and 50 
  outputx[,sim_number] <- x
  m <- 0.4 # Given slope value
  b <- 10 # Given y-intercept
  y = m*x + b # Calculates y-values from the 24 randomly generated x-values using given slope and intercept 
  outputy[,sim_number] <- y
}
outputy
  for (sigma_number in 1:sigma_num){
   outputy[,sim_number] = outputy[,sim_number]+rnorm(length(outputy[,sim_number]), mean=0, sd=sigma[sigma_number])
   youtput[[sigma_number]] <- outputy
  }

youtput[[1]]

#### Linear Regression ####

#### ANOVA ####