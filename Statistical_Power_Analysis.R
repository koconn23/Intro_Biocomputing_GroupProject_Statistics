#### A statistical power analysis ####
library(ggplot2)

num_sims <- 10 # Simulate the data 10 times 
n_per_sim <- 24 # Each simulation will have 24 values
set.seed(24) # Seed is necessary for reproducibility 
sigma_num <- 8
sigma <- c(1,2,4,6,8,12,16,24)

matrix_outputx <- array(NA, dim=c(num_sims, n_per_sim, sigma_num), dimnames = NULL)  # This creates a 10x24 matrix for the simulated x-values
matrix_outputy <- array(NA, dim=c(num_sims, n_per_sim, sigma_num), dimnames = NULL) # This creates a 10x24 matrix for the simulated y-values

outputx<-data.frame(0,ncol=n_per_sim,nrow=num_sims)
outputy<-data.frame(0,ncol=n_per_sim,nrow=num_sims)

for (sim_number in 1:num_sims){ # This starts your for loop
  x <- sample(x = 0:50, size = 24) # Creates a random generation of 24 x-values between 0 and 50 
  m <- 0.4 # Given slope value
  b <- 10 # Given y-intercept
  y <- m*x + b # Calculates y-values from the 24 randomly generated x-values using given slope and intercept 
 
for (sigma_number in length(sigma)){
   y = y+rnorm(x, mean=0, sd=sigma[sigma_number])
   outputx<-data.frame(x,ncol=n_per_sim,nrow=num_sims)
   outputy<-data.frame(y,ncol=n_per_sim,nrow=num_sims)
   xoutput<-list(outputx[sigma_number])
   youtput<-list(outputy[sigma_number])
    # This is where each y output is stored for each simulation
  }
  
}
matrix_outputx # Shows the output for the 10 x 24 matrix for the simulated x-values
matrix_outputy # Shows the output for the 10 x 24 matrix for the simulated y-values


matrix_outputx[sim_number,,sigma_number] # This is where each x output is stored for each simulation
matrix_outputy[sim_number,1:24,sigma_number]
#### Linear Regression ####

#### ANOVA ####