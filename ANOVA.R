#### ANOVA ####
#2-level anova
x = c(13,13,13,13,13,13,13,13,13,13,13,13,38,38,38,38,38,38,38,38,38,38,38,38)
x_values = matrix(rep(x,10),nrow=24,ncol=10)
num_sims <- 10 # Simulate the data 10 times 
n_per_sim <- 24 # Each simulation will have 24 values
set.seed(24) # Seed is necessary for reproducibility 
sigma_num <- 8
sigma <- c(1,2,4,6,8,12,16,24)

y <- .4 * x_values + 10

outputy<-as.data.frame(matrix(0, ncol=10, nrow=24))
youtput <-list()

  for (sigma_number in 1:sigma_num){
    outputy = y + rnorm(length(y), mean=0, sd=sigma[sigma_number])
    
    youtput[[sigma_number]] <- outputy
  }




############# 4-level anova
x = c(10,10,10,10,10,10,20,20,20,20,20,20,30,30,30,30,30,30,40,40,40,40,45,45,45,45)
x_values = matrix(rep(x,10),nrow=24,ncol=10)
num_sims <- 10 # Simulate the data 10 times 
n_per_sim <- 24 # Each simulation will have 24 values
set.seed(24) # Seed is necessary for reproducibility 
sigma_num <- 8
sigma <- c(1,2,4,6,8,12,16,24)

y <- .4 * x_values + 10

outputy<-as.data.frame(matrix(0, ncol=10, nrow=24))
youtput <-list()

for (sigma_number in 1:sigma_num){
  outputy = y + rnorm(length(y), mean=0, sd=sigma[sigma_number])
  
  youtput[[sigma_number]] <- outputy
}

youtput[[3]]





############# 8-level anova
x = c(5,5,5,10,10,10,15,15,15,20,20,20,25,25,25,30,30,30,35,35,35,40,40,40)
x_values = matrix(rep(x,10),nrow=24,ncol=10)
num_sims <- 10 # Simulate the data 10 times 
n_per_sim <- 24 # Each simulation will have 24 values
set.seed(24) # Seed is necessary for reproducibility 
sigma_num <- 8
sigma <- c(1,2,4,6,8,12,16,24)

y <- .4 * x_values + 10

outputy<-as.data.frame(matrix(0, ncol=10, nrow=24))
youtput <-list()

for (sigma_number in 1:sigma_num){
  outputy = y + rnorm(length(y), mean=0, sd=sigma[sigma_number])
  
  youtput[[sigma_number]] <- outputy
}

youtput[[3]]
