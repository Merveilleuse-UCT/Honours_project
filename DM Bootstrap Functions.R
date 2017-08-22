#Dummy data
#set.seed(1)
#e1 <- rnorm(10, mean = 0, sd = 1)
#set.seed(2)
#e2 <- rnorm(10, mean = 0, sd = 1) 

# FUNCTION 1 ; Do a dm test on bootstrapped sample

dm.bootstrap <- function(e1,e2, h, seed){
  # WHAT THE FUNCTION DOES
  
  #     This function takes two error vectors which are time indexed, then constructs a bootstrap sample 
  #     (indentical rows, with replacement) from both error vectors . The time index of error vectors is 
  #     matched by setting the same seed for each round of sampling using set.seed(seed). [See code below]
  
  #ARGUMENTS
  #     - e1 and e2 are error vectors from which we will be resampling from
  #     - h is a parameter that represents the forecasting time horizon for the error vectors
  #     - seed is a random integer that will be used as an arg. for set.seed so sampling from error vectors 
  #       is matched.
  
  #VALUE
  # Function returns the p.val when Diebold-Mariano test is conducted for bootstrapped sample errors
  
  #Step1: Select random integer to be the seed for sampling
  #seed <- round(runif(1, min = 1, max = 10000))  #This isn't changing when I re-run the code. I'll set the see outside the function
  
  #Step2: Sampling from error vectors(matching of time series indices are preserved using the random seed above)
  #Only continue with test if error vectors have the same length
  
  l1 <- length(e1)
  l2 <- length(e2)
  
  if(l1==l2){
    
    set.seed(seed)
    boot_e1 <- sample(e1, l1, replace = TRUE)
    set.seed(seed)
    boot_e2 <- sample(e2, l2, replace = TRUE)
    
  } else {
    
    print("Error vectors do not have the same length")
    
  }
  
  test <- dm.test(boot_e1, boot_e2, h = h,  alternative = c("two.sided"))  #Need to specify the forecast horizon(h), default is 1
  return(c(test$p.value))
  
}

# FUNCTION 2: Repeated the dm test on a bootstrap sample N times (uses function 1)
#Question: Think of a more efficient way to do this using dplyr

repeat.dm.bootstrap <- function(e1,e2,N,h){
  
  #   Conducts N DM Test(forecast horizon h) on a bootstrapped sample from e1 and e2
  #   Returns a vector of N p.values from the bootstrap dm tests
  
  library(forecast) #Needed by dm.bootstrap func. to do Diebold-Mariano Test
  
  p.values <- NULL
  for(i in 1:N){
    
    rand.int <- sample.int(1000, 1)
    p.values[i] <- dm.bootstrap(e1, e2, h, rand.int)
    
  }
  
  return(p.values)
}

  
p_vals <-repeat.dm.bootstrap(e1, e2, 1000, h = 1)  #Check if seed for rand.int is changing as a control measure 








