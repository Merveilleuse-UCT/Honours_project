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

#Test
p_vals <-repeat.dm.bootstrap(e_1, e_2, 10000, h = 1)  #Check if seed for rand.int is changing as a control measure 
hist(p_vals)


####Delete this later
#Data

#log. returns for ARIMA
index.0 <- read.table("top40index.txt")
index.1 <- as.matrix(index.0)  
index.2 <-(t(index.1))[2:1828,1]
index <- as.numeric(index.2)

log.return <- NULL
for(i in 1:length(index)-1){
  
  x <- index[i]
  y <- index[i+1]
  log.return[i] <-log(y/x)
  
}

#data-frame for prophet package
start_date <- as.Date("2012-07-01")
end_date <- as.Date("2014-01-01") # Should be "2017-06-30" if we use the full time period, i.e w/o in- sample and out of sample
dates <- bizseq(start_date,end_date,"actual")
prophet_data <- data.frame(ds = dates,y = log.return[1:550])  #Note to self : remove [1:1766] If forecasting using full period


#Model estimation
library(forecast)
arima.fit <- auto.arima(data$return, approximation = F, trace = F) #This also builds the best ARIMA model and it turns out that you got it right. Its the arima(2,0,2) :)
prophet.fit <- prophet(df = prophet_data,yearly.seasonality = TRUE) 


#Model Forecasts and residuals
#ARIMA
start_date <- 1766   #???
end_date <- 1825      # ???
period <- 1
arima_residuals <- list()
j = 1
for(i in seq(start_date, end_date, period)){
  arima_model <- arima(data$returns[1:i], order = c(2,0,2), 
                       optim.control = list(maxit = 1000))
  arima_pred <- predict(arima_model, n.ahead = 5, newdata = data$returns[(i+1):(i+5)])
  arima_error <- arima_pred$pred - data$returns[(i+1):(i+5)]
  arima_residuals[[j]] <- data.frame(ret = data$returns[(i+1):(i+5)], 
                                     pred = as.numeric(arima_pred$pred),
                                     error = as.numeric(arima_error))
  j <- j + 1
}
do.call(rbind, arima_residuals)
arima_five_res <- do.call(rbind, arima_residuals)

plot(arima_five_res$ret, type = "l")
lines(arima_five_res$pred, col = 2)

#Prophet
pred_period_dates <- make_future_dataframe(proph_fit,periods = 1826-550) #Dates for prediction period
library("tidyr")
proph_forecast <- predict(proph_fit,pred_period_dates) 
proph_in_sample_forecast <-  proph_forecast$yhat[1:550]
prophet_residuals <- proph_in_sample_forecast - log.return[1:550]


  #Bootstrap

e1 <- arima_five_res$error  #Qn: There's 300 of them, how to I control this? Why are there NA's?

e2 <- prophet_residuals[1:300]   #Truncated this so the length of the matrices match

p_vals <-repeat.dm.bootstrap(e1, e1, 1000, h = 1)  #Check if seed for rand.int is changing as a control measure 
hist(p_vals)






