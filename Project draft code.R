#2017 Honours Project: Draft Code           **
#Author: Marvelous Mubenesha                **  
#Student No. MBNMAR005                      **
#*********************************************
#*********************************************
install.packages("dplyr")
#---------0. i) Reading Data and some manipulation plus noise reduction----------------------
setwd("C:/Users/Merveilleuse2/Desktop/Project")

#Note to self: When the Dates data behaves, use filter to subset the in-sample and out-of-sample data by the variable date

#Transposing index from row of data to column because excel doesnt work on your PC anymore
index.0 <- read.table("top40index.txt") #Top40 Price index 01/07/2012 - 30/06/2017
#Error message explained: because the data is a row in excel, its reading the price index values as
#                         un-named independent/explanatory var of 1 obs. The msg lets you know its named them V1 -v1827 

#Transpose: This can be done in one line but i did it in steps to avoid errors and for readability
#**********
index.1 <- as.matrix(index.0)  #Convert data-frame to a matrix for easy manipulation
index.2 <-(t(index.1))[2:1828,1]#Transpose the matrix then slice out non-numeric values i.e the heading
index <- as.numeric(index.2)   #Convert values that have been read as strings into numbers

#Calculating log returns from price index
#*****************************************

log.return <- NULL
for(i in 1:length(index)-1){
  
  x <- index[i]
  y <- index[i+1]
  log.return[i] <-log(y/x)
  
}

#Some plots: for graphical understanding of the data
#****************************************************
plot(index, type = "l", main = "Price Index 07/2010 - 07/2017")
lines(density(log(index), adjust = 3), col = 7, lty = 3) #Nothing is visible on the plot coz of scale

plot(log.return, type = "l", main = "Log return on Price Index 07/2010 - 07/2017")
#Looks stationary which is a good place to start for the ARIMA model :D

#----- 0. ii) Dataframe of log returns and log prices for dplyr -------------------
data <- data.frame(
  price   = log(index)[1:1826],
  returns = log.return
)   #Consider price from day one to second last day to match length of returns data. Is this ok? 
head(data)  #Just to have a look at what things look like


#------------------------1. ARIMA Modelling----------------------------------------
#Questions for Hanjo####
#i) Should I use the auto.arima or first principles? What would he like to see?
#ii) Arima forecasts over the entire period but Prophet is forecasting the next day?
#    should I write separate code that estimates next day forecasts for ARIMA on a rolling-over basis?
#iii) How to write this such that the output is not printed in markdown

#ACF
data%>%select(returns)%>%acf  #Guide for ma component ma(2)
#PACF
data%>%select(returns)%>%pacf  #Guide for ar component ar(2), ar(5), ar(30) 
                               #Looks like there could be weekly(ar(5???)) and monthly seasonality

#Fitting possible ARIMA models (model 1 - model 9)
m1 <- data%>%do(fit = arima(data$returns, order = c(2,0,0))) #This approach uses dplyr notation but it takes longer to run and we loose access to other estimation results like the residuals
m2 <- data%>%do(fit = arima(data$returns, order = c(0,0,2)))
m3 <- data%>%do(fit = arima(data$returns, order = c(0,0,5)))
#m4 <-system.time(arima(data$returns, order = c(0,0,30))) #This one takes long to run for reasons ...???
m5 <- arima(data$returns, order = c(2,0,2))
m6 <- data%>%do(fit = arima(data$returns, order = c(2,0,5)))
#m7 <- arima(data$returns, order = c(2,0,30)) #This one takes long to run for reasons...???, even longer with dplyr notation ...???


m1$fit
m2$fit
m3$fit
#m4$aic
m5$aic
m6$fit
#m7$aic

#Model with the lowest AIC is m5 ARIMA(2,0,2)

#ARIMA Modelling using forecast package: automatically chooses ####
#install.packages("forecast")
#library(forecast)
#auto.arima(data$return, approximation = F, trace = F) #This also builds the best ARIMA model and it turns out that you got it right. Its the arima(2,0,2) :)

#----Separating into in-sample and out-of-sample for forecast analysis: No need to do this because we'll compare using DM Test in your paper----

m5 <- arima(data$returns[1:550], order = c(2,0,2))
par(mfrow=c(1,2))
acf(m5$residuals, main = "Residual ACF")
pacf(m5$residuals,  main = "Residual PACF")
#The ACF and PACF of ARIMA model don't look like there are uncaptured patterns


#Forecasting ARIMA####
pred <- predict(m5, n.ahead =1826-550 )  #estimating forecasts(Log returns)

log.pred <- pred$pred #Log return forecasts
log.se <- pred$se     #Std error of log return forecasts

#Construction of confidence intervals for estimated forecasts
log.upper_bound <- log.pred + 2*log.se
log.lower_bound <- log.pred - 2*log.se

#Daily returns: Estimates and confidence interval bounds
return_pred <- exp(log.pred)
return_pred.u <- exp(log.upper_bound)
return_pred.l <- exp(log.lower_bound)


#Calculating forecasted prices(p_hat), upper bound(p_hat.u) and lower bound(p_hat.l)

#Used a for loop here. Think about how to maybe execute an apply function or even better, dplyr

#Step 1: Create empty vectors to store the forecasted prices and the bounds for the estimates
Price_hat <- NULL
Price_hat.u <- NULL
Price_hat.l <- NULL

#Step 2: Set the initial price to the price on last day of the in-sample period
P_zero <- index[1766]  #index value just before the first forecast
Price_hat[1] <- P_zero          #Starting values for each forecast vector
Price_hat.u[1] <- P_zero
Price_hat.l[1] <- P_zero

#Step 3: For loop that uses forecasted returns to calculate forecasted prices over the period
for(i in 1:length(return_pred)){
  Price_hat[1+i] <- Price_hat[i]*return_pred[i]
  Price_hat.u[1+i] <- Price_hat.u[i]*return_pred.u[i]
  Price_hat.l[1+i] <- Price_hat.l[i]*return_pred.l[i]
  
}

#Detour: Excl when sending to Hanjo. Did this for interests sake and for fun!!!!####
par(mfrow=c(1,1))
plot(index[1767:1826], type = "l", main = "Price(black) vs Forecasts(blue) with forecast bounds(red)",ylab = "Price",ylim=c(min(Price_hat.l), max(Price_hat.u)))
lines(Price_hat, col = 'blue')
lines(Price_hat.u, col = 'red')
lines(Price_hat.l,col = 'red')


#------------------------------2. Prophet Model---------------------------------------------
#install.packages("prophet")
library(prophet)

#2.1 Getting the data into a format(dataframe) required by the prophet forecasting package

# Creating a sequence of business days dates
#install.packages("bizdays")
library(bizdays)

#To create a sequence of business days,you specify the start and end dates inside the function bizseq
#Took data from the past 5 years
start_date <- as.Date("2012-07-01")
end_date <- as.Date("2014-01-01") # Should be "2017-06-30" if we use the full time period, i.e w/o in- sample and out of sample
dates <- bizseq(start_date,end_date,"actual")

prophet_data <- data.frame(ds = dates,y = log.return[1:550])  #Note to self : remove [1:1766] If forecasting using full period

#Model Estimation(log returns)
proph_fit <- prophet(df = prophet_data,yearly.seasonality = TRUE)   #facebook prophet

#Prophet Forecasts####

pred_period_dates <- make_future_dataframe(proph_fit,periods = 60) #Dates for prediction period
library("tidyr")
proph_forecast <- predict(proph_fit,pred_period_dates)  #Forecasts

#Detour######FORECAST VS ACTUAL
library(ggplot2)
plot( proph_fit, proph_forecast)
prophet_plot_components( proph_fit, proph_forecast)

#3. Diebold-Mariano Test####

#The test is found in the forecast package which was loaded when I used the auto.arima
#The DM_test requires forecast errors/residuals so first we'll calculate them for both the 
#ARIMA and Prophet model

#3.1 In sample one step forecast(1:1766)
#3.1.1 ARIMA Residuals
e_1 <- residuals(m5)[1:550]

#3.1.2 Prophet Residuals
#There is no function that automatically returns the residuals
#so we'll do this manually by subtracting the actual from the estimate(y minus y_hat)

#First we extract the in-sample forecasts from prophet model results
proph_in_sample_forecast <-  proph_forecast$yhat[1:550]

#Then we calculate the forecast errors/residuals
e_2 <- proph_in_sample_forecast - log.return[1:550]   # y_hat minus y


#3.1.3 DM Test  
dm.test(e_1,e_2)  #p = 0.6582 =>  Fail to reject the Null for in-sample

  
  
#3.2 Out of sample(1767:1826)
#3.2.1 Arima Forecast residuals
e_arima <- log.return[551:1826] - log.pred  #log.pred is the 60 periods ahead forecast using arima defined in section on Forecasting ARIMA


#3.2.2 Prophet Residuals
proph_oos_forecast <-  proph_forecast$yhat[551:1826]   #Prophet pseudo out-of-sample forecasts
e_proph <- proph_oos_forecast - log.return[551:1826]    # y_hat minus y


#3.2.3 DM Test
dm.test(e_arima, e_proph)   #Oh lord!!! My p-value is 0.9464. What does this mean? Go read!
#Sample size issues???

hist(e_proph, freq = T, main = "Prophet, h=1826-550")
hist(e_arima, freq = T, main = "Arima, h=1826-550")
hist(exp(e_proph))
hist(exp(e_arima))
?hist
#BIG QUESTION ABOUT RELIABILITY OF RESULTS: Is ARIMA doing one day ahead forecast or is it forecasting over the next 60 periods using the model?####
#if latter is true, i'll have to write iterative code that forecasts next day using history for 60 timepoints

#3.2.3 THINGS TO TRY: Playing around to see what happens when we change things
#Change from 60 to 30? or 20? which is shorter term. Do we consider a month to be 20 days(5weekdays)?
#What if you used logarithm of prices index as opposed to returns to model for prophet so trend feature can be captured?
#Widen prediction horizon but only do one day ahead prediction
