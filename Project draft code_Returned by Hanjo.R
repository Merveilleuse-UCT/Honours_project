#2017 Honours Project: Draft Code           **
#Author: Marvelous Mubenesha                **  
#Student No. MBNMAR005                      **
#*********************************************
#*********************************************

#setwd("~/Honours_project")

#---------0. i) import data----------------------------------------------------------------------

#library(readxl)
top40index <- read_excel("~/Honours_project/top40index.xlsx", 
                         col_types = c("date", "numeric"))

#Data cleaning : removing duplicates
index <- top40index[1:1827,]  #warning message is because data is being duplicated repeatedly. Duplicates start after row 1827
attach(index)
#head(data) 

#Calculating returns and log returns from price index
#*****************************************************
#library(PerformanceAnalytics)
returns <- CalculateReturns(as.ts(index[,2]))[-1]
log.returns <- CalculateReturns(as.ts(index[,2]), method = "log")[-1]

#Exploratory data analysis
#**************************
#Outliers
#boxplot(returns, log.returns, names = c("returns","log returns"), main = "Boxplots of Returns and Log Returns on full sample")  #Comment : looks like there are some outliers eventhough outliers are 'relatively' clustered

#Basic plots for returns and log returns

#par(mfrow = c(1,2))
#plot(returns, type = "l", main = "Returns on Price Index 07/2010 - 07/2017")
#plot(density(returns, adjust = 3), main = "Distribution of Returns on Price Index 07/2010 - 07/2017",col = 2, lty = 3) 

#plot(log.returns, type = "l", main = "Log return on Price Index 07/2010 - 07/2017")
#plot(density(log.returns, adjust = 3), main = "Distribution of Returns on Price Index 07/2010 - 07/2017", col = 2, lty = 3) 
#TO THINK ABOUT: CONSIDER SUPERIMPOSING THE STANDARD NORMAL DENSITY ONTO THE DENSITY PLOTS FOR CONTEXTUAL INTEPRETATION

#Further Data Cleaning: Formal outlier tests for time series
#************************************************************
library(tsoutliers)  # conducts multiple tests for outliers #Reference Chen and Liu;s as authors : https://stats.stackexchange.com/questions/104882/detecting-outliers-in-time-series-ls-ao-tc-using-tsoutliers-package-in-r-how
library(ggpubr)
returns.outlier.detection <- tsoutliers::tso(as.ts(returns), types = c("AO","IO","LS","TC","SLS"), maxit.iloop = 10)
log.returns.outlier.detection <- tsoutliers::tso(as.ts(log.returns),types = c("AO","IO","LS","TC","SLS"), maxit.iloop = 10)

returns.outlier.detection  # two additive outliers which appear in log return as well
log.return.outlier.detection  #Several outliers detected
#Further investigation shows that these outliers occur when return large in abs value. See : log.returns[c(299,302,348,1144,1156,1540)]

library(ggplot2)
dates <- index[2:1827,1]
data.ggplot <- as.data.frame(cbind(dates,returns,log.returns))
head(data.ggplot)

dev.off()  #need to do this for ggplot to work
ggplot(data = data.ggplot,aes(date, returns))+geom_point(na.rm = TRUE)
#Try to colour the outliers in a different colour

#Looking for the outliers? they occured on these dates : data.ggplot[c(299,302,348,1144,1156,1540),]
#How I'm dealing with them? Keep and move on, they're part of what happend and ideally, we'd like our forecasting model to adapt to such?????


#Comments
#first plots look stationary which is a good place to start for the ARIMA model :D
#second plots look like dbn of log return is standard normal :). Formal tests will be done
#____________________________________________________________________



#----- 0. ii) Dataframe of log returns and log prices for dplyr -------------------



#------------------------1. ARIMA Modelling----------------------------------------
#Questions for Hanjo####
#i) Should I use the auto.arima or first principles? What would he like to see?
## Hanjo - I would recommend using the first principle Box-Jenkins method. Your research question is concerned around whether automated forecasting algorithms can in fact predict just as well/better than conventional methods

#ii) Arima forecasts over the entire period but Prophet is forecasting the next day?
#    should I write separate code that estimates next day forecasts for ARIMA on a rolling-over basis?

## Hanjo - what you would like to do is compare the forecast horizons with each other - 1, 5, 20 days. How you would do this is fit the model on x_t data points and then use predict() to forecast 1-day, 5-days, 20-days ahead. See ?arima for examples at the bottom####

#iii) How to write this such that the output is not printed in markdown
## Hanjo - in the chunk, {r, echo = F, message = F} or right at the beginning of your document:
# ```{r, include = F}
# knitr::opts_chunk$set(echo = F, message = F)
# ```
#Partitioning data into in-sample and out-of-sample data
in.samp.returns <- returns[1:1566] #So that the out-of-sample period is a year from 01/08/2016 - 31/07/2017
in.samp.log.returns<- log.returns[1:1566]  
in.samp.dates <- index[1:1566,1]   #index is the main imported dataframe

oos.returns <- returns[1567:1826]
oos.log.returns <- log.returns[1567:1826]
oos.dates <- index[1567:1826,1]

#Box-Jenkins Methodology

#Step 1: Formally testing stationarity/Unit root tests
library(tseries)   #for Augmented Dickey-Fuller test
adf.test(in.samp.returns) 
adf.test(in.samp.log.returns) # both have p < 0.01 therefore we reject the null. Data is stationary 

#Step 2: ACF and PACF plots for model identification
#Returns
acf(in.samp.returns, main = "RETURNS") #ma(2)
pacf(in.samp.returns, main = "RETURNS") #ar(2), ar(30)

#Log returns
acf(in.samp.log.returns, main = "LOG RETURNS")  #ma(2)
pacf(in.samp.log.returns, main = "LOG RETURNS")  #ar(2), ar(30)

#ma and ar(30) could be overfitting and since model is for forecasting, I want it to be parsimonious ->
#looks like there could be seasonality but its below the bandwith 

#Step 3: Fitting possible ARIMA models (model 1 - model 9)

#Returns
m1 <- arima(in.samp.returns, order = c(0,0,2))
m2 <- arima(in.samp.returns, order = c(2,0,0))
m3 <- arima(in.samp.returns, order = c(2,0,2))

#w/ ar(30)
m4 <- arima(in.samp.returns, order = c(30,0,0))
m5 <- arima(in.samp.returns, order = c(30,0,2))
#####stopped here

m1$fit
m2$fit
m3$fit
#m4$aic
m5$aic
m6$fit
#m7$aic

# Model with the lowest AIC is m5 ARIMA(2,0,2)

#ARIMA Modelling using forecast package: automatically chooses lowest AIC model####
#install.packages("forecast")
#library(forecast)
#auto.arima(data$return, approximation = F, trace = F) #This also builds the best ARIMA model and it turns out that you got it right. Its the arima(2,0,2) :)
## Hanjo - its always smart using the auto.arima to check whether you got it right ;-)


#----Separating into in-sample and out-of-sample for forecast analysis: No need to do this because we'll compare using DM Test in your paper----

## Hanjo - you might want to test your forecasts over a longer period, say 365(instead of 60). I also increase number of itterations for the optimizations
library(lmtest)
m5 <- arima(data$returns[1:1766], order = c(2,0,2), 
            optim.control = list(maxit = 1000))
tsdiag(m5)
coeftest(m5)

par(mfrow=c(1,2))
acf(m5$residuals, main = "Residual ACF")
pacf(m5$residuals,  main = "Residual PACF")
#The ACF and PACF of ARIMA model don't look like there are uncaptured patterns

## Hanjo - I will now predict x_{t+1} for our out-of-sample period. The residuals here will be used in the DM test
start_date <- 1766 
end_date <- 1825
arima_residuals <- list()
j = 1
for(i in start_date:end_date){
  arima_model <- arima(data$returns[1:i], order = c(2,0,2), 
                       optim.control = list(maxit = 1000))
  arima_pred <- predict(arima_model, n.ahead = 1)
  arima_error <- arima_pred$pred - data$returns[i+1]
  arima_residuals[[j]] <- data.frame(ret = data$returns[i+1], 
                                     pred = as.numeric(arima_pred$pred),
                                     error = as.numeric(arima_error))
  j <- j + 1
}
do.call(rbind, arima_residuals)
arima_one_res <- do.call(rbind, arima_residuals)

plot(arima_one_res$ret, type = "l")
lines(arima_one_res$pred, col = 4)

## Hanjo - I will now predict x_{t+5} for our out-of-sample period. The residuals here will be used in the DM test

start_date <- 1766 
end_date <- 1825
period <- 5
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
# ----------------------------

#Forecasting ARIMA####
pred <- predict(m5, n.ahead = 60)  #estimating forecasts(Log returns)

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
end_date <- as.Date("2017-05-01") # Should be "2017-06-30" if we use the full time period, i.e w/o in- sample and out of sample
dates <- bizseq(start_date,end_date, "actual")

prophet_data <- data.frame(ds = dates,y = log.return[1:1766])  #Note to self : remove [1:1766] If forecasting using full period

#Model Estimation(log returns)
proph_fit <- prophet(df = prophet_data,yearly.seasonality = TRUE)   #facebook prophet

#Prophet Forecasts####
pred_period_dates <- make_future_dataframe(proph_fit, periods = 60) #Dates for prediction period

proph_forecast <- predict(proph_fit, pred_period_dates)  #Forecasts

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
e_1 <- residuals(m5)

#3.1.2 Prophet Residuals
#There is no function that automatically returns the residuals
#so we'll do this manually by subtracting the actual from the estimate(y minus y_hat)

#First we extract the in-sample forecasts from prophet model results
proph_in_sample_forecast <-  proph_forecast$yhat[1:1766]

#Then we calculate the forecast errors/residuals
e_2 <- proph_in_sample_forecast - log.return[1:1766]   # y_hat minus y

#3.1.3 DM Test  
dm.test(e_1,e_2)  #p = 0.6582 =>  Fail to reject the Null for in-sample



#3.2 Out of sample(1767:1826)
#3.2.1 Arima Forecast residuals
e_arima <- log.return[1767:1826] - log.pred  #log.pred is the 60 periods ahead forecast using arima defined in section on Forecasting ARIMA


#3.2.2 Prophet Residuals
proph_oos_forecast <-  proph_forecast$yhat[1767:1826]   #Prophet pseudo out-of-sample forecasts
e_proph <- proph_oos_forecast - log.return[1767:1826]    # y_hat minus y


#3.2.3 DM Test
dm.test(e_arima, e_proph)   #Oh lord!!! My p-value is 0.9464. What does this mean? Go read!



#BIG QUESTION ABOUT RELIABILITY OF RESULTS: Is ARIMA doing one day ahead forecast or is it forecasting over the next 60 periods using the model?####
#if latter is true, i'll have to write iterative code that forecasts next day using history for 60 timepoints 
## Hanjo - I showed you how we can do this at line 148. Thank you

#3.2.3 THINGS TO TRY: Playing around to see what happens when we change things
#Change from 60 to 30? or 20? Do we consider a month to be 20 days(5weekdays)?
#What if you used logarithm of price index as opposed to returns to model for prophet so trend feature can be captured?
## Hanjo - might be interesting
#Widen prediction horizon(nf=100,500,1000) but only do one day ahead prediction?? or maybe vary this too(h=1,2,...,10,30,60)
