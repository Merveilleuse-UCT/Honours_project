library(forecast)

fit1 <- rwf(EuStockMarkets[1:200,1],h=100)
fit2 <- meanf(EuStockMarkets[1:200,1],h=100)
plot(fit1)

xx<- accuracy(fit1, EuStockMarkets[200:299,1])
plot(xx)
