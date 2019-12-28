# Check https://towardsdatascience.com/stock-market-forecasting-using-time-series-c3d21f2dd37f

# Install and initialize all libraries
list.of.packages <- c("tidyverse", "forecast", "dplyr","astsa", "xts")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages, require, character.only = TRUE)

# Data taken from https://finance.yahoo.com/quote/DIS/history?p=DIS
dis <- read.csv('./DIS.csv')
dis <- dis %>% select(1,5)
dis$Date <- as.Date(dis$Date, formate='%Y/%m/%d')
dis <- xts(dis$Close, order.by = dis$Date)
dis <- head(dis,-1)

# Plot the time series
plot(dis)

# ACF & PACF (Seems like a non-stationary AR(1) model)
par(mfrow=c(2,1))
dis_acf <- acf(dis$Close, plot=FALSE)
plot(dis_acf, main='ACF of the dis Stock')
dis_pacf <- pacf(dis$Close, plot=FALSE)
plot(dis_pacf, main='PACF of the dis Stock')

# Box-Ljung test (We can no)
Box.test(dis$Close, type="Ljung")

# Create train and test split
dis_test <- last(dis,'12 month')
dis_train <- window(dis, start=index(first(dis)), end=index(first(dis_test)) - 1)

# ACF & PACF of the train set
acf2(dis_test)

# Predict the next 12 months
dis_pred <- sarima.for(dis_train, 12,1,0,0)


inter <- data.frame(low, high)

inter[,(ncol(inter)+1)] = (coredata(dis_test) > inter$low & coredata(dis_test) < inter$high)[,1]
inter[,(ncol(inter)+1)] = coredata(dis_test)
inter[,(ncol(inter)+1)] = pred$pred
inter[,(ncol(inter)+1)] = abs(coredata(dis_test) - pred$pred)/coredata(dis_test)

colnames(inter)[3] = "Contains"
colnames(inter)[4] = "Observed value"
colnames(inter)[5] = "Forecast"
colnames(inter)[6] = "Percentual Error"
inter

