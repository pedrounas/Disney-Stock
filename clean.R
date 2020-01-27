# Load file with the functions
source('helper.R')

# Install and initialize all libraries
list.of.packages <- c("tidyverse", "forecast", "dplyr","astsa", "xts", "tseries")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages, require, character.only = TRUE)

# Open the dataset and turn it into a time series
dis <- read.csv('./DIS.csv')
dis <- dis %>% select(1,5)
dis$Date <- as.Date(dis$Date, formate='%Y/%m/%d')
dis <- xts(dis$Close, order.by = dis$Date)
colnames(dis) <- c('Value')

# Unit root testing
adf.test(dis, alt='stationary')
kpss.test(dis)

# Plot the series
plot(dis, main='The Walt Disney Company')

# 1 Abordagem - Sem transformação

dis_test <- last(dis,'12 month')
dis_train <- window(dis, start=index(first(dis)), end=index(first(dis_test)) - 1)

acf2(dis_train)

auto.arima(dis_train, trace= T)
dis_arima <- sarima(dis_train, 0,1,0)
dis_pred_arima <- sarima.for(dis_train, 12, 0,1,0)

dis_es <- holt(dis_train, h = 12, beta = find_beta(dis_train, dis_test))
acf2(dis_es$mean, main='ACF and PACF of the ES Model')
Box.test(dis_es$mean, type='Ljung')
autoplot(dis_es)

results <- data.frame(Values <- dis_test)
results$ARIMA <- dis_pred_arima$pred
temp <- as.data.frame(dis_es$mean)
temp <- as.numeric(temp$x)
results$ES <- temp
results

accuracy(results$ARIMA, dis_test)
accuracy(results$ES, dis_test)

arima_table(dis_pred_arima, dis_test)
es_table(dis_es, dis_test)

# 2 Abordagem - Log Transform

dis_log <- log(dis)
dis_test_log <- last(dis_log,'12 month')
dis_train_log <- window(dis_log, start=index(first(dis_log)), end=index(first(dis_test_log)) - 1)
par(mfrow=c(2,1))
plot(dis, main='Ações da Walt Disney Company')
plot(dis_log, main ='Ações da Walt Disney Company - Log')
par(mfrow=c(1,1))

acf2(dis_train_log) # Ii
auto.arima(dis_train_log, seasonal = F, trace=T)

dis_log_arima <- sarima(dis_train_log, 0, 1, 0)
dis_log_pred_arima <- sarima.for(dis_train_log, 12, 0, 1 ,0)

dis_log_es <- holt(dis_train_log, h = 12, beta = find_beta(dis_train_log, dis_test_log))
acf2(dis_log_es$mean, main='ACF and PACF of the ES Model')
Box.test(dis_log_es$mean, type='Ljung')
autoplot(dis_log_es)


results_log <- data.frame(Values <- dis_test_log)
results_log$ARIMA <- dis_log_pred_arima$pred
temp <- as.data.frame(dis_log_es$mean)
temp <- as.numeric(temp$x)
results_log$ES <- temp
results_log

accuracy(results_log$ARIMA, dis_test_log)
accuracy(results_log$ES, dis_test_log)

arima_table(dis_log_pred_arima, dis_test_log)
es_table(dis_log_pred_es, dis_test_log)


# 3 Abordagem - Differenciação
ndiffs(dis) # Diz-nos que precisamos de diferenciar 2 vezes
dis_diff <- diff(dis)[-1,]
dis_diff <- diff(dis_diff)[-1,]
autoplot(dis_diff)

dis_test_diff <- last(dis_diff,'12 month')
dis_train_diff <- window(dis_diff, start=index(first(dis_diff)), end=index(first(dis_test_diff)) - 1)
acf2(dis_train_diff) # Parece indicar um ARIMA(4,0,0)

auto.arima(dis_train_diff, trace=T) # Confirma a nossa hipótese de ARIMA(4,0,0)

dis_diff_arima <- sarima(dis_train_diff, 4, 0 ,0)

dis_diff_pred_arima <- sarima.for(dis_train_diff,12,4,0,0)

dis_diff_es <- ses(dis_train_diff, alpha = find_alpha(dis_train_diff, dis_test_diff), h = 12)
autoplot(dis_diff_es)


results_diff <- data.frame(Values <- dis_test_diff)
results_diff$ARIMA <- dis_diff_pred_arima$pred
temp <- as.data.frame(dis_diff_es$mean)
temp <- as.numeric(temp$x)
results_diff$ES <- temp
results_diff

accuracy(results$ARIMA, dis_test_diff)
accuracy(results$ES, dis_test_diff)

arima_table(dis_diff_pred_arima, dis_test_diff)
es_table(dis_diff_es, dis_test_diff)
