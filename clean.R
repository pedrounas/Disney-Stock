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

# Plot the series
plot(dis)

# 1 Abordagem - Sem transformação

dis_test <- last(dis,'12 month')
dis_train <- window(dis, start=index(first(dis)), end=index(first(dis_test)) - 1)

acf2(dis_train) # Indica um AR(1) mas que necessitará de diferenciação.
# Box.test(dis_ar1$fit, type='Ljung') # Rejeitamos H0 
dis_ar1 <- sarima(dis_train, 1, 0,0)
dis_pred_ar1 <- sarima.for(dis_train, 12, 1,0,0)
dis_es_holt <- holt(dis_train, beta = 0.01914, h = 12) # Beta encontrado usando uma função auxiliar
dis_es_holt$mean

results <- data.frame(Values <- dis_test)
results$AR1 <- dis_pred_ar1$pred
temp <- as.data.frame(dis_es_holt$mean)
temp <- as.numeric(temp$x)
results$ES <- temp
results

# Após produção das tabelas comparar o error percentual médio
mean(inter$`Percentual Error`)
mean(inter_es$`Percentual Error`)

# 2 Abordagem - Log Transform

# 3 Abordagem - Differenciação

# Log-Transform
dis_log <- log(dis)

# Divide into train and test data
dis_test <- last(dis_diff,'12 month')
dis_train <- window(dis_diff, start=index(first(dis)), end=index(first(dis_test)) - 1)

# Ljung-Box test
Box.test(dis_train, type='Ljung')


# Testing

# Produzir tabela ARIMA
high <- dis_pred_ar1$pred + 1.96*dis_pred_ar1$se
low <- dis_pred_ar1$pred - 1.96*dis_pred_ar1$se

inter <- data.frame(low, high)

inter[,(ncol(inter)+1)] = (coredata(dis_test) > inter$low & coredata(dis_test) < inter$high)[,1]
inter[,(ncol(inter)+1)] = coredata(dis_test)
inter[,(ncol(inter)+1)] = dis_pred_ar1$pred
inter[,(ncol(inter)+1)] = abs(coredata(dis_test) - dis_pred_ar1$pred)/coredata(dis_test)

colnames(inter)[3] = "Contains"
colnames(inter)[4] = "Observed value"
colnames(inter)[5] = "Forecast"
colnames(inter)[6] = "Percentual Error"
inter

# Produzir tabela ES
high_es <- dis_es_holt$upper[,2]
low_es <- dis_es_holt$lower[,2]
inter_es <- data.frame(low_es, high_es)

inter_es[,(ncol(inter_es)+1)] = (coredata(dis_test) > inter_es$low & coredata(dis_test) < inter_es$high)[,1]
inter_es[,(ncol(inter_es)+1)] = coredata(dis_test)
inter_es[,(ncol(inter_es)+1)] = dis_es_holt$mean
inter_es[,(ncol(inter_es)+1)] = abs(coredata(dis_test) - dis_es_holt$mean)/coredata(dis_test)

colnames(inter_es)[3] = "Contains"
colnames(inter_es)[4] = "Observed value"
colnames(inter_es)[5] = "Forecast"
colnames(inter_es)[6] = "Percentual Error"
inter_es

# Encontrar menor beta
beta <- seq(.0001, .5, by = .001)
RMSE <- NA
for(i in seq_along(beta)) {
  fit <- holt(dis_train, beta = beta[i], h = 100)
  RMSE[i] <- accuracy(fit, dis_test)[2,2]
}

beta.fit <- data_frame(beta, RMSE)
beta.min <- filter(beta.fit, RMSE == min(RMSE))
beta.min