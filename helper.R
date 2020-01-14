# Produzir a tabela de resultados do modelo ARIMA

arima_table <- function(predictions, data) {
  
  high <- predictions$pred + 1.96*predictions$se
  low <- predictions$pred - 1.96*predictions$se
  
  inter <- data.frame(low, high)
  
  inter[,(ncol(inter)+1)] = (coredata(data) > inter$low & coredata(data) < inter$high)[,1]
  inter[,(ncol(inter)+1)] = coredata(data)
  inter[,(ncol(inter)+1)] = predictions$pred
  inter[,(ncol(inter)+1)] = abs(coredata(data) - predictions$pred)/coredata(data)
  
  colnames(inter)[3] = "Contains"
  colnames(inter)[4] = "Observed value"
  colnames(inter)[5] = "Forecast"
  colnames(inter)[6] = "Percentual Error"
  inter
}

# Produzir a tabela de resultados do modelo ES

es_table <- function(predictions, data) {
  
  high_es <- predictions$upper[,2]
  low_es <- predictions$lower[,2]
  
  inter_es <- data.frame(low_es, high_es)
  
  inter_es[,(ncol(inter_es)+1)] = (coredata(data) > inter_es$low & coredata(data) < inter_es$high)[,1]
  inter_es[,(ncol(inter_es)+1)] = coredata(data)
  inter_es[,(ncol(inter_es)+1)] = predictions$mean
  inter_es[,(ncol(inter_es)+1)] = abs(coredata(data) - predictions$mean)/coredata(data)
  
  colnames(inter_es)[3] = "Contains"
  colnames(inter_es)[4] = "Observed value"
  colnames(inter_es)[5] = "Forecast"
  colnames(inter_es)[6] = "Percentual Error"
  inter_es
  
}

# Encontrar o valor de beta que minimiza o RMSE

find_beta <- function(data, training) {
  beta <- seq(.0001, .5, by = .001)
  RMSE <- NA
  for(i in seq_along(beta)) {
    fit <- holt(data, beta = beta[i], h = 100)
    RMSE[i] <- accuracy(fit, training)[2,2]
  }
  
  beta.fit <- tibble(beta, RMSE)
  beta.min <- filter(beta.fit, RMSE == min(RMSE))
  return(beta.min$beta)
}

find_alpha <- function(data, training) {

  alpha <- seq(.01, .99, by = .01)
  RMSE <- NA
  for(i in seq_along(alpha)) {
    fit <- ses(data, alpha = alpha[i], h = 100)
    RMSE[i] <- accuracy(fit, training)[2,2]
  }
  
  alpha.fit <- tibble(alpha, RMSE)
  alpha.min <- filter(alpha.fit, RMSE == min(RMSE))
  return(alpha.min$alpha)
}