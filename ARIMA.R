# 代码 10-1

# 模型定阶
library(TSA)
res1 <- armasubsets(y = halcyon.purchase, nar = 5, nma = 5)  # 原始序列BIC定阶
plot(res1)
res2 <- armasubsets(y = purchase.diff, nar = 5, nma = 5)  # 周期性差分序列BIC定阶
plot(res2)



# 代码 10-2

# 模型残差检验
library(forecast)
for(p in c(0, 1, 3)){
  for(q in c(3, 4)){
    for (P in c(0, 1, 3)) {
      arima.model <- Arima(halcyon.purchase, order = c(p, 0, q),  
                           seasonal = list(order = c(P, 1, 1), period = 7))
      box.test.result <- Box.test(arima.model$residuals, lag = 1, 
                                  type = "Ljung-Box") 
      print(paste('p =', p, '; q =', q , '; P =', P,'; 残差P值:', 
                  round(box.test.result$p.value, 4), collapse = ""))
    }
  }
}



# 代码 10-3

# 模型评估
for(p in c(0, 1, 3)){
  for(q in c(3, 4)){
    for (P in c(0, 1, 3)) {
      arima.model <- Arima(halcyon.purchase, order = c(p, 0, q), 
                           seasonal = list(order = c(P, 1, 1), period = 7))
      forecast.data <- forecast(arima.model, h = 31)
      pred <- forecast.data$mean
      index <- which(data.purchase$report_date_new >= as.Date("2014-08-1"))
      target <-  ts(data.purchase$purchase[index])
      result <- data.frame(target,pred)
      result$target <- as.numeric(result$target)
      result$pred <- as.numeric(result$pred)
      result$error <- abs(result$target - result$pred) / result$target
      result$score <- 5 * cos(10 / 3 * pi * result$error) + 5
      result$score[which(result$error >= 0.3)] <- 0
      print(paste('p =', p, '; q =', q , '; P =', P,'；误差：', 
                  round(mean(result$error), 4),'; 得分:', 
                  round(mean(result$score), 4), collapse = ""))
    }
  }
} 



# 代码 10-4

# 拟合相对最优ARIMA模型
arima.model <- Arima(halcyon.purchase, order = c(0, 0, 3), 
                     seasonal = list(order = c(1, 1, 1), period = 7))
forecast.data <- forecast(arima.model, h = 31)
pred <- forecast.data$mean
index <- which(data.purchase$report_date_new >= as.Date("2014-08-1"))
target <-  ts(data.purchase$purchase[index])
result <- data.frame(target, pred)
result$target <- as.numeric(result$target)
result$pred <- as.numeric(result$pred)
result$error <- abs(result$target - result$pred) / result$target
result$score <- 5 * cos(10 / 3 * pi * result$error) + 5
result$score[which(result$error >= 0.3)] <- 0
head(result)
