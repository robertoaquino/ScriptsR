## Modelo Midas - Nowcasting do PIB ##

# Pacotes
library(midasr)
library(sidrar)
library(forecast)

# Obtendo dados
pib <- get_sidra(api = "/t/1621/n1/all/v/all/p/all/c11255/90707/d/v584%201")
pib <- ts(pib[,5], start = c(1996,1), frequency = 4)

caged <- get_sidra(api = "/t/6318/n1/all/v/1641/p/all/c629/32387")
caged <- ts(caged[,5], start = c(2012,3), frequency = 12)

# Gerando séries log diferença
y <- diff(log(pib)) * 100
x <- diff(log(caged)) * 100

nx <- ts(c(rep(NA, 194), x), start = start(y), frequency = 12)
ny <- ts(c(y, NA), start = start(y), frequency = 4)

# Visualição dos dados
plot.ts(ny, xlab = "Time", ylab = "Percentages", col = 2, ylim = c(-5, 6))
lines(nx, col = 4, lty = 2)  # Adiciona a série nx com uma linha pontilhada para destacar a defasagem
legend("topleft", legend = c("PIB Trimestral", "Emprego Mensal"), col = c(2, 4), lty = c(1, 2))

# Separando a base de dados treino
xx <- window(nx, start = c(2013, 1), end = c(2022, 3))
yy <- window(ny, start = c(2013, 1), end = c(2022, 1))

# Estimação Modelo 1
model1 <- midas_r(yy ~ mls(yy, 1, 1) + mls(xx, 3:7, 3, nbeta), 
                  start = list(xx = c(1.7, 1, 5)))
coef(model1)

# Estimação Modelo 2
model2 <- midas_r(yy ~ mls(yy, 1, 1) + mls(xx, 3:11, 3, nbetaMT), 
                  start = list(xx = c(2, 1, 5, 0)))
coef(model2)

# Estimação Modelo 3
um <- midas_r(yy ~ mls(yy, 1, 1) + mls(xx, 3:11, 3), start = NULL)
coef(um)

# Conjunto de dados fulldata
fulldata <- list(
  xx = window(nx, start = c(2013, 1), end = c(2024, 9)),  # Séries mensais do caged
  yy = window(ny, start = c(2013, 1), end = c(2024, 3))   # Séries trimestrais do pib
)

insample <- 1:length(yy)
outsample <- (1:length(fulldata$yy))[-insample]

# Validação de modelos fora da amostra
avgf <- average_forecast(list(model1, model2, um), data = fulldata, 
                         insample = insample, outsample = outsample)

sqrt(avgf$accuracy$individual$MSE.out.of.sample)

# Reestimação Melhor Modelo fulldata
y1 <- window(y, start = c(2013, 1), end = c(2024, 3))
x1 <- window(x, start = c(2013, 1), end = c(2024, 9))

best_model <- midas_r(y1 ~ mls(y1, 1, 1) + mls(x1, 3:7, 3, nbeta), 
                      start = list(x1 = c(1.7, 1, 5)))
coef(best_model)

# Forecasting do PIB
# Ajuste do modelo ARIMA para x
ts_x <- auto.arima(x)

# Previsão de x para 12/2024
fcst_x <- forecast(ts_x, h = 1)
new_value <- fcst_x$mean

# Obtendo as duas últimas observações de x
last_obs <- tail(x, 2)

# Criando o vetor new_xx
new_xx <- c(last_obs, new_value)
new_yy <- c(NA)  # Observação NA para o quarto trimestre de 2024

# Previsão PIB 2024Q4 com base no melhor modelo
forecast_best <- forecast(best_model, newdata = list(x1 = new_xx, y1 = new_yy))
round(forecast_best$mean, 2)
