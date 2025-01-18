#### Nowcasting GDP ####

## Pacotes
library(midasr)
library(sidrar)
library(rbcb)
library(forecast)

# Obtendo dados
pib <- get_sidra(api = "/t/1621/n1/all/v/all/p/all/c11255/90707/d/v584%201")
pib <- ts(pib[,5], start = c(1996,1), frequency = 4)

pmc <- get_sidra(api = "/t/8881/n1/all/v/7170/p/all/c11046/56736/d/v7170%205")
pmc <- ts(pmc[,5], start = c(2003,1), frequency = 12)

pms <- get_sidra(api = "/t/8688/n1/all/v/7168/p/all/c11046/56726/c12355/107071/d/v7168%205")
pms <- ts(pms[,5], start = c(2011,1), frequency = 12)

pim <- get_sidra(api = "/t/8888/n1/all/v/12607/p/all/c544/129314/d/v12607%205")
pim <- ts(pim[,5], start = c(2002,1), frequency = 12)

ibc <- get_series(24364, start_date = "2003-01-01", end_date = "2024-12-31", as = c("ts"))

# Gerando séries log diferença
d_pib <- diff(log(pib)) * 100
d_ibc <- diff(log(ibc)) * 100
d_pim <- diff(log(pim)) * 100
d_pms <- diff(log(pms)) * 100
d_pmc <- diff(log(pmc)) * 100

## Estimation Best Model fulldata
ibc_f = window(d_ibc, start = c(2013, 1), end = c(2024, 9))  # Séries mensais do IBC
pim_f = window(d_pim, start = c(2013, 1), end = c(2024, 9))  # Séries mensais do PIM
pmc_f = window(d_pmc, start = c(2013, 1), end = c(2024, 9))  # Séries mensais do PMC
pms_f = window(d_pms, start = c(2013, 1), end = c(2024, 9))  # Séries mensais do PMS
pib_f = window(d_pib, start = c(2013, 1), end = c(2024, 3))   # Séries trimestrais do PIB


# Best Model fulldata - Modelo Midas
best_model <- midas_r(pib_f ~ mls(pib_f, 1, 1) + 
                        mls(ibc_f, 3:1, 3, nbeta) +
                        mls(pim_f, 3:1, 3, nbeta) + 
                        mls(pmc_f, 3:1, 3, nbeta) +
                        mls(pms_f, 3:1, 3, nbeta),
                      start = list(ibc_f = c(1.7, 1, 1.5), 
                                   pim_f = c(1.7, 1, 1.5), 
                                   pmc_f = c(1.7, 1, 1.5), 
                                   pms_f = c(1.7, 1, 1.5)))
summary(best_model)


# Obtendo as duas últimas observações de cada série
last_obs_nn_ibc <- tail(n_ibc, 2)
last_obs_nn_pim <- tail(n_pim, 2)
last_obs_nn_pmc <- tail(n_pmc, 2)
last_obs_nn_pms <- tail(n_pms, 2)

# Repetição do último valor das séries para previsão
new_value_ibc <- rep(last_obs_nn_ibc[length(last_obs_nn_ibc)], 1)
new_value_pim <- rep(last_obs_nn_pim[length(last_obs_nn_pim)], 1)
new_value_pmc <- rep(last_obs_nn_pmc[length(last_obs_nn_pmc)], 1)
new_value_pms <- rep(last_obs_nn_pms[length(last_obs_nn_pms)], 1)

# Criando os vetores para previsão
new_xx_ibc <- c(last_obs_nn_ibc, new_value_ibc)
new_xx_pim <- c(last_obs_nn_pim, new_value_pim)
new_xx_pmc <- c(last_obs_nn_pmc, new_value_pmc)
new_xx_pms <- c(last_obs_nn_pms, new_value_pms)

new_yy <- c(NA)  # Observação NA para o quarto trimestre de 2024

# Previsão PIB 2024Q4 com base no melhor modelo
forecast_best <- forecast(best_model, newdata = list(ibc_f = new_xx_ibc,
                                                     pim_f = new_xx_pim,
                                                     pmc_f = new_xx_pmc,
                                                     pms_f = new_xx_pms,
                                                     pib_f = new_yy))
round(forecast_best$mean, 2)

