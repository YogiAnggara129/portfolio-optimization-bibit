source("scripts/portfolio_optimizer.R")

df_bibit_ret <- read.csv("data/nav_bibit_fund_return_train.csv", row.names = "DateTime")
df_ihsg_ret <- read.csv("data/ihsg_close_return.csv", row.names = "Date")
r_br <- 0.035 / 48

# Choose fund that have positive expected return
mu_bibit <- colMeans(df_bibit_ret)
df_bibit_ret <- df_bibit_ret[which(mu_bibit > 0)]

#####################################
# Multi objective
## Optimal portfolio for gamma = 70 with backward algorithm
backward(df_bibit_ret, function(x) {
    multi_objective(x, gamma = 70)
})

#####################################

#####################################
# Single index model
bi7drr_year <- 0.035
bi7drr_weekly <- bi7drr_year / 48
single_index(df_bibit_ret, df_ihsg_ret$Close, r_br = bi7drr_weekly)
#####################################

#####################################
# Simulation

## Simulation of expected return and risk agains gamma
library(ggplot2)

gamma_list <- seq(1, 50, 1)
value <- c()
type <- c()
for (gamma in gamma_list) {
    mo_model <- backward(df_bibit_ret, function(x) {
        multi_objective(x, gamma = gamma)
    })
    value <- c(value, mo_model$portfolio_expected_return, mo_model$portfolio_var_risk)
    type <- c(type, "Return Ekspektasi", "Risiko (Variansi)")
}
sim_ret_risk_mo_model <- data.frame(gamma=gamma_list, value=value, type=type)

### Multi objective model only
ggplot(sim_ret_risk_mo_model, aes(x=gamma, y=value)) +
    geom_line(aes(linetype=type)) +
    theme_bw()

### With Single index model
si_model <- single_index(df_bibit_ret, df_ihsg_ret$Close, r_br = bi7drr_weekly)
ggplot(sim_ret_risk_mo_model, aes(x = gamma, y = value)) +
    geom_line(aes(linetype = type)) +
    geom_hline(yintercept = si_model$portfolio_expected_return) +
    geom_hline(yintercept = si_model$portfolio_var_risk) +
    theme_bw()

## Simulation of asset growth
df_bibit_sim <- read.csv("data/nav_bibit_fund_test.csv", row.names = 'DateTime')

mo_model <- backward(df_bibit_ret, function(x) {
    multi_objective(x, gamma = 100)
})
sim_mo_model <- asset_growth_sim(df_bibit_sim[mo_model$invest_names], model = mo_model)

si_model <- single_index(df_bibit_ret, df_ihsg_ret$Close, r_br = bi7drr_weekly)
sim_si_model <- asset_growth_sim(df_bibit_sim[si_model$invest_names], model = si_model)
plot(sim_si_model, type = "b")
lines(sim_mo_model)

names(which(colMeans(df_bibit_sim) > 0))
gmv(df_bibit)
simple_return(df_bibit_sim)
df_bibit_sim[si_model$invest_names]

#############
