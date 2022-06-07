source("scripts/portfolio_optimization.R")

df_bibit <- read.csv("data/nav_bibit_fund_return_train.csv")
df_ihsg <- read.csv("data/ihsg_close_return.csv")
r_br <- 0.035 / 48
# Set DateTime as index / row names
row.names(df_bibit) <- df_bibit$DateTime
df_bibit$DateTime <- NULL
row.names(df_ihsg) <- df_ihsg$Date
df_ihsg$Date <- NULL

# Choose fund that have positive expected return
mu_bibit <- colMeans(df_bibit)
df_bibit <- df_bibit[which(mu_bibit > 0)]

# Expected return and risk
## Bibit
mu_bibit <- colMeans(df_bibit)
s_bibit <- cov(df_bibit)
## IHSG
mu_ihsg <- colMeans(df_ihsg)
s_ihsg <- cov(df_ihsg)

#####################################
# Multi objective

## Optimal portfolio for gamma = 30
temp <- multi_objective(df_bibit, gamma = 2)
which(temp == min(temp))

## Optimal portfolio for gamma = 70 with backward algorithm
backward(df_bibit, function(x) {
    multi_objective(x, gamma = 70)
})

#####################################

#####################################
# Single index model
bi7drr_year <- 0.035
bi7drr_weekly <- bi7drr_year / 48
single_index(df_bibit, df_ihsg$Close, r_br = bi7drr_weekly)
#####################################