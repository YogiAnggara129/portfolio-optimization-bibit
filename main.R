source("scripts/multi_objective.R")
source("scripts/mv.R")

df <- read.csv("data/nav_bibit_fund_return.csv")

# Set DateTime as index / row names
row.names(df) <- df$DateTime
df$DateTime <- NULL

# Optimal portfolio if gamma = 30
s <- cov(df)
mu <- colMeans(df)
multi_objective(s = s, mu = mu, gamma = 30)

# Optimal portfolio for top three best mu if gamma = 30
mu_best_3 <- sort(mu, decreasing = TRUE)[1:3]
mu_best_3
df2 <- df[names(mu_best_3)]
head(df2)
s2 <- cov(df2)
mu2 <- colMeans(df2)
multi_objective(df2, gamma = 30)
