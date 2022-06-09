# Global minimum variance model
gmv <- function(df, s = cov(df), mu = colMeans(df)) {
    s_inv <- solve(s)
    e <- rep(1, length(mu))
    w_opt <- (s_inv %*% e) / (t(e) %*% s_inv %*% e)

    invest_names <- row.names(w_opt)
    portfolio_expected_return <- t(w_opt) %*% mu
    portfolio_var_risk <- t(w_opt) %*% s %*% w_opt

    return(list(
        w_opt = w_opt,
        invest_names = invest_names,
        individual_expected_return = mu,
        individeual_var_risk = s,
        portfolio_expected_return = portfolio_expected_return,
        portfolio_var_risk = portfolio_var_risk
    ))
}

# Mean-variance model
mv <- function(df, s = cov(df), mu = colMeans(df), r = 0.20) {
    e <- rep(1, length(mu))
    s_inv <- solve(s)
    a <- t(e) %*% s_inv %*% e
    b <- t(mu) %*% s_inv %*% e
    c <- t(mu) %*% s_inv %*% mu
    d <- (b - a * r) / (b^2 - a * c)
    f <- (b * r - c) / (b^2 - a * c)
    g <- (d * mu + f * e)
    w_opt <- s_inv %*% g

    invest_names <- row.names(w_opt)
    portfolio_expected_return <- t(w_opt) %*% mu
    portfolio_var_risk <- t(w_opt) %*% s %*% w_opt

    return(list(
        w_opt = w_opt,
        invest_names = invest_names,
        individual_expected_return = mu,
        individeual_var_risk = s,
        portfolio_expected_return = portfolio_expected_return,
        portfolio_var_risk = portfolio_var_risk
    ))
}

# Multi Objective Model
multi_objective <- function(df, s = cov(df), mu = colMeans(df), gamma = 10) {
    e <- rep(1, length(mu))
    s_inv <- solve(s)
    a <- as.numeric(t(e) %*% s_inv %*% e)
    b <- as.numeric(t(mu) %*% s_inv %*% e)
    w_opt <- 1 / gamma * (s_inv %*% mu - (b - gamma) / a * (s_inv %*% e))

    invest_names <- row.names(w_opt)
    portfolio_expected_return <- t(w_opt) %*% mu
    portfolio_var_risk <- t(w_opt) %*% s %*% w_opt

    return(list(
        w_opt = w_opt,
        invest_names = invest_names,
        individual_expected_return = mu,
        individeual_var_risk = s,
        portfolio_expected_return = portfolio_expected_return,
        portfolio_var_risk = portfolio_var_risk
    ))
}

# Signle index model
single_index <- function(df_invest_ret, market_ret, r_br) {
    mu_invest <- colMeans(df_invest_ret)
    mu_market <- mean(market_ret)
    s_market <- var(market_ret)
    si_model <- lm(as.matrix(df_invest_ret) ~ as.vector(market_ret))
    alpha <- si_model$coefficients[1, ]
    beta <- si_model$coefficients[2, ]

    erb <- (mu_invest - r_br) / beta
    erb <- sort(erb, decreasing = TRUE)

    mu_invest <- mu_invest[names(erb)]
    alpha <- alpha[names(erb)]
    beta <- beta[names(erb)]

    sigma2_err <- diag(cov(si_model$residual))
    sigma2_err <- sigma2_err[names(erb)]
    sigma2_market <- var(market_ret)
    a_big <- (mu_invest - r_br) * beta / sigma2_err
    b_big <- beta^2 / sigma2_err

    c_big <- c()
    for (i in seq_len(length(mu_invest))) {
        c_big <- c(c_big, sigma2_market * sum(a_big[1:i]) / (1 + sigma2_market * sum(b_big[1:i])))
    }

    cut_of_point <- max(c_big)
    portfolio_invest_names <- names(erb[erb > cut_of_point])
    z <- beta[portfolio_invest_names] / sigma2_err[portfolio_invest_names] * (erb[portfolio_invest_names] - cut_of_point)
    w_opt <- z / sum(z)

    invest_names <- names(w_opt)
    description <- data.frame(mu_invest, alpha, beta, sigma2_err, erb, a_big, b_big, c_big)
    portfolio_expected_return <- t(w_opt) %*% alpha[portfolio_invest_names] + t(w_opt) %*% beta[portfolio_invest_names] * mu_market
    portfolio_var_risk <- (t(w_opt) %*% beta[portfolio_invest_names])^2 * s_market + (t(w_opt) %*% sqrt(sigma2_err[portfolio_invest_names]))^2

    return(list(
        w_opt = w_opt,
        description = description,
        cut_of_point = cut_of_point,
        invest_names = invest_names,
        portfolio_expected_return = portfolio_expected_return,
        portfolio_var_risk = portfolio_var_risk
    ))
}

# Backward stepwise selection algorithm
backward <- function(df, model) {
    while (TRUE) {
        portfolio_model <- model(df)

        if (!sum(portfolio_model$w_opt < 0) | ncol(df) < 3) {
            break
        }

        df <- df[, -which(portfolio_model$w_opt == min(portfolio_model$w_opt))]
    }

    return(portfolio_model)
}

# Asset growth simulation
asset_growth_sim <- function(df_price_sim, asset = 10 * 10^6, model) {
    asset_per_fund <- asset * model$w_opt
    unit_per_fund <- asset_per_fund / df_price_sim[1, ]
    return(as.matrix(df_price_sim) %*% t(as.matrix(unit_per_fund)))
}
