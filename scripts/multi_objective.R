# Model Multi Objective
multi_objective <- function(df, s=cov(df), mu=colMeans(df), gamma=10) {
  e <- rep(1, length(mu))
  s_inv <- solve(s)
  a <- as.numeric(t(e) %*% s_inv %*% e)
  b <- as.numeric(t(mu) %*% s_inv %*% e)
  w <- 1 / gamma * (s_inv %*% mu - (b - gamma) / a * (s_inv %*% e))
  return(w)
}
