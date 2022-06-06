mv <- function(df, s=cov(df), mu=colMeans(df), r = 0.20) {
  e <- rep(1, length(mu))
  s_inv <- solve(s)
  a <- t(e) %*% s_inv %*% e
  b <- t(mu) %*% s_inv %*% e
  c <- t(mu) %*% s_inv %*% mu
  d <- (b - a * r) / (b^2 - a * c)
  f <- (b * r - c) / (b^2 - a * c)
  g <- (d * mu + f * e)
  w <- s_inv %*% g
  return(w)
}
