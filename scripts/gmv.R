gmv <- function(df, s=cov(df), mu=colMeans(df)) {
    s_inv <- solve(s)
    e <- rep(1, length(mu))
    w <- (s_inv %*% e) / (t(e) %*% s_inv %*% e)
    return(w)
}