simple_return <- function(df) {
    date <- row.names(df)
    df_ret <- lapply(df, function(x) {
        diff(x) / x[-length(x)]
    })
    df_ret <- as.data.frame(df_ret)
    row.names(df_ret) <- date[-1]
    return(df_ret)
}

log_return <- function(df) {
    date <- row.names(df)
    df_ret <- lapply(df, function(x) {
        log(x[-1, ] / x[-length(x)])
    })
    row.names(df_ret) <- date[-1]
    return(df_ret)
}
