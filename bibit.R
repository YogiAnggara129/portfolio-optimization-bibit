library("rjson")
library("RSelenium")
library("wdman")

setwd("E:\\My Project\\Course\\portofolio-bibit")

# Collect data

## Get fund name in bibit

# bibit.json from:
# https://bibit-reksadana.vercel.app/api?buy_from_bibit=true&per_page=10000

bibit <- fromJSON(file = "data/bibit.json")
data <- bibit$data

name <- sapply(data, function(x) {
    x$name
})
type <- sapply(data, function(x) {
    x$type
})
minbuy <- sapply(data, function(x) {
    x$minbuy
})

df <- data.frame(name, type, minbuy)
df
write.csv(df, "data/fund_in_bibit.csv", row.names = FALSE)

# Get NAV history data

# shell("docker pull selenium/standalone-firefox")
# shell("docker run -d -p 4445:4444 selenium/standalone-firefox")
# RSelenium::checkForServer()

# binman::rm_platform("phantomjs")
# wdman::selenium(retcommand = TRUE)
# rD <- rsDriver(browser="chrome", port=4545L)
# rD <- rsDriver(browser = "firefox", port = 4545L)
# rD <- rsDriver()
# remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "firefox'")
# remDr <- rD[["client"]]
# remDr$open()
# remDr$navigate("http://www.google.com/ncr")
# remDr$getTitle()
