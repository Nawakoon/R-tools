gnp <- read.csv(file.choose())
gnp <- ts(gnp)  ## object as time series ; or attach dataname

## hand code
lag <- 0
lag1 <- lag+1
z <- diff(gnp)
n <- length(z)
z.diff <- embed(z, lag1)[,1]
z.lag.1 <- gnp[(lag1):n]
tt <- lag1:n

# no constant and trend
summary(lm(z.diff~-1+z.lag.1))

# has constant and no trend
summary(lm(z.diff~-1+z.lag.1))
