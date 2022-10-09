## import
data <- read.csv(file.choose(), head = TRUE)
attach(data)
y <- ts(gnp) # set to time series instead of crossection

## DF test -> diff(y) = gamma * y_t-1 + u
## DF test -> diff(y) = alpha + gamma * y_t-1 + u
## DF test -> diff(y) = alpha + gamma * y_t-1 + lamda*timetrend + u

z <- c(0, diff(y) # diff is y2 - y1, y3 - y2, y4 - y3, ..... # and 0 to make the same dimension as other varible
n <- length(y)               
z.lag.1 <- c(0,y[1:(n-1)])   # y_t-1
timetrend <- 1:n             

# no constant & no time trend ; diff(y) = gamma * y_t-1 + u

df1 <- lm(z ~ )
summary(df1)

df2 <- lm(z ~ z.lag.1)
summary(df2)

df3 <- lm(z ~ )
summary(df3)

## try some cool packages
install.packages("urca")
library(urca)










