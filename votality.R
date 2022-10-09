#install.packages("rugarch")
#install.packages("dynlm")
library(rugarch)
library(dynlm)

## import data ##
data <- read.csv(file.choose(), header=TRUE)
attach(data)
dailyreturn <- diff(log((set)))
plot(ts(dailyreturn, start = c(2008,1,2), freq = 252,), ylab="return", main = "SET index return")
hist(dailyreturn, main = "SET index return")

## ARC (1) Mean Model ## y = ß0 + u to find u for using
meanEQ <- dynlm(dailyreturn ~ 1)
summary(meanEQ)

## ARCH (1) Variance Model ##
ehatsq <- ts(resid(meanEQ)^2)
ARCH <- dynlm(ehatsq~L(ehatsq, 1))
summary(ARCH)

## test
T <- length(dailyreturn)
k1 <- 1
k2 <- 2 # this ARCH only have 2 parameter
q <- abs(k1-k2)
Rsq <- 0.07907 # use multiple R squared
LM <- (T-q) * Rsq
alpha <- 0.05
Chicr <- qchisq(1-alpha, abs(k1-k2)) # param(conf.interval, degree of freedom)
pvalue <- 1-pchisq(Chicr, T-q, lower.tail = FALSE)

#====== Step 2 Select GARCH TYPE ===============#
# ARCH(1) == GARCH(1,0) model
arch<-ugarchspec(mean.model = list(armaOrder = c(0, 0)), variance.model = list(model = "fGARCH", garchOrder = c(1, 0),
                                                                               submodel="GARCH"), distribution.model = "norm")
# GARCH(1,1) model
garch<-ugarchspec(mean.model = list(armaOrder = c(0, 0)), variance.model = list(model = "fGARCH", garchOrder = c(1, 1),
                                                                                submodel="GARCH"), distribution.model = "norm")
# IGARCH(1,1) model
Igarch<-ugarchspec(mean.model = list(armaOrder = c(0, 0)),variance.model = list(model = "iGARCH", garchOrder = c(1, 1)),
                   distribution.model = "norm")
# EGARCH(1,1) model
Egarch<-ugarchspec(mean.model = list(armaOrder = c(0, 0)),variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
                   distribution.model = "norm")
# TGARCH(1,1) model
Tgarch<-ugarchspec(mean.model = list(armaOrder = c(0, 0)), variance.model = list(model = "fGARCH", garchOrder = c(1, 1),
                                                                                 submodel="TGARCH"), distribution.model = "norm")
# AGARCH(1,1) model
Agarch<-ugarchspec(mean.model = list(armaOrder = c(0, 0)), variance.model = list(model = "fGARCH", garchOrder = c(1, 1),
                                                                                 submodel="AVGARCH"), distribution.model = "norm")
# GJRGARCH(1,1) model
GJRgarch<-ugarchspec(mean.model = list(armaOrder = c(0, 0)), variance.model = list(model = "fGARCH", garchOrder = c(1, 1),
                                                                                   submodel="GJRGARCH"), distribution.model = "norm")
# GARCH(1,1) in Mean model
garchM<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0),include.mean = TRUE,archm = TRUE, archpow =2), distribution.model = "norm")

## step 3 Maximum Likelihood Estimator EX:GARCH-M
Modelfit <- ugarchfit(spec = garchM, data = dailyreturn)

#====== Step 4 Plot Volatility EX: GARCH-M ===============#
hhat <- ts(Modelfit@fit$sigma^2)
plot(ts(hhat, start=c(2008,1,2), freq=252), ylab="volatility",main="SET index volatility")

