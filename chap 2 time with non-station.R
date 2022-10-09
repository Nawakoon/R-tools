#### wrangling ####
data <- read.csv(file.choose(), head = TRUE)
attach(data)
library("urca")

#### test cointegration ####
# I(0)
I0testNone <- ur.df(LY, type = "none", selectlags = c("AIC"))
I0testNonex <- ur.df(LC, type = "none", selectlags = c("AIC"))
summary(I0testNone)

# I(1)
I1testNone <- ur.df(diff(LY), type = "none", selectlags = c("AIC"))
I1testNonex <- ur.df(diff(LC), type = "none", selectlags = c("AIC"))
summary(I1testNone)

# I(2)
I2testNone <- ur.df(diff(diff(LY)), type = "none", selectlags = c("AIC"))
summary(I2testNone)

# run regression to find residuals for testing
LR <- lm(LY~LC)
u <- residuals(LR)

# check stationary of u
test1 <- ur.df(u, type = "none", selectlags = c("AIC"))
test2 <- ur.df(u, type = "drift", selectlags = c("AIC"))
test3 <- ur.df(u, type = "trend", selectlags = c("AIC"))
summary(test1)
summary(LR)

# ECM run diff(Y) ~ ulag1 + diff(x)
n <- length(LY)
dLY <- c(0, diff(LY))
dLC <- c(0, diff(LC))
u1 <- c(0, u[1:(n-1)])

# short run equation
SR <- lm(dLY ~ u1 + dLC)
summary(SR)



