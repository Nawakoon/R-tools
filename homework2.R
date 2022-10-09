#### tidy ####
data <- read.csv(file.choose(), head = TRUE)
attach(data)

#### test stationary ####
# I(0)
library("urca")
GSI0Test <- ur.df(GS10, type = "none", selectlags = "AIC")
TBI0Test <- ur.df(TB3, type = "none", selectlags = "AIC")
summary(GSI0Test) # not station
summary(TBI0Test) # not station

# I(1)
GSI1Test <- ur.df(diff(GS10), type = "none", selectlags = "AIC")
TBI1Test <- ur.df(diff(TB3), type = "none", selectlags = "AIC")
summary(GSI1Test) # station
summary(TBI1Test) # station

#### run regression, find residual for testing ####
hwmodel <- lm(GS10 ~ TB3)
u <- residuals(hwmodel)

# test residual stationary
uTestNone <- ur.df(u, type = "none", selectlags = "AIC")
uTestdrift <- ur.df(u, type = "drift", selectlags = "AIC")
uTesttrend <- ur.df(u, type = "trend", selectlags = "AIC")
summary(uTestNone)   # u station
summary(uTestdrift)  # u station
summary(uTesttrend)  # u station

# summary model #
summary(hwmodel)



