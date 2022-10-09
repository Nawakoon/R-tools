library('foreign')
library('stargazer')
library('MASS')
library('erer')

data <- read.csv(file.choose(), header = TRUE)
head(data)

# STEP 1 : ordered logit model
m1 <- polr(factor(apply)~ pared + public + gpa, data=data, Hess = TRUE)
summary(m1)

# STEP 2 : test
tstat <- coef(summary(m1))[,3]
pvalue <- 2 * (1 - pnorm(abs(tstat)))
pvalue

exp(coef(m1)) # odd ratio result
marginal <- ocME(m1) # marginal effect result
