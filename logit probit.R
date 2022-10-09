data<-read.csv(file.choose(), head=TRUE)
attach(data)
head(data)

## logit model
logit=glm(travel~lnsalary+household+sex+price+lnexpense,family=binomial(link="logit"))
summary(logit)

## probit model
probit=glm(travel~lnsalary+household+sex+price+lnexpense,family=binomial(link="probit"))
summary(probit)

## choose the least AIC ; so use probit
## can't interpret directly so do marginal effect
# so do marginal effect -> install.packages("mfx")
library("mfx")
data=data.frame(travel, lnsalary, household, sex, price)
mfx=logitmfx(formula = travel~price+lnexpense, data=data)
## choose only the sig. variable (price, lnexpense)

## let see if we use all variable
mfx2=logitmfx(formula = travel~lnsalary+household+sex+price+lnexpense,data=data)

## becare [1] sex

## odd ratio ; use expo tp interpret
# lnsalary = -7.28037
exp(-7.28037)
# exp(-7.28037) = 0.0006889306 mean lnsarary +1 make travel prob -(1-0.0006889306) in percent change






