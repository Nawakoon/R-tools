## logit probit model nawakoon peerarasee 621610155
# tidy
data <- read.csv(file.choose(), head=TRUE)
attach(data)
head(data)

# model probit
probitmodel <- glm(travel~lnsalary+household+sex+price+lnexpense,family=binomial(link = "probit"))
summary(probitmodel)

# model logit
logitmodel <- glm(travel~lnsalary+household+sex+price+lnexpense,family=binomial(link = "logit"))
summary(logitmodel)

# likelihood ratio test ; LR = 2(logLur - logLr) ~ Chi^2
restricted <- glm(travel~1)
unrestricted <- glm(travel~lnsalary+household+sex+price+lnexpense)
logLr <- logLik(restricted)
logLur <- logLik(unrestricted)
loglikelihoodstat <- 2*(logLur-logLr)

## marginal effect ; sig only household and price
library("mfx")
marginalEffect <- logitmfx(formula = travel~lnsalary+household+sex+price+lnexpense , data = data)

## odd ratio
oddlnsalary <- exp(0.7009467)
oddhousehold <- exp(0.1278554)
oddsex <- exp(0.0581469)
oddprice <- exp(-0.0356904)
oddlnexpense <- exp(-1.2388466)
