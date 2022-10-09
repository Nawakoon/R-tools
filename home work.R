# econometric work linear data from excel sheet 8

# tidy ; import & set variable
data <- read.csv(file.choose(),header=TRUE)
attach(data)

linearmodel <- lm(lntravel~lnsalary+household+sex+price+lnexpense)
summary(linearmodel)

## t-test, f-test , r-square 
Beta <- c(-0.2031965, -3.3919591, 0.5256631, 0.1351832, 0.8722086, 5.3391117)
SD <- c(0.90161, 3.28107, 0.25162, 0.40522, 0.05806, 6.42754)
tstatB0 <- Beta[1]/SD[1]
tstatB1 <- Beta[2]/SD[2]
tstatB2 <- Beta[3]/SD[3]
tstatB3 <- Beta[4]/SD[4]
tstatB4 <- Beta[5]/SD[5]
tstatB5 <- Beta[6]/SD[6]
betaname <- c("beta0", "lnsalary", "household", "sex", "price", "lnexpense")
tstateach = c(tstatB0, tstatB1, tstatB2, tstatB3, tstatB4, tstatB5)
names(tstateach) <- betaname

## f-test ; [(SSRr-SSRu)/q]-[SSRu/(n-k)]
Mr=lm(lntravel~ 1)
Er=residuals(Mr)
Mu=lm(lntravel~lnsalary+household+sex+price+lnexpense)
Eu=residuals(Mu)
SSRr=sum(Er^2)
SSRu=sum(Eu^2)
k=7
n=length(lntravel)
q=5
Fstat=((SSRr-SSRu)/q)/(SSRu/(n-k))

## r-square ; r^2= 1-(SSR/SST)
u <- residuals(linearmodel)
SSR <- sum(u^2)
SST <- sum((lntravel-mean(lntravel))^2)
Rsquare <- 1-(SSR/SST)

## logit
# tidy
data2 <- read.csv(file.choose(), head=TRUE)
attach(data2)
head(data2)
# model
logitmodel <- glm(travel~lnsalary+household+sex+price+lnexpense,family=binomial(link = "logit"))
summary(logitmodel)

# model probit
probitmodel <- glm(travel~lnsalary+household+sex+price+lnexpense,family=binomial(link = "probit"))
summary(probitmodel)

## marginal effect ; sig only household and price
library("mfx")
marginalEffect <- logitmfx(formula = travel~ household+price, data = data2)





