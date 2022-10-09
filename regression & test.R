data<-read.csv(file.choose(),header=TRUE)
attach(data)
X=as.matrix(cbind(1,lnsalary,sex))
Y=as.matrix(lntravel)
beta=round(solve(t(X)%*%X)%*%t(X)%*%Y,digits=3)
u=lntravel-beta[1]-(beta[2]*lnsalary)-(beta[3]*sex)
k=3
n=400
var=1/(n-k)*as.numeric(t(u)%*%u)*solve(t(X)%*%X)
VARBeta=diag(var)
SEBeta0=sqrt(VARBeta[1])
SEBeta1=sqrt(VARBeta[2])
SEBeta2=sqrt(VARBeta[3])
tstatBeta0=beta[1]/SEBeta0
tstatBeta1=beta[2]/SEBeta1
tstatBeta2=beta[3]/SEBeta2

## muticolinear test
install.packages("mctest")
library(mctest)
X=cbind(lnsalary,household,sex,price,lnexpense)
cor(X)
model=lm(lntravel~lnsalary+household+sex+price+lnexpense)
imcdiag(model)

## R square ; R square = 1-(rss/tss)
error=residuals(model)
SSR=sum(error^2)
SST=sum((lntravel-mean(lntravel))^2)
R2=1-SSR/SST

## adjusted R2; adjR2=1-((SSR/(n-k))/(SST)))
SST=sum((lntravel-mean(lntravel))^2)
SSR=sum(error^2)
k=5
n=length(lntravel)
AdjR2=1-((SSR/(n-k))/(SST/(n-1)))

## f-test ; [(SSRr-SSRu)/q]-[SSRu/(n-k)]
Mr=lm(lntravel~ 1)
Er=residuals(Mr)
Mu=lm(lntravel~lnsalary+household+sex+price)
Eu=residuals(Mu)
SSRr=sum(Er^2)
SSRu=sum(Eu^2)
k=5
n=length(lntravel)
q=4
Fstat=((SSRr-SSRu)/q)/(SSRu/(n-k))

## p-value
pvalue=1-pf(Fstat,k,n-k)

## goodness & hetero
# H0=no hetero
# H1=hetero
# breusch-Pagan test = R2*n
m2=lm(lntravel~lnsalary+sex)
error=residuals(m2)
bpmodel<-lm(error^2~lnsalary+sex)
summary(bpmodel)
bpteststat<-0.003281*n
k=3
df<-k-1

## test bp ; if p
pvaluebp<-1-pchisq(bpteststat, df)

## cheat packages
bptest0<-bptest(m2)

## pchisq test ; chi2 pro to accept H1
pvalue=1-pchisq(bpteststat, df)
install.packages("lmtest")
library(lmtest)
bptest0=bptest(m2)
bptest0

## white test = R2*n
yhat=fitted(m2)
whitemodel=lm(error^2~yhat*yhat^2)
summary(whitemodel)
wteststat <- 0.003199*n
k=3
df<-k-1

## pchisq if pvalue<0.5 mean hetero
pvaluewt=1-pchisq(wteststat, df)





