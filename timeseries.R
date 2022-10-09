data=read.csv(file.choose(),head=TRUE)
attach(data)
y=ts(gnp)
#DF1==> diff(y)=gamma*y_t-1+u
#DF2==> diff(y)=alpha+gamma*y_t-1+u
#DF3==> diff(y)=alpha+gamma*y_t-1+lamda*tt+u
z=diff(y) # ∆y
n=length(y)
z.lag.1=c(0,y[1:(n-1)]) # y_t-1
tt=1:n # time trend
Z=c(0,z) # diff(y)
# no constant and no time trend
df1=lm(Z~-1+z.lag.1)
summary(df1)
# constant and no time trend
df2=lm(Z~z.lag.1)
summary(df2)
# constant and time trend
df3=lm(Z~z.lag.1+tt)
summary(df3)

