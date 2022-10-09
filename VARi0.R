library("vars")

## wragling 
data(Canada)
GCanada <- diff(log(Canada))

## unit root for I(0)
# do by yourself

## lag selection
VARselect(GCanada, lag.max = 5, type = "none")     # lag 4
VARselect(GCanada, lag.max = 5, type = "const")    # lag 2 or 1
VARselect(GCanada, lag.max = 5, type = "trend")    # lag 2
VARselect(GCanada, lag.max = 5, type = "both")     # lag 2

## estimate VAR model
VARmodel1 <- VAR(GCanada, p = 4, type = "none")
VARmodel2 <- VAR(GCanada, p = 2, type = "const")
VARmodel3 <- VAR(GCanada, p = 2, type = "trend")
VARmodel4 <- VAR(GCanada, p = 2, type = "both")

summary(VARmodel1)
summary(VARmodel2)

## Impulse responses
Impulse <- irf(VARmodel2)
plot(Impulse)

## Variance decmposition
FEVD <- fevd(VARmodel2)

