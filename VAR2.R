library(vars)
data("Canada")

## Canada is I(1)
VARselect(Canada, lag.max = 5, type= "none")  # lag 2,3
VARselect(Canada, lag.max = 5, type= "const") # lag 2,3
VARselect(Canada, lag.max = 5, type= "trend") # lag 2,3
VARselect(Canada, lag.max = 5, type= "both")  # lag 1,2,3

# Step Cointegration to see it have long run correlation or not
test1 <- ca.jo(Canada, ecdet = "const", type = "eigen", K = 2)
test2 <- ca.jo(Canada, ecdet = "const", type = "trace", K = 2)

summary(test1) # r = 1
summary(test2) # r = 2

# Step 4 VECM
library(tsDyn)
modelVECM = VECM(Canada, r=2, lag = 2, include = c("const"), estim = "ML")
summary(modelVECM)


