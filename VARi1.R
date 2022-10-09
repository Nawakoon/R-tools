library("vars")

## tidy
data("Canada")

# Assume that Cananda is I(1)
VARselect(Canada, lag.max = 5, type = "none")   # 2, 3
VARselect(Canada, lag.max = 5, type = "const")  # 2, 3
VARselect(Canada, lag.max = 5, type = "trend")  # 2, 3
VARselect(Canada, lag.max = 5, type = "both")   # 3

# Cointegration test
test1 <- ca.jo(Canada, ecdet = "const", type = "eigen", K = 2)
test2 <- ca.jo(Canada, ecdet = "const", type = "trace", K = 2)
summary(test1)  # r = 1
summary(test2)  # r = 2

# VAR, irf, fevd lag = 2
VARmodel1 <- VAR(Canada, p = 2, type = "const")
summary(VARmodel1)

# VECM
install.packages("tsDyn")
library("tsDyn")

VECMoutput <- VECM(Canada, r = 2, lag = 2, include = c("const"))
summary(VECMoutput)



