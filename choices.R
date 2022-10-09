library('foreign')
library('stargazer')
library('MASS')
library('erer')
library('nnet')

data <- read.csv(file.choose(), header = TRUE)
head(data)

# STEP 1 : multinomial logit model
multi <- multinom(choice ~ price.4 + price.2, data = data)
summary(multi)

# STEP 2 : P-value
stargazer(multi, type='text', out='multil.htm')

# STEP 3 : odd ratio
exp(coef(multi))

# STEP 3 : marginal effect #
# better go stata #


