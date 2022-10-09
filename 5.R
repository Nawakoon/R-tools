install.packages('plm')
library(plm)

### Step 1 Import data ###
data <- read.csv(file.choose(), head=TRUE)

### Step 2 Convert file to be Panel data ###
panel <- pdata.frame(data, c('id', 'year'))

### Step 3 Run Panel regression (Fixed effect) ###

# 3.1 (First Difference)
fd <- plm(y ~ x1 + x2, model = 'fd', data = panel)
summary(fd)

# 3.2 (Fixed effect)
fe <- plm(y ~ x1 + x2, effect = 'individual', model = 'within', data = panel)
summary(fe)
# effect = c('individual', 'time', 'twoways')

# 3.3 (Random effect)
re <- plm(y ~ x1 + x2, effect = 'individual', model = 'random', data = panel)
summary(re)

# 3.4 (Pooling OLS)
pool <- plm(y ~ x1 + x2, model = 'pool', data = panel)
summary(pool)

### Hausman Test ###
phtest(fe, re)