library(plm)
data <- read.csv(file.choose(), header = TRUE)
panel_data <- pdata.frame(data, c('country', 'year'))

### run fixed effect model ###
fix_ind <- plm(GDP ~ Consump + Financial + Industialization + Inflation, effect = 'individual', model = 'within', data = panel_data)
fix_time <- plm(GDP ~ Consump + Financial + Industialization + Inflation, effect = 'time', model = 'within', data = panel_data)
fix_two <- plm(GDP ~ Consump + Financial + Industialization + Inflation, effect = 'twoways', model = 'within', data = panel_data)
summary(fix_ind)
summary(fix_time) # best R^2
summary(fix_two)

### run random effect model ###
rd_ind <- plm(GDP ~ Consump + Financial + Industialization + Inflation, effect = 'individual', model = 'random', data = panel_data)
rd_time <- plm(GDP ~ Consump + Financial + Industialization + Inflation, effect = 'time', model = 'random', data = panel_data)
rd_two <- plm(GDP ~ Consump + Financial + Industialization + Inflation, effect = 'twoways', model = 'random', data = panel_data)
summary(rd_ind)
summary(rd_time) # best R^2
summary(rd_two)

### Hausman Test ###
phtest(fix_time, rd_time) # not sig use H0
