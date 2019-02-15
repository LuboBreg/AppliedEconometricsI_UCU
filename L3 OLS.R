
#Regression 
library(wooldridge)  # Thank you for a hint :) 
library(ggplot2)

#Bivariate model
data('wage1')
qplot( wage1$educ, wage1$wage) + xlab('Years of Education') + ylab('Wage per an hour') + title('Wages vs Education, 1976')
qplot( wage1$educ, wage1$wage) + 
  xlab('Years of Education') + 
  ylab('Wage per an hour') +
  geom_smooth(method = "lm")

wage_model <- lm(wage ~ educ, data = wage1)
summary(wage_model)

#Chaning the functional form of equation
#LogLin 
log_wage_model <- lm(lwage ~ educ, data = wage1)
summary(log_wage_model)
qplot( wage1$educ, wage1$lwage) + 
  xlab('Years of Education') + 
  ylab('Log of wage per an hour') +
  geom_smooth(method = "lm")

#Loglog 
wage1$leduc <- log(wage1$educ)
summary(loglog_wage_model)
qplot( wage1$leduc, wage1$lwage) + 
  xlab('Log Years of Education') + 
  ylab('Log of wage per an hour') +
  geom_smooth(method = "lm")


#Multivariate model 
qplot( wage1$educ, wage1$lwage) + 
  xlab('Years of Education') + 
  ylab('Log of wage per an hour') +
  geom_smooth(method = "lm")

qplot( wage1$exper, wage1$lwage) + 
  xlab('exper') + 
  ylab('Log of wage per an hour') +
  geom_smooth(method = "lm")

qplot( wage1$tenure, wage1$lwage) + 
  xlab('tenure') + 
  ylab('Log of wage per an hour') +
  geom_smooth(method = "lm")

qplot( wage1$nonwhite, wage1$lwage) + 
  xlab('Nonwhite') + 
  ylab('Log of wage per an hour') +
  geom_smooth(method = "lm") +
  geom_boxplot(mapping = aes(group = wage1$nonwhite))

qplot( wage1$married, wage1$lwage) + 
  xlab('Married') + 
  ylab('Log of wage per an hour') +
  geom_smooth(method = "lm") +
  geom_boxplot(mapping = aes(group = wage1$nonwhite))

qplot( wage1$female, wage1$lwage) + 
  xlab('Female') + 
  ylab('Log of wage per an hour') +
  geom_smooth(method = "lm") +
  geom_boxplot(mapping = aes(group = wage1$nonwhite))

mv_wage_model <- lm(lwage ~ educ + exper + tenure + nonwhite + female + married, data = wage1)
summary(mv_wage_model)

