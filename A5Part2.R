library(alr4)

data("oldfaith")
attach(oldfaith)

Model0=lm(Interval~poly(Duration,3,raw=T), data=oldfaith)
Model1=lm(Interval~poly(Duration,4,raw=T), data=oldfaith)

coef(summary(Model0))
coef(summary(Model1))

deviance(Model0)
deviance(Model1)