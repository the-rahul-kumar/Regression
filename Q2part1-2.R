library(ISLR) 
attach(Carseats)

plot(Price, Sales)

lm.fit<-lm(Sales~Price,data=Carseats) 
summary(lm.fit)
coef(lm.fit)

confint(lm.fit)

round(predict(lm.fit, data.frame(Price=c(200))),2)
predict(lm.fit, data.frame(Price=c(200)),interval="prediction")

