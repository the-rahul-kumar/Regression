library(forecast)
library(splines)
co <- read.csv("cohhpop.csv", col.names=c("age", "pop"), header=FALSE)



co$m_avg <- ma(co$pop,5)

plot(co$pop~co$age,pch=20)
#lines(co$m_avg~co$age,col=4,lwd = 2)
#lines(loess.smooth(co$age,co$pop,span = 0.25),col=2,lwd = 2)


fit <- lm(co$pop ~ bs(co$age, knots = seq(25,75,25)))
pdat <- data.frame(x = seq(min(co$age), max(co$age)))
pdat <- transform(pdat, yhat = predict(fit, newdata = pdat))
lines(yhat ~ x, data = pdat, lwd = 2, col = 2)

fit2 <- lm(co$pop ~ bs(co$age, knots = seq(20,80,20)))
pdat2 <- data.frame(x = seq(min(co$age), max(co$age)))
pdat2 <- transform(pdat2, yhat2 = predict(fit2, newdata = pdat2))
lines(yhat2 ~ x, data = pdat2, lwd = 2, col = 3)

fit3 <- lm(co$pop ~ bs(co$age, knots = c(24.5,49.0,73.5)))
pdat3 <- data.frame(x = seq(min(co$age), max(co$age)))
pdat3 <- transform(pdat3, yhat3 = predict(fit3, newdata = pdat3))
lines(yhat3 ~ x, data = pdat3, lwd = 2, col = 4)


legend("topright", legend = c("Knots = 25,50,70","Knots = 20,40,60,80","Knots = 24.5,49.0,73.5"),lty=c(1,1,1), col = c(2,3,4),lwd = 2)


