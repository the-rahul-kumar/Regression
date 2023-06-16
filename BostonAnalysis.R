library(MASS)
lm.fit<-lm(medv~lstat,data=Boston)
confint(lm.fit)
qt(0.025,504,lower.tail=FALSE)