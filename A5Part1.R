bordeaux <- read.csv('Bordeaux.csv', header=T)
attach(bordeaux)

library(tidyverse)
library(ResourceSelection)


bordeaux.glm<-glm(P95andAbove ~ log(Price),family=binomial) 
summary(bordeaux.glm)

d0 <- bordeaux.glm$null.deviance
d1 <- bordeaux.glm$deviance

alpha <- 0.05

crit.val <- qchisq(1-alpha, df=1)
if(d0-d1 > crit.val){
  cat("We reject H0 at alpha=", alpha, "significance level.")
}else{
  cat("We cannot reject H0 at alpha=", alpha, "significance level.")
}


newdata <- data.frame(Price=seq(min(Price), max(Price),len=500))
newdata$P95andAbove = predict(bordeaux.glm, newdata, type="response")

plot(P95andAbove ~ log(Price))
lines(P95andAbove ~ log(Price), newdata, lwd=2)

pred <- ifelse(predict(bordeaux.glm, type = "response") > 0.5, 1, 0)
head(pred)

c_mat <- table(predicted = pred, actual = bordeaux$P95andAbove)
c_mat

accuracy <- sum(c_mat[1], c_mat[4]) / sum(c_mat[1:4])
accuracy*100

newprice = newdata <- data.frame(Price=c(500,600,850,1150))
newprice$P95andAbove = predict(bordeaux.glm, newprice, type="response")

hl <- hoslem.test(bordeaux.glm$y, fitted(bordeaux.glm), g=4)
hl
cbind(hl$observed,hl$expected)

