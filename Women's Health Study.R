
data <- data.frame(Y=c(0,1,1,0,2,3,0,1,1,1,1,2,0,1,3,0,1,2,1,3,3,4,1,3,2,0,
                        2,0,3,0,0,1,1,1,1,0,0,2,2,0,1,2,0,0,1,1,1,0,2),
                   J=c(rep(1,26),rep(2,23)) )
# Mean of the groups
mean(data[data$J==1,1])
mean(data[data$J==2,1])

# Var of the groups
var(data[data$J==1,1])
var(data[data$J==2,1])

# SD of the groups
sd(data[data$J==1,1])
sd(data[data$J==2,1])


#Under H0
# MLE under null hypothesis
theta.hat0 <- mean(data$Y)
# Value of the Maximum Likelyhood
sum(log(dpois(data$Y, lambda = theta.hat0)))

#Under H1

# MLE under alternative hypothesis
theta.hat1 <- mean(data[data$J==1,1])
theta.hat1
theta.hat2 <- mean(data[data$J==2,1])
theta.hat2

sum(log(dpois(data[data$J==1,1], lambda = theta.hat1)))
sum(log(dpois(data[data$J==2,1], lambda = theta.hat2)))

sum(log(dpois(data[data$J==1,1], lambda = theta.hat1)))+sum(log(dpois(data[data$J==2,1], lambda = theta.hat2)))

#Under H0

r0 <- (data$Y - theta.hat0)/sqrt(theta.hat0)
r0 <- round(r0,2)

r0.town <- r0[data$J==1]
r0.country <- r0[data$J==2]

par(mfrow=c(1,2), oma = c(0,0,2,0))
plot(table(r0.town), xlab="r", ylab="Town")
plot(table(r0.country), xlab="r", ylab="Country")
mtext("Model Under H0", outer=TRUE, cex=1.5)

#Under H1

r1.town <- (data[data$J==1,1] - theta.hat1)/sqrt(theta.hat1)
r1.town <- round(r1.town,2)

r1.country <- (data[data$J==2,1] - theta.hat2)/sqrt(theta.hat2)
r1.country <- round(r1.country,2)

par(mfrow=c(1,2), oma = c(0,0,2,0))
plot(table(r1.town), xlab="r", ylab="Town")
plot(table(r1.country), xlab="r", ylab="Country")
mtext("Model Under H1", outer=TRUE, cex=1.5)