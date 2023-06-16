library(ISLR) 
help(Carseats)
data <- data("Carseats")

nrow(Carseats)
ncol(Carseats)

round(mean(Carseats$Income),2)
round(sd(Carseats$Population),2)
round(median(Carseats$Price),2)

#plot(Carseats$Price, Carseats$Sales)

hist(Carseats$Sales)

mu.hat <- function(y){
  ( sum(y) / length(y) )
}

result1 <- mu.hat(y=Carseats$Sales)
result1
round(result1,3)

var.hat <- function(y, mu2){
  ( (sum(y^2) - (2*mu2*sum(y)) + (length(y)*mu2^2) )/ length(y) )
}

CSM <- mean(Carseats$Sales)
result2 <- var.hat(y=Carseats$Sales, mu2 = CSM)
result2
round(result2,3)