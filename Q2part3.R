Bordeaux <- read.csv(file = 'Bordeaux.csv')

#par(mfrow=c(1,2))
#plot(Bordeaux$ParkerPoints, Bordeaux$Price, xlab = "Parker Points", ylab = "Price")
#plot(Bordeaux$Pomerol, Bordeaux$Price, xlab = "Pomerol", ylab = "Price")

#lm.fit<-lm(Price~ParkerPoints+Pomerol,data=Bordeaux)
#summary(lm.fit)

#X=model.matrix(lm.fit) 
#H=X%*%solve(t(X)%*%X)%*%t(X) 

#r=lm.fit$residuals 
#sigmahat=sqrt(sum(r^2)/lm.fit$df.residual) 
#r0=r/(sigmahat*sqrt(1-diag(H))) 
#yhat=lm.fit$fitted.values 
#plot(yhat,r0,xlab="fitted values",ylab="standardised residuals") 
#abline(h=c(-2,0,2), lty=c(2,1,2))

lm.fit<-lm(log(Price)~ParkerPoints+Pomerol,data=Bordeaux)
summary(lm.fit)

X=model.matrix(lm.fit) 
H=X%*%solve(t(X)%*%X)%*%t(X) 

r=lm.fit$residuals 
sigmahat=sqrt(sum(r^2)/lm.fit$df.residual) 
r0=r/(sigmahat*sqrt(1-diag(H))) 
yhat=lm.fit$fitted.values 
plot(yhat,r0,xlab="fitted values",ylab="standardised residuals") 
abline(h=c(-2,0,2), lty=c(2,1,2))