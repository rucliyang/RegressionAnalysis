set.seed (777)
n <- 20
beta_0 <- 1
beta_1 <- 2
X <- rnorm (n , 2, 1)
X<-c(X,10)

epsilon <- rnorm (n+1 , 0, 1)
Y <- beta_0 + beta_1*X + epsilon
plot (X , Y,pch=19,xlim=c(0,10),ylim=c(0,21))
text(X[21],Y[21]-1.5,'A')
fit <- lm(Y ~ X)
cooks.distance(fit)
hatvalues(fit)
hatvalues(fit)>2*mean(hatvalues(fit))
abline(fit)
summary (fit)
plot(fit)
par(family='STKaiti')
plot(residuals.lm(fit),pch=19,lty=6,xlab = '样例编号',ylab = '残差');

rm(list=ls())
set.seed (777)
n <- 20
beta_0 <- 1
beta_1 <- 2
X <- rnorm (n , 2, 1)
XX<-c(X,5)
epsilon <- rnorm (n , 0, 1)
Y <- beta_0 + beta_1*X + epsilon
YY<-c(Y,1)
plot (X , Y,pch=19,xlim=c(0,5),ylim=c(0,15))
plot (XX , YY,pch=19,xlim=c(0,5),ylim=c(0,15),xlab='X',ylab='Y')
text(XX[21],YY[21]-1,'B')
fit<-lm(Y~X)

fit1 <- lm(YY ~ XX)
rstandard(fit1)
rstudent(fit1)*sqrt((21-1-2)/(21-1-1-rstudent(fit1)))
abline(fit1)
abline(fit)

