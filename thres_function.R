library(mvtnorm)
library(glmnet)
library(ncvreg)
set.seed(1234)
par(family='STSongti-SC-Regular', mar=c(5,5,4,2))

lambda=2
beta.true = c(-10:10)
beta.lasso = NULL
for(i in 1:length(beta.true)){
  beta.lasso[i] = sign(beta.true[i])*max(abs(beta.true[i])-lambda, 0)
}
plot(beta.true, beta.true, type="l", xlab=expression(beta), ylab=expression(hat(beta)), col=1)
points(beta.true, beta.lasso, type="l", lty=2, col=2, lwd=2)

a = 3.7
lambda = 2
beta.scad = rep(0, length(beta.true))
for(i in 1:length(beta.true)){
  if(abs(beta.true[i]) <= lambda){
    beta.scad[i] = sign(beta.true[i])*max(abs(beta.true[i])-lambda, 0)
  } 
  if(abs(beta.true[i]) > lambda & abs(beta.true[i]) <= a*lambda) {
    beta.scad[i] = ((a-1)*beta.true[i] - sign(beta.true[i])*a*lambda)/(a-2)
  }
  if((abs(beta.true[i]) > a*lambda)){
    beta.scad[i] = beta.true[i]
  }
}
points(beta.true, beta.scad, type="l", lty=3, col=3, lwd=2)

lambda=2
beta.true = c(-10:10)
beta.adalasso = NULL
for(i in 1:length(beta.true)){
  beta.adalasso[i] = sign(beta.true[i])*max(abs(beta.true[i])-abs(beta.true[i])^(-1)*lambda, 0)
}
points(beta.true, beta.adalasso, type="l", lty=4, col=4, lwd=2)
legend("bottomright", c("Lasso", "自适应Lasso", "SCAD"),
       lty=c(2,4,3), col=c(2,4,3), lwd=c(2,2,2), cex=0.8)



