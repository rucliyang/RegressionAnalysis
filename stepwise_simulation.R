library(mvtnorm)
##生成数据##
set.seed(1234)
n <- 20
p <- 10
beta <- c(1, 2, -1, 1, -1.5, -0.01, 0.2, 0.1, 0.01, 0.0001)
X <- rmvnorm(n, mean = rep(0,p), sigma = diag(p))
colnames(X) <- paste("X", 1:p, sep = "")
epsilon <- rnorm(n)
Y <- X %*% beta + epsilon
data.sim <- data.frame(Y,X)
##前进法##
forreg <- step(lm(Y~1, data=data.sim), scope = list(
  upper=~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, lower=~1), direction = "forward")
##后退法##
backreg <- step(lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data=data.sim))
##逐步回归##
stepreg <- step(lm(Y~1, data=data.sim), scope = list(
  upper=~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, lower=~1))

##生成数据##
Sigma.matrix <- matrix(0.5, p, p)
diag(Sigma.matrix) <- 1
X <- rmvnorm(n, mean = rep(0,p), sigma = Sigma.matrix)
colnames(X) <- paste("X", 1:p, sep = "")
epsilon <- rnorm(n)
Y <- X %*% beta + epsilon
data.sim <- data.frame(Y,X)
##前进法##
forreg <- step(lm(Y~1, data=data.sim), scope = list(
  upper=~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, lower=~1), direction = "forward")
##后退法##
backreg <- step(lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data=data.sim))
##逐步回归##
stepreg <- step(lm(Y~1, data=data.sim), scope = list(
  upper=~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, lower=~1))


