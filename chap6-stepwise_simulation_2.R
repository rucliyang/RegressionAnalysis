library(mvtnorm)
##生成数据##
set.seed(1234)
n <- 100
p <- 10
beta <- c(1, 2, -1, 1, -1.5, -0.01, 0.5, -0.5, 0.01, 0.0001)
X <- rmvnorm(n, mean = rep(0,p), sigma = diag(p))
colnames(X) <- paste("X", 1:p, sep = "")
epsilon <- rnorm(n, mean = 0, sd = 1.5)
Y <- X %*% beta + epsilon
data.sim <- data.frame(Y,X)
##前进法##
forreg <- step(lm(Y~1, data=data.sim), scope = list(
  upper=~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, lower=~1), direction = "forward")

add1(lm(Y~1,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
add1(lm(Y~X2,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
add1(lm(Y~X2+X5,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
add1(lm(Y~X2+X5+X4,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
add1(lm(Y~X2+X5+X4+X1,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
add1(lm(Y~X2+X5+X4+X1+X3,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
add1(lm(Y~X2+X5+X4+X1+X3+X8,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
add1(lm(Y~X2+X5+X4+X1+X3+X8+X7,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")


##后退法##
backreg <- step(lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data=data.sim))

drop1(lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data = data.sim), test="F")
drop1(lm(Y ~ X1+X2+X3+X4+X5+X7+X8+X9+X10, data = data.sim), test="F")
drop1(lm(Y ~ X1+X2+X3+X4+X5+X7+X8+X10, data = data.sim), test="F")
drop1(lm(Y ~ X1+X2+X3+X4+X5+X7+X8, data = data.sim), test="F")

##逐步回归##
stepreg <- step(lm(Y~1, data=data.sim), scope = list(
  upper=~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, lower=~1))

add1(lm(Y~1,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
add1(lm(Y~X2,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
add1(lm(Y~X2+X5,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
drop1(lm(Y~X2+X5,data = data.sim), test="F")
add1(lm(Y~X2+X5+X4,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
drop1(lm(Y~X2+X5+X4,data = data.sim), test="F")
add1(lm(Y~X2+X5+X4+X1,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
drop1(lm(Y~X2+X5+X4+X1,data = data.sim), test="F")
add1(lm(Y~X2+X5+X4+X1+X3,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
drop1(lm(Y~X2+X5+X4+X1+X3,data = data.sim), test="F")
add1(lm(Y~X2+X5+X4+X1+X3+X8,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
drop1(lm(Y~X2+X5+X4+X1+X3+X8,data = data.sim), test="F")
add1(lm(Y~X2+X5+X4+X1+X3+X8+X7,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
drop1(lm(Y~X2+X5+X4+X1+X3+X8+X7,data = data.sim), test="F")


##生成数据##
set.seed(1234)
n <- 100
p <- 10
beta <- c(1, 2, -1, 1, -1.5, -0.01, 0.5, -0.5, 0.01, 0.0001)
Sigma.matrix <- matrix(NA, p, p)
for(i in 1:p){
  for(j in 1:p){
    Sigma.matrix[i,j] <- 0.5^abs(i-j)
  }
}
#Sigma.matrix <- matrix(0.6, p, p)
#diag(Sigma.matrix) <- 1
X <- rmvnorm(n, mean = rep(0,p), sigma = Sigma.matrix)
colnames(X) <- paste("X", 1:p, sep = "")
epsilon <- rnorm(n, mean = 0, sd = 1.5)
Y <- X %*% beta + epsilon
data.sim <- data.frame(Y,X)
##前进法##
forreg <- step(lm(Y~1, data=data.sim), scope = list(
  upper=~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, lower=~1), direction = "forward")

add1(lm(Y~1,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
add1(lm(Y~X2,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
add1(lm(Y~X2+X5,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
add1(lm(Y~X2+X5+X1,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
add1(lm(Y~X2+X5+X1+X4,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
add1(lm(Y~X2+X5+X1+X4+X3,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")

##后退法##
backreg <- step(lm(Y~X1+X2+X3+X4+X5+X6+X7+X8, data=data.sim))

drop1(lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data=data.sim), test="F")
drop1(lm(Y ~ X1+X2+X3+X4+X5+X7+X8+X9+X10, data=data.sim), test="F")
drop1(lm(Y ~ X1+X2+X3+X4+X5+X7+X8+X10, data=data.sim), test="F")
drop1(lm(Y ~ X1+X2+X3+X4+X5+X7+X8, data=data.sim), test="F")

##逐步回归##
stepreg <- step(lm(Y~1, data=data.sim), scope = list(
  upper=~X1+X2+X3+X4+X5+X6, lower=~1))

add1(lm(Y~1,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
add1(lm(Y~X2,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
add1(lm(Y~X2+X5,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
drop1(lm(Y~X2+X5,data = data.sim), test="F")
add1(lm(Y~X2+X5+X1,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
drop1(lm(Y~X2+X5+X1,data = data.sim), test="F")
add1(lm(Y~X2+X5+X1+X4,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
drop1(lm(Y~X2+X5+X1+X4,data = data.sim), test="F")
add1(lm(Y~X2+X5+X1+X4+X3,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, test="F")
drop1(lm(Y~X2+X5+X1+X4+X3,data = data.sim), test="F")

## Toy example for stepwise $\alpha_{in}$ and $\alpha_{out}$
set.seed(1234)
n <- 100
p <- 6
beta <- c(0.5, 1, 0.1, -1, 0.1, -0.5)
X <- rmvnorm(n, mean = rep(0,p), sigma = diag(p))
colnames(X) <- paste("X", 1:p, sep = "")
epsilon <- rnorm(n, mean = 0, sd = 1.5)
Y <- X %*% beta + epsilon
data.sim <- data.frame(Y,X)

add1(lm(Y~1,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6, test="F")
qf(0.9, 1, 98); qf(0.95, 1, 98)
add1(lm(Y~X4,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6, test="F")
qf(0.9, 1, 97); qf(0.95, 1, 97)
add1(lm(Y~X4+X2,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6, test="F")
drop1(lm(Y~X4+X2,data = data.sim), test="F")
qf(0.9, 1, 96); qf(0.95, 1, 96)
add1(lm(Y~X4+X2+X6,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6, test="F")
drop1(lm(Y~X4+X2+X6,data = data.sim), test="F")
qf(0.9, 1, 95); qf(0.95, 1, 95)
add1(lm(Y~X4+X2+X6+X1,data = data.sim), Y ~ X1+X2+X3+X4+X5+X6, test="F")
drop1(lm(Y~X4+X2+X6+X1,data = data.sim), test="F")
