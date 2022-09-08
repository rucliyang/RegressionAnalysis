library(mvtnorm)
library(glmnet)
library(ncvreg)
set.seed(1234)

p = 10
n = 50
beta0 <- 1
beta.true = c(1,1/2,1/3,1/4,1/5,0,0,0,0,0)
X <- rmvnorm(n, mean=rep(0,p), sigma=diag(p))
epsilon <- rnorm(n)
Y <- X %*% beta.true + epsilon + beta0

lm(Y ~ X)$coefficients

ridge.mod = glmnet(X, Y, alpha=0) #alpha=0  ridge
cv.ridge = cv.glmnet(X, Y, alpha=0)
coef(ridge.mod, s=cv.ridge$lambda.min)

lasso.mod = glmnet(X, Y, alpha=1) #alpha=0  ridge
cv.lasso = cv.glmnet(X, Y, alpha=1)
coef(lasso.mod, s=cv.lasso$lambda.min)

penalty.factor=abs(lm(Y ~ X)$coefficients[-1])^(-1)
adalasso.mod = glmnet(X, Y, alpha=1, penalty.factor = penalty.factor) #alpha=0  ridge
cv.adalasso = cv.glmnet(X, Y, alpha=1, penalty.factor = penalty.factor)
coef(adalasso.mod, s=cv.adalasso$lambda.min)

scad.mod <- ncvreg(X, Y, penalty="SCAD")  #MCP leads to less variables
cv.scad <- cv.ncvreg(X, Y, penalty="SCAD")
scad.mod$beta[,cv.scad$min]

mcp.mod <- ncvreg(X, Y, penalty="MCP")  #MCP leads to less variables
cv.mcp <- cv.ncvreg(X, Y, penalty="MCP")
mcp.mod$beta[,cv.mcp$min]

enet.mod = glmnet(X, Y, alpha=0.5) #alpha=0  ridge
cv.enet = cv.glmnet(X, Y, alpha=0.5)
coef(enet.mod, s=cv.enet$lambda.min)

lasso2.mod <- ncvreg(X, Y, penalty="lasso")  #MCP leads to less variables
plot(lasso2.mod, lty=c(1:10), col=c(1:10))
legend("topleft", paste("X",1:10,sep=""), lty=c(1:10), col=c(1:10), cex=0.5)


########多次simulation########
library(mvtnorm)
library(glmnet)
library(ncvreg)
set.seed(1234)

p = 10
n = 50
beta0 <- 1
beta.true = c(1,1/2,1/3,1/4,1/5,0,0,0,0,0)
X <- rmvnorm(n, mean=rep(0,p), sigma=diag(p))

times = 100
ols <- ridge <- lasso <- adalasso <- scad <- mcp <- enet <- NULL
for(i in 1:times){
  epsilon <- rnorm(n)
  Y <- X %*% beta.true + epsilon + beta0
  ols <- rbind(ols, c(lm(Y ~ X)$coefficients))
  
  ridge.mod = glmnet(X, Y, alpha=0) #alpha=0  ridge
  cv.ridge = cv.glmnet(X, Y, alpha=0)
  ridge <- rbind(ridge, as.vector(coef(ridge.mod, s=cv.ridge$lambda.min)))
  
  lasso.mod = glmnet(X, Y, alpha=1) #alpha=0  ridge
  cv.lasso = cv.glmnet(X, Y, alpha=1)
  lasso <- rbind(lasso, as.vector(coef(lasso.mod, s=cv.lasso$lambda.min)))
  
  penalty.factor=abs(lm(Y ~ X)$coefficients[-1])^(-1)
  adalasso.mod = glmnet(X, Y, alpha=1, penalty.factor = penalty.factor) #alpha=0  ridge
  cv.adalasso = cv.glmnet(X, Y, alpha=1, penalty.factor = penalty.factor)
  adalasso <- rbind(adalasso, as.vector(coef(adalasso.mod, s=cv.adalasso$lambda.min)))
  
  scad.mod <- ncvreg(X, Y, penalty="SCAD")  #MCP leads to less variables
  cv.scad <- cv.ncvreg(X, Y, penalty="SCAD")
  scad <- rbind(scad, c(scad.mod$beta[,cv.scad$min]))
  
  mcp.mod <- ncvreg(X, Y, penalty="MCP")  #MCP leads to less variables
  cv.mcp <- cv.ncvreg(X, Y, penalty="MCP")
  mcp <- rbind(mcp, c(mcp.mod$beta[,cv.mcp$min]))
  
  enet.mod = glmnet(X, Y, alpha=0.5) #alpha=0  ridge
  cv.enet = cv.glmnet(X, Y, alpha=0.5)
  enet <- rbind(enet, as.vector(coef(enet.mod, s=cv.enet$lambda.min)))
}
apply(ols, 2, mean)
apply(ridge, 2, mean)
apply(lasso, 2, mean)
apply(scad, 2, mean)
apply(mcp, 2, mean)
apply(adalasso, 2, mean)
apply(enet, 2, mean)

