## 一元线性回归案例分析

# 成绩案例

## 数据

Y <- c(95,72,35,80,66,50,0,98,72,0,90,55,79,0,75,77,95,66)
X <- c(96,89,0,77,47,30,0,90,59,0,93,77,78,18,74,64,86,67)


## 初步分析
# 做散点图查看数据的分布情况：
plot(X,Y,xlab='家庭作业分数',ylab='考试分数',xlim=c(0,100),ylim=c(0,100))
fit <- lm(Y~X)
abline(fit)

## 拟合一元回归模型

#用R软件内置的函数$lm$来进行一元回归模型的拟合

# 拟合一元回归模型
fit <- lm(Y ~ X) #拟合模型
summary(fit) #模型拟合结果

#模型拟合效果
plot(X, Y)
abline(fit)


## 预测
# 给定变量$X$的新值$x_0=75$得到预测结果

x0 <- 75
beta <- fit$coefficients
# 新值的单值预测
y0 <- cbind(1,x0)%*%beta
n <- length(Y)
Y_hat <- cbind(1,X)%*%beta
Lxx <- sum((X-mean(X))^2)
sigma <- sqrt(1/(n-2)*sum((Y-Y_hat)^2))
h00 <- 1/n+(x0-mean(X))^2/Lxx
# 新值的区间预测
lwr1 <- y0-qt((1-0.05/2),n-2,lower.tail=T)*sqrt(1+h00)*sigma
upr1 <- y0+qt((1-0.05/2),n-2,lower.tail=T)*sqrt(1+h00)*sigma
# 新值平均值的区间预测
lwr2 <- y0-qt((1-0.05/2),n-2,lower.tail=T)*sqrt(h00)*sigma
upr2 <- y0+qt((1-0.05/2),n-2,lower.tail=T)*sqrt(h00)*sigma

# 例 2.1：一元线性回归分析步骤

## 数据生成
# 先在R软件上生成数据。首先确定模型为一元线性回归模型：
set.seed(777)
n <- 100
beta_0 <- 1
beta_1 <- 2
X <- rnorm(n, 2, 1)
epsilon <- rnorm(n, 0, 1)
Y <- beta_0 + beta_1*X + epsilon

## 初步分析

#做散点图查看数据的分布情况：
plot(X, Y)

# 拟合一元回归模型
fit <- lm(Y ~ X) #拟合模型
summary(fit) #模型拟合结果

# 用beta_0, beta_1的显示解进行估计
hat_beta1 <- sum((X-mean(X))*(Y-mean(Y)))/sum((X-mean(X))^2)
hat_beta0 <- mean(Y) - hat_beta1*mean(X)

#模型拟合效果
plot(X, Y)
abline(fit)

## 回归诊断，分析输出结果

#三种显著性检验的关系
summary(fit) #模型拟合结果
r <- cor(Y,X)
sqrt(n-2)*r/sqrt(1-r^2)

# 1. 独立性
library(lmtest)
dwtest(fit) 
# P-value大于0.05，所以不具有自相关性，
# 在正态假设前提下，具有独立性
# 2. 正态性
# Kolmogorov-Smirnov 检验
Y_hat <- cbind(1,X)%*%fit$coef
e <- Y - Y_hat
ks.test(e,rnorm(n,0,1)) #符合标准正态分布
# 3. 残差分析
e <- Y-Y_hat
plot(X,e,ylim=c(-4,4))
abline(h=3, lty = 'dashed', lwd=2)
abline(h=-3, lty = 'dashed', lwd=2)

## 区间估计

# 区间估计
Lxx <- sum((X-mean(X))^2)
sigma <- sqrt(1/(n-2)*sum((Y-Y_hat)^2))
conf.int=function(beta0,beta1,sigma,alpha) {
  t=qt(1-alpha/2,n-2,lower.tail = T)
  beta1_conf <- c(beta1-sigma*t/sqrt(Lxx),
                  beta1+sigma*t/sqrt(Lxx))
  beta0_conf <- c(beta0-sigma*t*sqrt((1/n)+((mean(X)^2)/Lxx)),
                  beta0+sigma*t*sqrt((1/n)+((mean(X)^2)/Lxx)))
  return(list(conf0=beta0_conf,conf1=beta1_conf))
}
conf <- conf.int(hat_beta0,hat_beta1,sigma,0.05)
conf_beta0 <- conf$conf0
conf_beta1 <- conf$conf1

## 预测

# 预测 给定预测新值 x0=4
x0 <- 4
beta <- fit$coefficients
y0 <- cbind(1,x0)%*%beta
n <- length(Y)
Y_hat <- cbind(1,X)%*%beta
Lxx <- sum((X-mean(X))^2)
sigma <- sqrt(1/(n-2)*sum((Y-Y_hat)^2))
h00 <- 1/n+(x0-mean(X))^2/Lxx

# 新值的区间预测
lwr1 <- y0-qt((1-0.05/2),n-2,lower.tail=T)*sqrt(1+h00)*sigma
upr1 <- y0+qt((1-0.05/2),n-2,lower.tail=T)*sqrt(1+h00)*sigma

# 新值平均值的区间预测
lwr2 <- y0-qt((1-0.05/2),n-2,lower.tail=T)*sqrt(h00)*sigma
upr2 <- y0+qt((1-0.05/2),n-2,lower.tail=T)*sqrt(h00)*sigma


# 例 2.2：模拟数据验证估计参数性质

set.seed(7777)
nSimu <- 1000
n <- 1000 #样本量，可改为100、500等
beta0 <- 1
beta1 <- 2

beta_cov <- rep(0, nSimu)
beta_var <- matrix(0, nr = nSimu, nc = 2)
beta_hat <- matrix(0, nr = nSimu, nc = 2)

beta0_conf <- matrix(0, nr = nSimu, nc = 2)
beta1_conf <- matrix(0, nr = nSimu, nc = 2)


for(k in 1:nSimu){
  X   <- rnorm(n, 2, 1) 
  
  Lxx <- sum((X - mean(X))^2)
  eps       <- rnorm(n, 0, 1)
  Y         <- beta0 + beta1 * X + eps
  fit       <- lm(Y ~ X)
  Y_hat     <- cbind(1, X) %*% fit$coef
  sigma_hat <- sqrt(sum((Y - Y_hat)^2)/(n - 2))
  
  # 保存每次的的回归系数估计值
  beta_hat[k, ] <- fit$coef 
  
  # 保存每次的置信区间上下界
  conf           <- conf.int(beta_hat[k, 1], beta_hat[k, 2], sigma_hat, 0.05)
  beta0_conf[k,] <- conf$conf0 
  beta1_conf[k,] <- conf$conf1
  
  # 保存每次估计参数的方差的估计值
  beta_var[k, 1] <- (1/n + mean(X)^2/Lxx) * sigma_hat^2
  beta_var[k, 2] <- sigma_hat^2/Lxx
  beta_cov[k]    <- (-mean(X)/Lxx) * sigma_hat^2
}

# 验证无偏性
if.unbias           <- matrix(0, nr = 2, nc = 2)
if.unbias[1, ]      <- c(beta0, beta1)
if.unbias[2, ]      <- colMeans(beta_hat)
colnames(if.unbias) <- paste0('beta', c(0, 1))
rownames(if.unbias) <- c('true beta', 'mean of beta_hat')
if.unbias

# 验证参数估计的方差接近理论方差
var_compare           <- matrix(0, nr = 3, nc = 2)
var_compare[1, ]      <- apply(beta_hat, 2, var)
var_compare[2, ]      <- colMeans(beta_var)
var_compare[3, ]      <- c((1/n + mean(X)^2/Lxx), 1/Lxx)
colnames(var_compare) <- c('beta0', 'beta1')
rownames(var_compare) <- c('empirical', 'mean', 'theoretical')
var_compare

cover.rate <- c(sum(beta0_conf[,1] < beta0 & beta0 < beta0_conf[,2])/nSimu, 
                sum(beta1_conf[,1] < beta1 & beta1 < beta1_conf[,2])/nSimu)
cover.rate
