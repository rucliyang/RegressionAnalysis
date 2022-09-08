library(mcb)
library(mvtnorm)

set.seed(1234)
par(family='STSongti-SC-Regular')
n = 100
p = 10
beta.true = c(1,1,1,1,1,0,0,0,0,0)
X <- rmvnorm(n, mean=rep(0,p), sigma=diag(p))
epsilon = rnorm(n)
Y <- X %*% beta.true + epsilon

mcb_adalasso <- mcb(X, Y, B=1000, method="aLasso", lambda = 0.05, family = "g", level=0.05)
mcb_lasso <- mcb(X, Y, B=1000, method="Lasso", lambda = 0.05, family = "g", level=0.05)
mcb_mcp <- mcb(X, Y, B=1000, method="MCP", lambda = 0.05, family = "g", level=0.05)
mcb_scad <- mcb(X, Y, B=1000, method="SCAD", lambda = 0.05, family = "g", level=0.05)
adalasso = mcb_adalasso$mcbframe
lasso = mcb_lasso$mcbframe
mcp = mcb_mcp$mcbframe
scad = mcb_scad$mcbframe
width = lasso$width/p
plot(width, lasso$bcr, xlab="w/p", ylab="CR", type="b", lty=1, pch=1, col=1, ylim=c(0,1))
points(width, adalasso$bcr, type="b", lty=2, pch=2, col=2)
points(width, mcp$bcr, type="b", lty=3, pch=3, col=3)
points(width, scad$bcr, type="b", lty=4, pch=4, col=4)
legend("bottomright", c("Lasso", "自适应Lasso", "MCP", "SCAD"), 
       lty=c(1,2,3,4), pch=c(1,2,3,4), col=c(1,2,3,4), cex=0.8)

