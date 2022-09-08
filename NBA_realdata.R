NBA.a <- read.csv("/Users/lirong/Library/Mobile Documents/com~apple~CloudDocs/回归课件/大作业数据/应用回归分析+潘淑君/数据/原始数据/1819a.csv")
NBA.f <- read.csv("/Users/lirong/Library/Mobile Documents/com~apple~CloudDocs/回归课件/大作业数据/应用回归分析+潘淑君/数据/原始数据/1819f.csv")
duiqi = 0
duiqi.s = NULL
for(i in 1:500){
  duiqi = duiqi + sum(NBA.f$Player[i]==NBA.a$Player[i])
  duiqi.s = c(duiqi.s, sum(NBA.f$Player[i]==NBA.a$Player[i]))
}
NBA.f$Player[which(duiqi.s==0)]
NBA.a$Player[which(duiqi.s==0)]

NBA.f.screen <- NBA.f[, c("Tm", "Pos", "Age",
                          "MP", "PTS", "FG.", "X3P.", "X2P.", "FT.", "TRB", "AST", "STL", "BLK", "TOV", "PF")]

NBA.data <- data.frame(NBA.a$Player, NBA.f.screen, NBA.a$PER)
colnames(NBA.data)[1]="Player"
colnames(NBA.data)[ncol(NBA.data)]="PER"
NBA.data <- na.omit(NBA.data)

write.csv(NBA.data, file="/Users/lirong/Library/Mobile Documents/com~apple~CloudDocs/回归课件/书稿书稿书稿书稿/NBAData.csv")

#NBA.player.data <- NBA.data[,-c(1, 2, 3)]
#fit.lm <- lm(PER ~ ., data = NBA.player.data)
#summary(fit.lm)

set.seed(123456)
ind <- sample(1:nrow(NBA.data), 100, replace = F)
NBAer <- NBA.data[ind, -c(1,2,3)]
fit.lm <- lm(PER ~ ., data = NBAer)
summary(fit.lm)

#write.csv(NBAer, file="/Users/lirong/Library/Mobile Documents/com~apple~CloudDocs/回归课件/书稿书稿书稿书稿/chapter6rmd/NBAData.csv")

NBAer.screen <- NBAer[,c("MP", "PTS", "FG.", "TOV", "PER")]
fit.lm <- lm(PER ~ ., data = NBAer.screen)
summary(fit.lm)

library(leaps)
NBAer.screen.r2adj <- leaps(x=NBAer.screen[,-5], y=NBAer.screen[,5], method="adjr2")
NBAer.screen.r2adj
cbind(NBAer.screen.r2adj$which, adjRsq = NBAer.screen.r2adj$adjr2)
NBAer.screen.cp <- leaps(x=NBAer.screen[,-5], y=NBAer.screen[,5], method="Cp")
NBAer.screen.cp
cbind(NBAer.screen.cp$which, cp = NBAer.screen.cp$Cp)

x <- NBAer.screen[,-5]
y <- NBAer.screen[,5]
NBAer.screen.aic <- NBAer.screen.bic <- NULL
for(j in 1:nrow(NBAer.screen.r2adj$which)){
  x.subset <- as.matrix(x[,NBAer.screen.r2adj$which[j,]])
  lm.subset <- lm(y ~ x.subset)
  NBAer.screen.aic <- c(NBAer.screen.aic, AIC(lm.subset))
  NBAer.screen.bic <- c(NBAer.screen.bic, BIC(lm.subset))
}
cbind(NBAer.screen.cp$which, adjRsq = NBAer.screen.r2adj$adjr2, 
      aic = NBAer.screen.aic, bic = NBAer.screen.bic, cp = NBAer.screen.cp$Cp)

##Forward
add1(lm(PER~1,data=NBAer.screen), PER~MP+PTS+FG.+TOV, test="F")
add1(lm(PER~FG.,data=NBAer.screen), PER~MP+PTS+FG.+TOV, test="F")
add1(lm(PER~PTS+FG.,data=NBAer.screen), PER~MP+PTS+FG.+TOV, test="F")
add1(lm(PER~PTS+FG.+MP,data=NBAer.screen), PER~MP+PTS+FG.+TOV, test="F")

##Backward
drop1(lm(PER~MP+PTS+FG.+TOV,data=NBAer.screen), test="F")
drop1(lm(PER~MP+PTS+FG.,data=NBAer.screen), test="F")

##Stepwise
add1(lm(PER~1,data=NBAer.screen), PER~MP+PTS+FG.+TOV, test="F")
add1(lm(PER~FG.,data=NBAer.screen), PER~MP+PTS+FG.+TOV, test="F")
add1(lm(PER~PTS+FG.,data=NBAer.screen), PER~MP+PTS+FG.+TOV, test="F")
drop1(lm(PER~PTS+FG.,data=NBAer.screen), test="F")
add1(lm(PER~PTS+FG.+MP,data=NBAer.screen), PER~MP+PTS+FG.+TOV, test="F")
drop1(lm(PER~PTS+FG.+MP,data=NBAer.screen), test="F")

#variable selection
library(mvtnorm)
library(glmnet)
library(ncvreg)
set.seed(123456)

Y = NBAer[,14]
X = as.matrix(NBAer[,-14])
Y = Y - mean(Y)
X <- apply(X, 2, scale)
lasso.mod = glmnet(X, Y, alpha=1) #alpha=0  ridge
cv.lasso = cv.glmnet(X, Y, alpha=1)
coef(lasso.mod, s=cv.lasso$lambda.min)

enet.mod = glmnet(X, Y, alpha=0.5) #alpha=0  ridge
cv.enet = cv.glmnet(X, Y, alpha=0.5)
coef(enet.mod, s=cv.enet$lambda.min)

penalty.factor=abs(lm(Y ~ X)$coefficients[-1])^(-1)
adalasso.mod = glmnet(X, Y, alpha=1, penalty.factor = penalty.factor) #alpha=0  ridge
cv.adalasso = cv.glmnet(X, Y, alpha=1, penalty.factor = penalty.factor)
coef(adalasso.mod, s=cv.adalasso$lambda.min)

mcp.mod <- ncvreg(X, Y, penalty="MCP")  #MCP leads to less variables
cv.mcp <- cv.ncvreg(X, Y, penalty="MCP")
mcp.mod$beta[,cv.mcp$min]

scad.mod <- ncvreg(X, Y, penalty="SCAD")  #MCP leads to less variables
cv.scad <- cv.ncvreg(X, Y, penalty="SCAD")
scad.mod$beta[,cv.scad$mi n]


##MCB
library(mcb)
set.seed(123456)

mcb_adalasso <- mcb(X, Y, B=1000, method="aLasso", lambda = 0.05, family = "g", level=0.05)
mcb_lasso <- mcb(X, Y, B=1000, method="Lasso", lambda = 0.05, family = "g", level=0.05)
mcb_mcp <- mcb(X, Y, B=1000, method="MCP", lambda = 0.05, family = "g", level=0.05)
mcb_scad <- mcb(X, Y, B=1000, method="SCAD", lambda = 0.05, family = "g", level=0.05)
adalasso = mcb_adalasso$mcbframe
lasso = mcb_lasso$mcbframe
mcp = mcb_mcp$mcbframe
scad = mcb_scad$mcbframe
width = lasso$width/p
par(family='STSongti-SC-Regular')
plot(width, lasso$bcr, xlab="w/p", ylab="CR", type="b", col=1, lty=1, pch=1, ylim=c(0,1))
points(width, adalasso$bcr, type="b", col=2, lty=2, pch=2)
points(width, mcp$bcr, type="b", col=3, lty=3, pch=3)
points(width, scad$bcr, type="b", col=4, lty=4, pch=4)
legend("bottomright", c("Lasso", "自适应Lasso", "MCP", "SCAD"), 
       lty=c(1,2,3,4), pch=c(1,2,3,4), col=c(1,2,3,4), cex=0.8)
