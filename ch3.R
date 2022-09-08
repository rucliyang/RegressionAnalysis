# data generating.
options(digits=10)
#n = 20
#set.seed(100)
## 数据生成
# 先在R软件上生成数据。
# n = 20
# options(digits=3)
# set.seed(100) #设定随即种子

# x1 = seq(1,10,length.out = n)+rnorm(n, mean = 0.3, sd = 1)
# x2 = 1000*seq(10,20,length.out = n)+rnorm(n, mean = 0.5, sd = 1)
# x3 = seq(30,40,length.out = n)+rnorm(n, mean = 0.2, sd = 1)
# eps = rnorm(n, mean = 0, sd = 1)
# y = 3.0*x1+0.018*x2+5.0*x3 + eps
# dat = data.frame(x1,x2,x3,y)
# dat
## 观察自变量与因变量线性关系
#&emsp;&emsp;利用表中数据，选取三个自变量中的两个和因变量一同绘制三维图型。从下图可以看出，各元素含量与该农作物的收成之间具有明显的线性关系。
# data visualization.
# library(scatterplot3d)
# scatterplot3d(x1,x2,y,zlim = c(300,600))
# title('(a)', line = -35)
# scatterplot3d(x1,x3,y,zlim = c(300,600))
# title('(b)', line = -35)
# scatterplot3d(x2,x3,y,zlim = c(300,600))
# title('(c)', line = -35)
library(GGally)
library(ggplot2)
dat <- read_excel("/Aviation.xlsx")
ggpairs(data = dat, columns=1:4,aes()) # 绘制矩阵散点图


# modelling and testing.
attach(dat)
mod = lm(y~x1+x2+x3)
summary(mod)
n = length(y)
y_hat = mod$coefficients[1] + 
  mod$coefficients[2]*x1 + 
  mod$coefficients[3]*x2 +
  mod$coefficients[4]*x3

SSE = sum((y - y_hat)^2)
SSE
# partial F test.
mod_x1 = lm(y~x2+x3)
summary(mod_x1)

y1_hat = mod_x1$coefficients[1] + 
  mod_x1$coefficients[2]*x2 + 
  mod_x1$coefficients[3]*x3

# SSE1 = sum((y - y1_hat)^2)
# SSE1
SSR1 = sum((y1_hat - mean(y))^2)
SSR1

# (SSE1 - SSE)/SSE1
(SSR - SSR1)/(SSE/(n-3-1))
# alternative method.
anova(mod, mod_x1)

mod_x2 = lm(y~x1+x3)
summary(mod_x2)
anova(mod, mod_x2)

mod_x3 = lm(y~x1+x2)
summary(mod_x3)
anova(mod, mod_x3)

# standardization procedure.
sx1 = (x1 - mean(x1))/sqrt(sum((x1 - mean(x1))^2))
sx2 = (x2 - mean(x2))/sqrt(sum((x2 - mean(x2))^2))
sx3 = (x3 - mean(x3))/sqrt(sum((x3 - mean(x3))^2))
sy =  (y - mean(y))/sqrt(sum((y - mean(y))^2))
sdat = data.frame(sx1, sx2, sx3, sy)
sdat

smod = lm(sy~sx1+sx2+sx3)
summary(smod)

# partial coefficient of determination and partial correlation
r_sqr1 = (sum((y - mod_x1$fitted.values)^2) - sum((y - mod$fitted.values)^2))/(sum((y - mod_x1$fitted.values)^2))
r_sqr2 = (sum((y - mod_x2$fitted.values)^2) - sum((y - mod$fitted.values)^2))/(sum((y - mod_x2$fitted.values)^2))
r_sqr3 = (sum((y - mod_x3$fitted.values)^2) - sum((y - mod$fitted.values)^2))/(sum((y - mod_x3$fitted.values)^2))
r_sqr1
r_sqr2
r_sqr3

sqrt(r_sqr1)
sqrt(r_sqr2)
sqrt(r_sqr3)

library(GGally)
library(ggplot2)
data = read.csv('/Users/rayner/Desktop/所有文件/data.csv')
ggpairs(data=data, columns=1:9,aes()) # 绘制矩阵散点图
