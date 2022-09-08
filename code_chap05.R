###################例5.2
library(MASS)
run=200;u=20
R.seq=seq(0.1,0.9,length.out = u)
n=1000
a=0  #X的均值
rho=0.5;rho2=0.7;rho0=0.2  #不同相关性
n0=300  #测试集样本量
P=2 #变量维数
beta<-rep(1,P)
pe.const=pe.trans=pe.low=NULL
for(i in 1:u){
  R2=R.seq[i]
  err=err0=err1=NULL
  
  for(z in 1:run){
    Sigma0<-matrix(c(1,rho0,rho0,1),2,2,byrow = T)
    var_model<-t(beta)%*%Sigma0%*%beta
    sigma0.1<-as.numeric((var_model*(1-R2))/R2)
    X0<-mvrnorm(n=n,mu=rep(a,P),Sigma = Sigma0)
    e0<-rnorm(n=n,mean = 0,sd=sqrt(sigma0.1))
    Y0<-X0%*%beta+e0
    data0<-data.frame(Y0,X0)
    flag0=sample(1:n,0.7*n,replace = F)
    data.train0=data0[flag0,]
    data.test0=data0[-flag0,]
    data.train0=as.data.frame(apply(data.train0,2,FUN=function(x){scale(x,center = T,scale=F)}))
    data.test0=apply(data.test0,2,FUN=function(x){scale(x,center = T,scale=F)})
    mod0=lm(Y0~.,data=data.train0)
    err0=c(err0,mean((data.test0[,1]-data.test0[,-1]%*%mod0$coefficients[-1])^2))
    
    
    Sigma1<-matrix(c(1,rho,rho,1),2,2,byrow = T)
    var_model<-t(beta)%*%Sigma1%*%beta
    sigma1.1<-as.numeric((var_model*(1-R2))/R2)
    X1<-mvrnorm(n=n,mu=rep(a,P),Sigma = Sigma1)
    e1<-rnorm(n=n,mean = 0,sd=sqrt(sigma1.1))
    Y1<-X1%*%beta+e1
    data1<-data.frame(Y1,X1)
    flag1=sample(1:n,0.7*n,replace = F)
    data.train1=data1[flag1,]
    data.test1=data1[-flag1,]
    data.train1=as.data.frame(apply(data.train1,2,FUN=function(x){scale(x,center = T,scale=F)}))
    data.test1=apply(data.test1,2,FUN=function(x){scale(x,center = T,scale=F)})
    mod1=lm(Y1~.,data=data.train1)
    err1=c(err1,mean((data.test1[,1]-data.test1[,-1]%*%mod1$coefficients[-1])^2))
    
    Sigma2<-matrix(c(1,rho2,rho2,1),2,2,byrow = T)
    var_model<-t(beta)%*%Sigma2%*%beta
    sigma2.1<-as.numeric((var_model*(1-R2))/R2)
    X2<-mvrnorm(n=n0,mu=rep(a,P),Sigma = Sigma2)
    e2<-rnorm(n=n0,mean = 0,sd=sqrt(sigma2.1))
    Y2<-X2%*%beta+e2
    testdata=cbind(Y2,X2)
    
    err=c(err,mean((testdata[,1]-testdata[,-1]%*%mod1$coefficients[-1])^2))
    
  }
  (pe.const=c(pe.const,mean(err1)))
  (pe.trans=c(pe.trans,mean(err)))
  (pe.low=c(pe.low,mean(err0)))
}
data.pre=data.frame(PE=c(pe.const,pe.trans,pe.low),method=c(rep("rho=0.5",u),rep("rho=0.7",u),rep("rho=0.2",u)),
                    SNR=rep(R.seq,3))

###作图
library(ggplot2)
p=ggplot(data=data.pre,aes(SNR,PE,group=method,color=method,shape=method))+geom_line()+geom_point()
p=p+theme(legend.position=c(0.9,0.9))+labs(x="R square")
p



###################例 5.1

## 输入数据

y=c(10227,10872,11350,8775,8539,9994,11046,11164,10559,8979,8535,7980,9179,10394,11039,11450)/100
x1=c(112,111,111.1,117.5,127.6,135.7,142.9,153.8,166,179.3,190.2,197.6,202.6,208.5,215.2,224.4)
x2=c(121.3,125.3,133.1,147.7,161.2,170.5,181.5,195.3,217.7,247,272.3,286.6,297.4,307.6,318.5,323.4)
x3=c(776.8,839.6,949.8,1038.4,1142.8,1252.6,1379.3,1551.2,1729.3,1918,2127.6,2261.4,2428.1,2670.6,2841.1,3022.1)/10
x4=c(4.89,4.55,7.38,8.61,6.16,5.22,5.5,7.78,10.25,11.28,13.73,11.2,8.69,9.65,7.75,6.31)
x5=c(79637,82153,85064,86794,85846,88752,92017,96048,98824,99303,100397,99526,100834,105005,107150,109597)/1000

data.use=data.frame(y,x1,x2,x3,x4,x5)##合并为数据框

head(data.use) #显示前6条记录

## 可视化探索


library(GGally)
p=ggpairs(data.use)
p

## 拟合模型
data.use=as.data.frame(apply(data.use,2,FUN=function(x){scale(x,center = T,scale=T)}))

model1=lm(data.use$y~.,data=data.use)#拟合模型

summary(model1)##总结模型，查看结果


## 协方差矩阵判定法

cor(data.use[,-1])

## 方差膨胀因子判定法

data.use2=data.use[,-1]
model2=lm(x1~.,data=data.use2)
summary(model2)

(v1=1/(1-0.996))


model3=lm(x2~.,data=data.use2)
summary(model3)

(v2=1/(1-0.9977))

(model4=lm(x3~.,data=data.use2))
summary(model4)

(v3=1/(1-0.9957))

model5=lm(x4~.,data=data.use2)
summary(model5)

(v4=1/(1-0.7976))

model6=lm(x5~.,data=data.use2)
summary(model6)

(v5=1/(1-0.9756))

(vif=c(v1,v2,v3,v4,v5))
mean(vif)


## 特征根判定法

xx=cbind(1,data.use[,-1])
xx=scale(xx,center = F,scale=apply(xx,2,function(x) sqrt(sum(x^2))))
myeigen=eigen(crossprod(xx))
eigenval=myeigen$values
round(sqrt(max(eigenval)/eigenval),2)


## 岭回归估计

library(MASS)
lambda =seq(0,30,0.001)
model.r=lm.ridge(y~.,data=data.use,lambda =lambda)
df=data.frame(t(model.r$coef),(model.r$lambda))
colnames(df)=c("x1","x2","x3","x4","x5","lambda")
dat=data.frame(系数估计=c(df$x1,df$x2,df$x3,df$x4,df$x5),岭参数k=c(df$lambda,
                                                            df$lambda,df$lambda,df$lambda,df$lambda),
                   变量名称=c(rep("新车消费价格指数",length(lambda)),
                          rep("消费者价格指数",length(lambda)),
                          rep("个人可支配收入",length(lambda)),rep("利率",length(lambda)),
                          rep("就业劳动人数",length(lambda))))


g=ggplot(dat,aes(x=岭参数k,y=系数估计,lty=变量名称))+geom_line()+theme(legend.position = c(0.9,0.9))

g=g+theme(legend.position = c(0.9,0.9))+geom_vline(xintercept =5,lty=4)
g

## 结果

model.rf=lm.ridge(y~.,data=data.use,lambda =5)
model.rf


###################数值模拟

library(ggplot2)
library(MASS)
set.seed(123)
run=1000
n=200
P=5
ols=ridge=NULL
for(m in 1:run){
  x1=rnorm(n,1,2)
  e=rnorm(n,1,2)
  e1=rnorm(n,1,2)
  x2=runif(n,-0.5,0.5)
  x3=0.9*x1+0.3*x2+e
  x4=0.5*x2+e1
  X=cbind(x1,x2,x3,x4)
  beta=rep(1,4)
  ep=rnorm(n,2,4)
  y=X%*%beta+ep
  yn=scale(y,center = T,scale=F)
  Xn=apply(X,2,FUN = function(x){scale(x,center = TRUE,scale = F)})
  m1=lm(yn~Xn)
  ols=rbind(ols,m1$coefficients[-1])
  m2=lm.ridge(y~X,lambda =seq(0,10000,1))
  m3=lm.ridge(y~X,lambda =m2$lambda[m2$GCV==min(m2$GCV)])
  ridge=rbind(ridge,m3$coef)
}
colnames(ols)=paste0("x",1:(4))
colnames(ridge)=paste0("x",1:(4))
ols1=as.data.frame(ols)
ridge1=as.data.frame(ridge)
df=data.frame(coef.est=c(ols1$x2,ridge1$x2),methods=c(rep("最小二乘估计",1000),rep("岭估计",1000)))

##作图
g=ggplot(data=df,aes(coef.est,lty=methods))+geom_density()+xlim(-2.6,4.5)+ylim(0,1.4)
g=g+xlab("估计系数beta_hat")+ylab("概率密度")+ geom_vline(aes(xintercept=1), linetype=3)
g=g+theme(legend.position = c(0.9, 0.9))
g







