lambda=1
gamma=2.5
a=3.7
lambda1=lambda2=0.5

scad.pen <- function(t, lambda, a){
  if(abs(t) < lambda) sp <- lambda*abs(t)
  if(abs(t) >= lambda && abs(t) < a*lambda) sp <- -(t^2-2*a*lambda*abs(t)+lambda^2)/(2*(a-1))
  if(abs(t) >= a*lambda) sp <- (a+1)*lambda^2/2
  return(sp)
}

mcp.pen <- function(t, lambda, gamma){
  In <- function(c) 1-c/(lambda*gamma)
  if(abs(t) <= lambda*gamma) mp <- lambda * integrate(In, lower=0, upper=abs(t))[[1]]
  else mp <- lambda * integrate(In, lower=0, upper=lambda*gamma)[[1]]
}

x <- seq(-6,6,by=0.01)
mcp <- scad <- lasso <- adalasso <- enet <- NULL
for(i in 1:length(x)){
  mcp[i] <- mcp.pen(x[i], lambda, gamma)
  scad[i] <- scad.pen(x[i], lambda, a)
  lasso[i] <- lambda*abs(x[i])
  adalasso[i] <- lambda*(1/abs(x[i])^2)*abs(x[i])
  enet[i] <- lambda1*abs(x[i]) + lambda2*x[i]^2
}
plot(x, lasso, type="l",  ylim=c(0,5),
     xlab=expression(beta), ylab=expression(rho[lambda](beta)), lwd=2, col=1)
#axis(1,0,0) xaxt = "n", yaxt="n"
points(x, enet, type="l", lty=2, lwd=2, col=2)
points(x, mcp, type="l", lty=3, lwd=2, col=3)
points(x, scad, type="l", lty=4, lwd=2, col=4)
legend("bottomright", c("Lasso","Enet","MCP","SCAD"), 
       lwd=c(2,2,2,2), lty=c(1,2,3,4), col=c(1,2,3,4), cex=0.4)

###########
x <- seq(-10,10, by=0.01)
y = x
plot(x, y, type="l")

if(abs(y)<3){
  y.lasso=0
}else y.lasso=y





