rm(list=ls())
set.seed(2019)
n <- 60
captions <- paste0('(', letters[1:6], ')')
dat <- list()
dat[[1]] <- 1.8 * rnorm(n)
dat[[2]] <- rnorm(n, mean=sqrt(1.5*sort(rexp(n))) * sample(c(-2.5, 2.5), n, replace=T))
dat[[3]] <- runif(n)*1.5 - rev(dat[[2]])
dat[[4]] <- sin(sort(runif(n)) * pi) * sample(c(1, -1), n, replace=T) + rnorm(n)/5
dat[[5]] <- rnorm(n)/17 - (sort(runif(n)) - 0.5)^2
dat[[6]] <- rnorm(n)/8 + 3*(sort(runif(n)) - 0.65)^2

# postscript('r-y.eps')
pdf('r-y.pdf')
layout(matrix(1:6, 3, 2, byrow=TRUE), c(1.3, 1.5), rep(1,3), TRUE)
par(mar=c(2, 1, 1, 1))
for (i in 1:6) {
  y <- scale(dat[[i]], scale=F)
  p0 <- c(0, length(y) + 0.8)
  p1 <- c(min(y) - 0.2, max(y) + 0.2)
  plot(y, pch='x', bty='n', yaxt='n', xaxt='n', xlab='', ylab='', xlim=p0, ylim=p1)
  if (i == 1) {
    lines(p0, rep(0.8*p1[2], 2), lty=2)
    lines(p0, rep(2*mean(p1)-0.8*p1[2], 2), lty=2)
    text(p0[1]-1.3, 0.8*p1[2], 2)
    text(p0[1]-1.3, 2*mean(p1)-0.8*p1[2], -2)
  }
  lines(p0, rep(mean(p1), 2), lty=2)
  arrows(p0[1], p1[1], p0[2], p1[1], 0.08)
  arrows(p0[1], p1[1], p0[1], p1[2], 0.08)
  mtext(expression(hat(italic(y))), side=1, adj=1.02, padj=0.0, line=-0.5)
  mtext(expression(italic(r)), side=3, adj=-0.02, padj=0.0, line=-1.5)
  mtext(captions[i], side=1)
  text(p0[1]-1.3, mean(p1), 0)
}
dev.off()




rm(list=ls())
set.seed(2019)
ry = rnorm(41,mean=sort(sample(c(-0.5, 0.5), 41, replace = T)));
x <- seq(-4,4,0.2);
#plot(x,x+y);

#n <- 30
captions <- paste0('(', letters[1:2], ')')
dat <- list()
dat[[1]] <- x + ry;
dat[[2]] <- -(x + ry);

# postscript('et_et_1.eps')
pdf('et_et_1.pdf')

layout(matrix(1:2, 1, 2, byrow=TRUE), c(1.3, 1.5), rep(1,1), TRUE)

par(mar=c(2, 1, 1, 1))
for (i in 1:2) {
    plot(dat[[i]], x, pch= 20, cex = 0.7, bty='n', yaxt='n', xaxt='n', xlab='', ylab='',xlim=c(-8,8), ylim=c(-8,8))
    arrows(x0=c(-7,0), y0=c(0,-7), x1=c(7,0),y1=c(0,7),0.08, lwd = 1.25);
	text(6.5, -1.5, expression(italic(e)[t-1]));
	text(-1.5, 6.5, expression(italic(e)[t]));
	text(-1,-1,expression(italic(O)));
	if (i == 1) {
    text(5 , 5, as.roman(1))
	text(-5 , -5, as.roman(3))
  }
	if (i == 2) {
    text(-5 , 5, as.roman(2))
	text(5 , -5, as.roman(4))
  }
    mtext(captions[i], side=1)
}

#####################
t <- c(1:12);
dat <- list()
#dat[[1]] <- c(1.5, -2, 1.2, -1.2, 1.2, -0.9, 1.7, -0.8, 2, -1.5, 3, -2);
#dat[[2]] <- c(1.5, 1, 0.4, -0.4, -1, -1.2, -1.1, -0.8, -0.2, 0.3, 0.8, 1.3);

dat[[1]] <- c(3.5, -3, 3.2, -3.2, 4.2, -0.9, 2.7, -1.8, 4, -1.5, 3, -3);
dat[[2]] <- c(4.5, 2.5, 0.4, -2.2, -3.8, -4.8, -4, -3, -1.8, 0, 1.3, 3);

par(mar=c(2, 1, 1, 1))
for (i in 1:2) {
    plot(t, dat[[i]], pch= 20, cex = 0.7, type = "b", lty = 5, bty='n', yaxt='n', xaxt='n', xlab='', ylab='',xlim=c(0,14), ylim=c(-6,6))
    arrows(x0=c(0,0), y0=c(0,-6), x1=c(14,0),y1=c(0,6),0.08, lwd = 1.25);
	text(13, -1.5, expression(italic(t)));
	mtext(expression(italic(e)[t]), side = 3, adj = -0.03);
    mtext(captions[i], side=1)
}












