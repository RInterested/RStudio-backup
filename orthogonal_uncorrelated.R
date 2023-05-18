library(mvtnorm)
library(squash)
library(plot3D)

set.seed(0)

n <- 1e7

sigma <- matrix(c(1,1,1,1),nrow=2,byrow=T)
mean <- c(-1,1)

bivar <- rmvnorm(n,mean,sigma)
X <- bivar[,1]
Y <- bivar[,2]
colMeans(bivar)
cov(X,Y)
(X %*% Y) / n

hist(X, border=F, col=rgb(0,0,0,.1), xlim=c(-5,5))
hist(Y, border=F, add=T, col=rgb(0,0,0,.3))
plot(X[1:1000],Y[1:1000],pch=19, cex=.3)
hist2(X,Y)

##  Create cuts:
x_c <- cut(X, 50)
y_c <- cut(Y, 50)
##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)
##  Plot as a 3D histogram:
hist3D(z=z, col = jet.col(100, alpha = 0.8), 
       colkey = F, scale = T, border='gray90',
       ticktype = 'detailed', zlab='', xlab='X', ylab='Y',
       axes3D='gray', frequency=T)

##  Plot as a 2D heatmap:
image2D(z=z, border="black")



Sig <- matrix(c(1,0,0,1),nrow=2,byrow=T)
bivar <- rmvnorm(n,mean,Sig)

X <- bivar[,1]
Y <- bivar[,2]
colMeans(bivar)
cov(X,Y)
(X %*% Y) / n

hist(X, border=F, col=rgb(0,0,0,.1), xlim=c(-5,5))
hist(Y, border=F, add=T, col=rgb(0,0,0,.3))
plot(X[1:1000],Y[1:1000],pch=19, cex=.3)
hist2(x,y)

##  Create cuts:
x_c <- cut(X, 50)
y_c <- cut(Y, 50)
##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)
##  Plot as a 3D histogram:
hist3D(z=z, col = jet.col(100, alpha = 0.2), 
       colkey = F, scale = T, border='gray90',
       ticktype = 'detailed', zlab='', xlab='X', ylab='Y',
       axes3D='gray', frequency=T)

##  Plot as a 2D heatmap:
image2D(z=z, border="black")


X <- runif(1e6,min=-1,max=1)
Y <- ifelse(X>0,X,-X)
plot(X[1:100],Y[1:100],pch=16)
rug(X[1:100],side=1,col="grey")
rug(Y[1:100],side=2,col="grey")

mean(X)
mean(Y)
cov(X,Y)
(X %*% Y) / n
