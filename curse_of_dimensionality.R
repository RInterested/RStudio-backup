library(plot3D)
library(scatterplot3d)

n <- 1e5

u <- runif(n,-1,1)
v <- runif(n,-1,1)
w <- runif(n,-1,1)
scatter3D(u, v, w, bty = "g", pch = 19, cex=.2, col=2, alpha=0.2,
          axes=F)

m <- cbind(u,v,w,sqrt(u^2 + v^2 +w^2))
s <- m[m[,4]<1,1:4]
scatter3D(s[,1], s[,2], s[,3], bty = "g", pch = 19, 
          cex=.2, col=rgb(1,.38,0, alpha = 0.3), axes=F)
mean(s[,4] > 0.9)
rind <- s[s[,4]>.9,1:4]
scatter3D(rind[,1], rind[,2], rind[,3], bty = "g", pch = 19, 
          cex=.2, col=rgb(.9,.2,0, alpha = 0.7), axes=F, add=T)


M <- matrix(runif(2*n,-1,1),ncol=2)
M <- cbind(M,sqrt(M[,1]^2+M[,2]^2))
d <- M[M[,3]<1,]
plot(d[,1],d[,2], col=rgb(1,.38,0, alpha = 0.2), pch=19, cex=.2,
     xlab='', ylab='', tck=F, axes=F)
out <- d[d[,3]>0.9,1:3]
points(out[,1], out[,2],  pch = 19, 
     cex=.2,  col=rgb(.9,.2,0, alpha = 0.5))
mean(d[,3]>.9)


four <- matrix(runif(4*n,-1,1), ncol=4)
four <- cbind(four,sqrt(four[,1]^2+four[,2]^2+four[,3]^2+four[,4]^2))
fd <- four[four[,5]<1,]
mean(fd[,5]>.9)


dist = 2
proj <- matrix(rep(0,nrow(fd)*3), ncol=3)
for(i in 1:nrow(proj)){
  matr <- matrix(c(1/(dist-fd[i,4]),0,0,0,
                   0,1/(dist-fd[i,4]),0,0,
                   0,0,1/(dist-fd[i,4]),0), ncol=4, byrow=T)
  proj[i,] <- matr %*% fd[i,1:4]
}

scatterplot3d(proj[,1], proj[,2], proj[,3], pch=20,
              color=rgb(1,.38,0, alpha = 0.2), axis=F, grid=F,
              angle=280, box=F, cex.symbols=.2)



five <- matrix(runif(5*n,-1,1), ncol=5)
five <- cbind(five,sqrt(five[,1]^2+five[,2]^2+
                          five[,3]^2+five[,4]^2+five[,5]^2))
fvd <- five[five[,6]<1,]
mean(fvd[,6]>.9)


dist = 2
proj4 <- matrix(rep(0,nrow(fvd)*4), ncol=4)
for(i in 1:nrow(proj4)){
  matr <- matrix(c(1/(dist-fvd[i,5]),0,0,0,0,
                   0,1/(dist-fvd[i,5]),0,0,0,
                   0,0,1/(dist-fvd[i,5]),0,0,
                   0,0,0,1/(dist-fvd[i,5]),0), ncol=5, byrow=T)
  proj4[i,] <- matr %*% fvd[i,1:5]
}

proj3 <- matrix(rep(0,nrow(proj4)*3), ncol=3)
for(i in 1:nrow(proj3)){
  matr <- matrix(c(1/(dist-proj4[i,4]),0,0,0,
                   0,1/(dist-proj4[i,4]),0,0,
                   0,0,1/(dist-proj4[i,4]),0), ncol=4, byrow=T)
  proj3[i,] <- matr %*% proj4[i,1:4]
}


scatterplot3d(proj3[,1], proj3[,2], proj3[,3], pch=20,
              color=rgb(1,.38,0, alpha = 0.2), axis=F, grid=F,
              angle=280, box=F, cex.symbols=.2)
