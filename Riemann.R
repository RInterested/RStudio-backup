library(pracma)
options(scipen=999)

Re=1.4

n=8000
nsim = 70
mat = matrix(0,nsim,n)
S = numeric(0)
Imry = numeric(0)

sl = 4
for(j in 1:nsim){
  Imry[j] <- ((((2*pi)/8) * sl) / (nsim) * j)
  s=Re + Imry[j] * 1i
  
  S[j] <- s
 for(k in 1:n){
   mat[j,k] <- 1/(k)^(s) 
 } 
}

abs(S) > 1

Mat = matrix(0,nsim,n)
for(i in 1:nsim) Mat[i,] <- cumsum(mat[i,]) 

size = 0.1
plot(Mat[1,], pch=19, cex=size*1.2, 
     col=rgb(
       1/nsim,
       (sin(0.3*1))^2, 
       (cos(0.2*1))^2),
     xlab="", xaxt="n",
     ylab="", yaxt="n",
      xlim=c(
          0.4,
          3.2),
      ylim=c(
          -1.5,
          0.3))
axis(side=1, at=seq(0.4, 3, by=0.2))
axis(side=2, at=round(seq(-1.5, 0.3, by=0.1),1))
points(zeta(S[1]), 
       col=rgb(
         1/nsim,
         (sin(0.3*1))^2, 
         (cos(0.2*1))^2), 
       pch=19, cex=1)

for(i in 2:nsim){
  points(Mat[i,], pch=19, cex=size, 
         col=rgb(
            i/nsim,
            (sin(0.3*i))^2, 
            (cos(0.2*i))^2,
                  1))
  points(zeta(S[i]), 
         col=rgb(
           i/nsim,
           (sin(0.3*i))^2, 
           (cos(0.2*i))^2,
                 1), 
         pch=19, cex=1)
}


x = seq(0, (((2*pi)/8) * sl), 0.01)
lines(zeta(Re+x*1i), lwd=2)
