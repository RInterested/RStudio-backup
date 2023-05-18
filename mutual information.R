library(MASS)
library(infotheo)

n = 1000
mu = c(0,0)

steps = 100
MI = rep(0,steps)
cor0 = 0.95
seq = seq(cor0, 1, length.out=steps)
for (i in 1:steps){
  Sigma = matrix(c(1, seq[i], seq[i], 1), nrow = 2, byrow=T)
  sam = mvrnorm(n, mu, Sigma)
  cor(sam[,1], sam[,2])
  discsam = discretize(sam)
  MI[i] <- mutinformation(discsam[,1], discsam[,2],"emp")
}

MI <- log(MI) / log(2)
plot(seq,MI, type='l', cex.axis=0.8, col=rgb(.5,.5,1,1), lwd=2,
     xlab='Correlation', ylab='Mutual Information (in bits)',
     main=paste("MI Gaussian Increasing Correlation From rho =", cor0,' to rho = 1'))

