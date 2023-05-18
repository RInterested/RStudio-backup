library(actuar)
shape = 1
rate = 0.1
x = seq(0,10,0.001)
y = dinvweibull(x, shape, rate = 1, scale = 1/rate, log = FALSE)
z = dweibull(x, shape, scale = 1, log = FALSE)

plot(x,z, type='l', col=2, lwd=2, xlab='', ylab='')
points(x,y, type='l', col=4, lwd=2)