mu = 35
sigma = 6
lim = 4 * sigma
tails = (1 - 0.85) / 2
X = seq(mu - lim, mu + lim, length=200)
Y = dnorm(X, mean=mu, sd=sigma)

plot(X, Y, type="l", main = "Gaussian PDF", xlab='X', ylab='Density of Probability', lwd=5)
xx = seq(qnorm(tails,mu,sigma), qnorm(1 - tails,mu,sigma), length=1000)
yy = dnorm(xx, mean=mu, sd=sigma)

segments( qnorm(tails,mu,sigma),0, qnorm(tails,mu,sigma),dnorm( qnorm(tails,mu,sigma), mean=mu, sd=sigma), lwd=3, lty=3)
segments( qnorm(1-tails,mu,sigma),0, qnorm(1-tails,mu,sigma),dnorm( qnorm(1-tails,mu,sigma), mean=mu, sd=sigma), lwd=3, lty=3)
abline(h=0)

library(png)
img <- readPNG(source = "heart.png")
rimg <- as.raster(img) # raster multilayer object
r <- nrow(rimg) / ncol(rimg) # image ratio

n = 9

xlef = seq(qnorm(tails,mu,sigma),qnorm(1-tails,mu,sigma), length.out=n)
xlef = xlef[1:(n-1)]
ydwn = rep(0, n-2)
xr =xlef + 1.8*r
yup = ydwn + 0.003*r

rasterImage(rimg, xlef, ydwn, xr, yup) 

for(i in 1:15){
  rasterImage(rimg, xlef, ydwn+0.005*i, xr, yup+0.005*i) 
}

polygon(c(X,rev(X)),c(Y,rep(max(Y) + 0.02*max(Y),length(X))),col='white', border =F)

lines(X, Y, lwd=5)
abline(h=0)
