x = seq(1,1e5,1)
#y = sqrt(x^2 +5*x -1) - sqrt(x^2 +5*x +2)
#y = (x+3)/(2*x+1)

y = ((x+3)/(2*x+1))^((x^2+4)/(x-2))
plot(x,y,type='l', col=2, lwd=4)
abline(h =0.5, lwd=3,lty=3)
