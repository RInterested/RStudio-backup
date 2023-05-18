# From 3B1B video https://youtu.be/CfW845LNObM

# Plotting two parallel lines to show the function 1 + 1/x as a transform"

 x = seq(-10,10,0.001)
 par(bg = "gray70")
 plot(x, x/x, type='l', ylab='', xlab='', 
      xlim=c(-9.9,9.9), ylim=c(0.65,1.05))
 lines(x, x/x - 0.3)
 
 #Plotting the transformation as segments from one line to the other:
 max = 10
 vec = numeric(0)
 j <- 0
 for (i in seq(-max,max,0.01)){
   j <- j + 1
   vec[j] <- 1 + 1/i
   segments(x0 = i, y0 = 1, x1 = vec[j], y1 = 1-0.3,
            col=rgb(max/(1/abs(i)+max),
                    max/(max/abs(i)+max),
                    max/(0.01*abs(i)+max), 
                    max/(10.5*abs(i)+max)))
 }


 # Plotting the golden ratio:
 
 abline(v=1.61802, col=rgb(0,0,0,.1))
 text(1.61802+0.5,0.68,'1.61802')
 
 
 # Sampling a few points to recurrently apply 1 + 1/x to them:
 set.seed(0)
 rep = 15
 v <- sample(vec,50)
 
for(i in 1: length(v)) points(v[i], 1, pch=19, cex=1.2, col=rgb(1,0,0,0.2))
 
 
 # Every time 1 + 1/x is apply the results are in a different row:
 m <- matrix(0, rep+1, length(v))
 m[1,] <- v
 
 # The 0.0001 is to avoid inf results:
 
 for (k in 1:rep){
   for (i in 1:ncol(m)){
     m[k+1, i] <- 1 + 1/(m[k,i]+0.0001)
     points(m[k+1,i], 0.7 + 0.2*k/rep, pch=19, cex=1.2,
              col=rgb(1-k/rep, 0, 0, 0.5*(1-k/rep)))
   }
 }
 
 

 # Now checking the unstable point around -0.618: 1/2(1-sqrt(5)):
 # Starting from the beginning of the plot:
 x = seq(-10,10,0.001)
 par(bg = "gray70")
 plot(x, x/x, type='l', ylab='', xlab='', 
      xlim=c(-9.9,9.9), ylim=c(0.65,1.05))
 lines(x, x/x - 0.3)
 
 
 max = 10
 vec = numeric(0)
 j <- 0
 for (i in seq(-max,max,0.01)){
   j <- j + 1
   vec[j] <- 1 + 1/i
   segments(x0 = i, y0 = 1, x1 = vec[j], y1 = 1-0.3,
            col=rgb(max/(1/abs(i)+max),
                    max/(max/abs(i)+max),
                    max/(0.01*abs(i)+max), 
                    max/(10.5*abs(i)+max)))
 }
 
 
 abline(v=-0.618033, col=rgb(0,0,0,.1))
 text(-0.618+0.5,0.68,'-0.618033')
 
 rep = 20
 v <- sample(seq(-0.620, -0.616, 0.00001),50)
 # Plotting these points
 for(i in 1: length(v)) points(v[i], 1, pch=19, cex=1.2, col=rgb(1,0,0,0.2))
 
 m <- matrix(0, rep+1, length(v))
 m[1,] <- v
 
 for (k in 1:rep){
   for (i in 1:ncol(m)){
     m[k+1, i] <- 1 + 1/(m[k,i]+0.0001)
     points(m[k+1,i], 0.7 + 0.2*k/rep, pch=19, cex=1.2,
            col=rgb(1-k/rep, 0, 0, 0.5*(1-k/rep)))
   }
 }
 
 
 # Stability around phi 1.1618:
 x = seq(-10,10,0.001)
 par(bg = "gray70")
 plot(x, x/x, type='l', ylab='', xlab='', 
      xlim=c(-9.9,9.9), ylim=c(0.65,1.05))
 lines(x, x/x - 0.3)
 
 
 max = 10
 vec = numeric(0)
 j <- 0
 for (i in seq(-max,max,0.01)){
   j <- j + 1
   vec[j] <- 1 + 1/i
   segments(x0 = i, y0 = 1, x1 = vec[j], y1 = 1-0.3,
            col=rgb(max/(1/abs(i)+max),
                    max/(max/abs(i)+max),
                    max/(0.01*abs(i)+max), 
                    max/(10.5*abs(i)+max)))
 }
 
 
 abline(v=1.61802, col=rgb(0,0,0,.1))
 text(1.61802+0.5,0.68,'1.61802')
 
 rep = 20
 v <- sample(seq(1.616, 1.620, 0.00001),50)
 # Plotting these points
 for(i in 1: length(v)) points(v[i], 1, pch=19, cex=2, col=rgb(1,0,0,0.2))
 
 m <- matrix(0, rep+1, length(v))
 m[1,] <- v
 
 for (k in 1:rep){
   for (i in 1:ncol(m)){
     m[k+1, i] <- 1 + 1/(m[k,i]+0.0001)
     points(m[k+1,i], 1 - 0.3, pch=19, cex=2,
            col=rgb(k/rep, 0, 0, 0.5))
   }
 }
 