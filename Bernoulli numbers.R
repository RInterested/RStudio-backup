library(MASS)

# Bernoulli numbers:

n = 11

asymmetric.lower.triang.Pascal = matrix(0,n,n)
for(i in 1:n){
  for(k in 0:(i-1)) 
    asymmetric.lower.triang.Pascal[i,k+1] = choose(i,k)
} 

M = asymmetric.lower.triang.Pascal
M

m = M * (((row(M) - col(M))%%2==1) * -1) +
M * (((row(M) - col(M))%%2==0) * 1) 
m

Faulhaber = fractions(solve(m))
rownames(Faulhaber) <- c(paste('S',1:nrow(Faulhaber)))
Faulhaber

rowSums(Faulhaber)
(Bernoulli = Faulhaber[,1])