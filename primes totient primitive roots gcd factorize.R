# This function checks wheather a number (or vector of numbers) is (are) prime.

is.prime = function(x){
  prime = 1
  is.it.a.prime <- numeric(0)
  for(k in 1:length(x)){
    for(i in 2:ceiling(sqrt(x[k]))) ifelse(x[k] %% i == 0, prime <- 0, 1)
    if(x[k] == 2) prime <- 1
    is.it.a.prime[k] <- prime == 1
  }
  
  rbind(number=x, is.it.a.prime)
}


# Which numbers in a vector are prime:

prime.select = function(x){
  vec <- numeric(0)
  for(k in 1:length(x)){
    prime = TRUE
    for(i in 2:ceiling(sqrt(x[k]))) ifelse(x[k] %% i == 0, prime <- FALSE, prime)
    if(x[k] == 2) prime <- TRUE
    vec[k] <- prime
  }
  m = rbind(x, as.numeric(vec))
  m[1,m[2,]==1]
}


# Factoring into primes

factors = function(x){
  fact = numeric(0)
  i <- 0
  div <- 2 
  rest <- x
  while(rest != 1){
    while(rest %% div == 0) {
      fact <- c(fact, div)
      rest <- rest / div
    }
    i <- i + 1
    div <- 2 * i + 1
  }
  fact
}


# Greatest common denominator:

gcd = function(a,b) {
  r <- a %% b
  while(r !=0 ){
    a <- b
    b <- r
    r <- a %% b 
  }
  return(b)
}


# Modular inverse:

inverse.mod.m = function(a,m){
  if(gcd(a,m) != 1){
    paste('WARNING:', a, 'and', m, 'are not relatively prime. Hence,', a, 'has no inverse!')
  }else{
    mat <- matrix(0,0,4)
    x = m; w = a
    while(x%%w != 0){
      r <- x%%w
      mat <- rbind(mat, c(r, x, floor(x/w), w))
      x <- w; w <- r 
    } 
    
    mat[1,1] <- - mat[1,3] * sign(a)
    mat[2,1] <-  1 * (sign(a)) - mat[2,3] * mat[1,1]
    if(nrow(mat)>2){
      for(i in 3: (nrow(mat))){
        mat[i,1] <-  mat[i-2,1] - mat[i,3] * mat[i-1,1]
      }
      ans <- mat[nrow(mat),1] 
      while(ans < 0)
        ans <- ans + m
    }else{
      ans <- 1*(sign(a)) - mat[2,3] * mat[1,1]
    }
    paste('The inverse of', a ,'is', ans, '(mod', m,')')
  }
}

# Totient Euler function:

phi = function(n){
  result = 1
  for (i in 2:n)
    if (gcd(i, n) == 1)
      result = result + 1
  return(paste('There are ', result, ' relatively prime integers from 1 to', n, '.'))
}


# Finding primitive roots mod n:

primitive.roots.mod.n = function(n){
  
  phi = function(n){
    result = 1
    for (i in 2:n)
      if (gcd(i, n) == 1)
        result = result + 1
    return(result)
  }
  
  print('WARNING: Loses accuracy above 17.')
  
  roots = numeric(0)
  for(i in 1:(n-1)){
    vec = numeric(0)
    for(k in 1:(n-1)){
      vec[k] <- i^k %% n 
    }
    roots[i] <- length(unique(vec))
  }
  
  len = length(which(roots==phi(n)))
  r = which(roots==phi(n))
  m <- matrix(0, len, n - 1)
  for (k in 1: len){
    for (i in 1:(n - 1)) m[k, i] <- r[k]^i %% n
  }
  
  l = list('primitive roots'=paste(list(which(roots==phi(n))), ' is/are primitive root(s) modulo', n, '.'), 'powers mod n'=m)
  return(l)
}