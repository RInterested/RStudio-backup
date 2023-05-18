
s = sample(c(0,1),100,T)
s

rle(s)$lengths[rle(s)$values==1]

ones = function(s){
  streaks = numeric()
  n = length(s)
  counter = 0
  for(i in 1:n){
    if (s[i] == 1){
    counter = counter + 1
    if(i == n && counter > 0) streaks = append(streaks, counter)
    }
    else{
    if(counter > 0) streaks = append(streaks, counter)
    counter = 0
    }
  }
  streaks
}

ones(s)

FALSE %in% c(rle(s)$lengths[rle(s)$values==1] == ones(s))


system.time(mean(replicate(1e6, TRUE %in% (ones(sample(c(0,1),100,T)) >= 7))))
system.time(mean(replicate(1e6, TRUE %in% (rle(s <- sample(c(0,1),100,T))[[1]][rle(s)[[2]]==1] >= 7))))


sk1 = function(s){
  n = length(s)
  y <- s[-1] != s[-n]
  i <- c(which(y), n)
  diff(c(0, i))[s[i]==1]
}

set.seed(0)

# https://stackoverflow.com/a/73438435/4089351

set.seed(0)
mean(replicate(1e6, TRUE %in% (with(rle(sample(c(0,1),100,T)), lengths[values==1]) >= 7)))
