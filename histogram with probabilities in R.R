set.seed(0)

# Define a fair coin:
coin = c(1,0)

# We tossed the coin 10 times and counted the number of heads. Repeat the experiment 20000 times.
n = 20000   # Number of experiments
flips = 10  # Number of coin flips in each experiment.

heads = colSums(replicate(n, sample(coin, flips, replace = T))) # Counts of heads in each experiment.

# The breaks are the number of possible outcomes: flips + 1

h = hist(heads, breaks = sort(unique(heads)), freq=F, 
         border=F, main = 'Histogram counts of heads',
         col=rgb(0.3,0.8,0.8,0.6), ylab='Probability', 
         xlab =  'No. of heads in 10 flips fair coin')

choose(10,2) * 0.5^10
