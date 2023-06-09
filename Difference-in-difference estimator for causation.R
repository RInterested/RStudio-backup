library(foreign)
mydata = read.dta("http://dss.princeton.edu/training/Panel101.dta")
head(mydata)

# Create a dummy variable to indicate the time when the treatment started. Lets
# assume that treatment started in 1994. In this case, years before 1994 will have a
# value of 0 and 1994+ a 1. If you already have this skip this step.

mydata$time = ifelse(mydata$year >= 1994, 1, 0)

# Create a dummy variable to identify the group exposed to the treatment. In this
# example lets assumed that countries with code 5,6, and 7 were treated (=1).
# Countries 1-4 were not treated (=0). If you already have this skip this step.

mydata$treated <-  ifelse(mydata$country == "E" |
mydata$country == "F" |
mydata$country == "G", 1, 0)


# Estimating the DID estimator (using the multiplication method, no
# need to generate the interaction)

didreg1 <-  lm(y ~ treated*time, data = mydata)
summary(didreg1)

# The coefficient for ‘treated:time’ is the difference-in-difference estimator (‘did’ in the previous example). The effect is
# significant at 10% with the treatment having a negative effect.

plot(didreg1)