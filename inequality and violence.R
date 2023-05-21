# After this tweet https://twitter.com/blair_fix/status/1658254935769886721?s=20

d = read.csv("ineq_and_violence.csv")
head(d)
colnames(d) = c("country", "inequality", "murders")
plot(log(murders) ~ inequality, d, pch = 19)
mod = lm(log(murders) ~ inequality, d)

summary(mod)


par(mfrow=c(4,2), mai = c(0.4, 0.7, 0.3, 0.11))

plot(murders ~ inequality, log="y", d, pch = 19, cex=0)
text(d$murders ~ d$inequality, labels=d$country, cex=0.8,
     col=ifelse(d$country %in% c('QA', 'BH','MZ'),2,1))

hist(residuals(mod), border=F)

plot(mod, 1:3, pch=19, cex.lab=1, cex.main=0.1, 
     cex=0.1, labels.id=d$country)

plot(mod, 4:6, pch=19, cex.lab=1, cex.main=0.1, 
     cex=ifelse(d$country %in% c('QA', 'BH','MZ'), 2, 0.1), 
     labels.id=d$country, 
     col=ifelse(d$country %in% c('QA', 'BH','MZ'),2,1))




# WITHOUT LOG TRANSFORM:


plot(murders ~ inequality, d, pch = 19)
mod2 = lm(murders ~ inequality, d)

summary(mod2)


par(mfrow=c(4,2), mai = c(0.4, 0.7, 0.3, 0.11))

plot(murders ~ inequality, d, pch = 19, cex=0)
text(d$murders ~ d$inequality, labels=d$country, cex=0.8,
     col=ifelse(d$country %in% c('QA', 'BH','MZ'),2,1))

hist(residuals(mod2), border=F)

plot(mod2, 1:3, pch=19, cex.lab=1, cex.main=0.1, 
     cex=0.1, labels.id=d$country)

plot(mod2, 4:6, pch=19, cex.lab=1, cex.main=0.1, 
     cex=ifelse(d$country %in% c('QA', 'BH','MZ'), 2, 0.1), 
     labels.id=d$country, 
     col=ifelse(d$country %in% c('QA', 'BH','MZ'),2,1))

library(moments)
skewness(residuals(mod2))

