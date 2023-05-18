# From https://mgimond.github.io/Stats-in-R/Logistic.html

dat <- read.csv("http://mgimond.github.io/Stats-in-R/Data/Income_and_education.csv", stringsAsFactors = TRUE)

# Limit the dataset to the two columns of interest
df <- data.frame('Coast'=dat$Coast, 'Income' = dat$Per.capita.income,
                 'Edu' = dat$Fraction.with.Bachelor.s.or.greater)
head(df)
M1 <- glm(Coast ~ Income, df, family = binomial)
M1
modelChi.M1 <- M1$null.deviance - M1$deviance
modelChi.M1
pseudo.R2.M1 <- modelChi / M1$null.deviance
pseudo.R2.M1

Chidf <- M1$df.null - M1$df.residual
chisq.prob <- 1 - pchisq(modelChi, Chidf)
chisq.prob

M2 <- glm(Coast ~ Income + Edu, df, family = binomial)
M2
modelChi.M2 <- M2$null.deviance - M2$deviance
pseudo.R2.M2 <- modelChi / M2$null.deviance
pseudo.R2.M2
pseudo.R2.M1

Chidf.M2 <- M2$df.null - M2$df.residual
modelChi.M2 <- M2$null.deviance - M2$deviance
1 - pchisq(modelChi.M2, Chidf.M2)