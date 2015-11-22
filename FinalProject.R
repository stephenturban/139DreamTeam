library(foreign)
Cohort <- read.csv("/Users/Stephen/Downloads/recentcohort.csv")


model1 <- lm(SAT_AVG ~ NPT4_PUB + NPT4_PRIV, data = Cohort)









