
# inddata2 <- subset(inddata, hour >= 7 & hour <= 19 & Q2 < 100)
table(inddata2$hour)
mean(inddata2$Q2)
chisq.test(table(inddata2$Q2, inddata2$hour),
           simulate.p.value = T,
           B = 1000)

fit <- lm(Q2 ~ hour, data = inddata2)
summary(fit)

# d_enc_last
anova(fit2)


pairs(inddata2[, c("hour", "Q2", "Freq")])
library(rattle)
rattle()

fit3 <- lm(Q2 ~ Freq * hour, data = inddata2)
summary(fit3)
anova(fit3)
hist(inddata2$Freq, breaks = max(inddata2$Freq))
hist(inddata2$Q2, breaks = 100)
inddata3 <- subset(inddata2, Q2>0)

library(MASS)
bc <- boxcox(inddata3$Q2 ~ inddata3$hour, lambda = seq(1, 2, 0.1))
inddata3$Q3 <- (inddata3$Q2)^5
hist(inddata3$Q3)
hist(inddata3$Q2)
