fit1 <- lm(Q2 ~ hour, data = inddata2)
av <- anova(fit1)
av
summary(fit1)
table(hour)
par(mfrow=c(2,2))
plot(fit1)
par(mfrow=c(1,1))



# install.packages("agricolae")
library(agricolae)
HSD.test(fit1, 'inddata2$hour')


TukeyHSD(a1)

table(inddata2)
# attach(inddata)
# str(inddata)
# chisq.test(hour, Q2)
# table(Q2, hour )
# tapply(Q2, hour, mean)
# chisq.test(table(Q2, hour ), simulate.p.value = TRUE)

drop1(a1, ~., test='F')


posthoc <- TukeyHSD(x=a1, inddata2$hour, conf.level=0.95)
library(MASS)
bc <- boxcox(inddata2$Q2 ~ inddata2$hour, lambda = seq(1, 2, 0.1))
(trans <- bc$x[which.max(bc$y)])

hist(inddata2$Q3)





chisq.test(table(inddata2$Q3, inddata2$hour ), simulate.p.value = T)
set.seed(1)

fit <- lm(Q3 ~ hour, data=inddata2)
summary(fit)
layout(matrix(c(1,2,3,4),2,2))
plot(fit)
par(mfrow=c(1,1))
sqrt(abs(coef(fit)))

hist(inddata2$Q2, breaks = 100)
inddata2$Q3 <- (inddata2$Q2)^2