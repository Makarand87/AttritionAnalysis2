setwd(
  "/\\shares\\shares\\Business Analytics\\Internal Analytics\\Medical Coding\\CD03 - Hypothesis Testing\\Functional Document"
)

data <- read.csv("ProdVolumn_Quality.csv")
data$Prod = data$Prod.Comp + data$Prod.Pend
str(data)
summary(data)
# sum(length(which(is.na(data$DRG.QC))))
data2 <- subset(data, Prod != 0)
data2 <- data2[!is.na(data2$DRG.QC),]

hist(data2$DRG.QC, breaks = 100, col = "#00009060")
hist(data2$Prod, breaks = 198, col = "#00009050")

mod1 <- lm(DRG.QC ~ Prod, data=data2)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1)
par(mfrow=c(1,1))
plot(data2$DRG.QC ~ data2$Prod)
abline(coef(mod1))
adj <- data.frame( pos=rep(4,nrow(data2)), jit=0, row.names=data2$EmployeeLoginName)
text(data2$Prod, data2$DRG.QC+adj$jit, data2$EmployeeLoginName, pos=adj$pos, cex=0.75)
table(data2$Prod)
data2$z <- cut2(data2$Prod, g=10)
table(z)
plot(data2$DRG.QC ~ data2$z)

mod2 <- lm(DRG.QC ~ z, data=data2)
summary(mod2)
par(mfrow=c(2,2))
plot(mod2)
par(mfrow=c(1,1))
anova(mod2)

data2$prod.g <- cut(data2$Prod, breaks = c(min(data2$Prod), 500, 1000, max(data2$Prod)),
              right=FALSE, include.lowest = T, labels = c("Low", "Medium", "High"))

data.frame(min=tapply(data2$Prod, data2$prod.g, min), 
           max=tapply(data2$Prod, data2$prod.g, max))

tapply(data2$DRG.QC, data2$prod.g, mean)
plot(data2$DRG.QC ~ data2$prod.g)
mod3 <- lm(DRG.QC ~ prod.g, data = data2)
summary(mod3)
anova(mod3)

# ddf <- data.frame( resid(mod3), rstandard(mod3), rstudent(mod3),
#                     hatvalues(mod3), cooks.distance(mod3), row.names = data2$EmployeeLoginName)
# ddf
# ddf[abs(ddf$rstandard) > 2 | abs(ddf$rstudent)>2, ]
# ddf[ ddf$hat > 2*4/20, ]
# 
# i <- order(-ddf$cook)[1:6]
# ddf[i,]
# 
# plot(fitted(mod1), rstudent(mod1))
