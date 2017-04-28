setwd(
  "/\\shares\\shares\\Business Analytics\\Internal Analytics\\Medical Coding\\CD03 - Hypothesis Testing\\Functional Document"
)

data <- read.csv("ProdVolumn_Quality.csv")
str(data)
summary(data)
table(data$QIP)
data$QIP <- as.integer(data$QIP)

data2 <- subset(data, QIP != 1 & !is.na(data$DRG.QC))

boxplot(data2$DRG.QC ~ data2$QIP, notch = TRUE)
table(data2$QIP)

str(data2)
library("Hmisc")
data2$Prd <- cut2(data2$Prod, g=10)
table(data2$Prd)
plot(data2$DRG.QC ~ data2$Prd)
data2$QIP <- as.factor(data2$QIP)

str(data2)

m1 <- lm(DRG.QC ~ Prd + QIP, data=data2)
summary(m1)
anova(m1)

fv <- fitted(m1) 
tfv <- tapply(fv,list(data2$Prd, data2$QIP),mean)
tfv
tfv[10,3] <- coef(m1) %*% c(1,0,0,0,0,0,0,0,0,1,0,1)
tfv[9,3] <- coef(m1) %*% c(1,0,0,0,0,0,0,0,1,0,0,1)
tfv[8,3] <- coef(m1) %*% c(1,0,0,0,0,0,0,1,0,0,0,1)
tfv[7,3] <- coef(m1) %*% c(1,0,0,0,0,0,1,0,0,0,0,1)
tfv


m2 <- lm(DRG.QC ~ Prd * QIP, data=data2)
summary(m2)
anova(m2)
qnorm(0.975)
