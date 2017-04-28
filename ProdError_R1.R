setwd(
  "/\\shares\\shares\\Business Analytics\\Internal Analytics\\Medical Coding\\CD03 - Hypothesis Testing\\Functional Document"
)

data <- read.csv("ProdVolumn_Quality_R0.csv")
data2 <- data[!is.na(data$DRG.QC),]
data2$Prod = data2$Prod.Comp + data2$Prod.Pend


hist(data2$DRG.QC, breaks = 100, col = "#00600060")
hist(data2$Prod, breaks = 198, col = "#00009050")
library("Hmisc")
data2$Prd <- cut2(data2$Prod, g=10)
table(data2$Prd)
plot(data2$DRG.QC ~ data2$Prd)

mod2 <- lm(DRG.QC ~ Prd, data=data2)
summary(mod2)
