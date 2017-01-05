setwd("C:/Users/makarand.ghule/Documents/AttritionAnalysis/20161220 P3 (SQL)")
# install.packages("RODBC")
library(RODBC)
odbcCloseAll()# odbcClose(edw_bi)
edw_bi <- odbcConnect("PulseDB_EDW_BI")
# head(sqlTables(edw_bi, tableType = "TABLE", schema = "dbo"), 10)
active <- sqlFetch(edw_bi, "AttAnalysis_tblActiveEmployees"); nrow(active)
inactive <- sqlFetch(edw_bi, "AttAnalysis_tblInActiveEmployees"); nrow(inactive)
dataa <- rbind(active, inactive); nrow(dataa); nrow(active) +  nrow(inactive) 
# str(dataa)
# head(dataa)
# summary(dataa)
AR <- subset(dataa, dataa$VerticalName == "Accounts Receivable");nrow(AR) # 4409


# TM <- subset(AR, JobRole == "Team Member" |  JobRole == "Team Leader" |  JobRole == "Desk Head / Team Co-ordinators"); nrow(TM); # 4310
TM <- subset(AR, JobRole == "Team Member"); nrow(TM); # 4157


# when DateofRelieving is NULL
# Current <- subset(TM, is.na(TM$DateOfRelieving) == TRUE | as.numeric(format(TM$DateOfRelieving, '%Y'))  >= 2015 ); nrow(Current) # 3198
# Current$Attrition = ifelse(is.na(Current$DateOfRelieving), 0, 1)
# Current$Available = ifelse(!is.na(Current$DateOfRelieving), 0, 1)
# Current$Status = as.factor(ifelse(is.na(Current$DateOfRelieving), "Current", "Past")) 

Current <- subset(TM, as.numeric(format(TM$DateOfRelieving, '%Y')) == 1900 | as.numeric(format(TM$DateOfRelieving, '%Y'))  >= 2015 ); nrow(Current) # 3072

################ Attrition ##########

# only 2 > on the basis that we'll not have date for current employees as told by sid (his discussion with IT) on 23 Dec
Current$Attrition = ifelse(as.numeric(format(Current$DateOfRelieving, '%Y')) == 1900, 0, 1); sum(Current$Attrition) # 1572
Current$Available = ifelse(as.numeric(format(Current$DateOfRelieving, '%Y')) != 1900, 0, 1); sum(Current$Available) # 1500
Current$Status = as.factor(ifelse(as.numeric(format(Current$DateOfRelieving, '%Y')) == 1900, "Current", "Past")); table(Current$Status)

######### Create Shift ####
data.frame(table(Current$Shift))
table(Current$Shift, Current$Status)
Current$Shift2 <- substr(Current$Shift, 1, 17)
table(Current$Shift2, Current$Status)

Current$Shift3 <- factor(Current$Shift2)

# combining 05:30 and 06:00 shift 
# levels(Current$Shift3) <- c("01:00 PM-10:00 PM", "04:00 PM-01:00 AM", "06:00 PM-03:00 AM", "06:00 PM-03:00 AM",
# "06:30 AM-03:30 PM" ,"06:30 PM-03:30 AM", "08:00 AM-05:00 PM", "08:00 PM-05:00 AM",
# "10:00 AM-07:00 PM" ,"11:00 AM-08:00 PM", "12:00 PM-09:00 PM", "9:00 AM-06:00 PM" )

# levels(Current$Shift3) <- c("01:00 PM-10:00 PM", "06:00 PM-03:00 AM", "06:00 PM-03:00 AM", "06:00 PM-03:00 AM",
# "06:30 AM-03:30 PM" ,"06:00 PM-03:00 AM", "08:00 AM-05:00 PM", "08:00 PM-05:00 AM",
# "10:00 AM-07:00 PM" ,"11:00 AM-08:00 PM", "12:00 PM-09:00 PM", "9:00 AM-06:00 PM" )


# Combining Other shifts too as these shift have very few employee working 

levels(Current$Shift3) <- c("08:00 AM-05:00 PM", "06:00 PM-03:00 AM", "06:00 PM-03:00 AM", "06:00 PM-03:00 AM",
                            "08:00 AM-05:00 PM" ,"06:00 PM-03:00 AM", "08:00 AM-05:00 PM", "06:00 PM-03:00 AM",
                            "08:00 AM-05:00 PM" ,"08:00 AM-05:00 PM", "08:00 AM-05:00 PM", "08:00 AM-05:00 PM" )
table(Current$Shift3, Current$Status)
Current$Shift=NULL
Current$Shift2=NULL

######### Create Distance ####
class(Current$DistanceInKms); head(Current$DistanceInKms)
D_str <- as.character(Current$DistanceInKms)
D1 <- gsub("km","", D_str, fixed=FALSE); class(D1); head(D1)
D2 <- gsub(",", "", D1, fixed=FALSE); class(D2); head(D2)
Current$Distance <- as.numeric(D2)

summary(Current$Distance)
Current$DistanceInKms <- NULL


########### Employee Age ############
# install.packages("lubridate")
library(lubridate)
age <- function(dob, age.day = today(), units = "years", floor = FALSE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

Current$EmployeeAge <- ifelse(Current$Status=="Current", age(Current$DateofBirth), age(Current$DateofBirth, age.day = Current$DateOfRelieving))
summary(Current$EmployeeAge)


################### Exploratory Data Analysis #############

na_list <- sapply(dataa, function(y)(sum(length(which(is.na(y))))))
na_count <- data.frame(na_list)
zero_list <- sapply(dataa, function(y)(sum(length(which(y==0)))))
zero_count <- data.frame(zero_list)
uniq_list <- sapply(dataa, function(y)(sum(length(unique(y)))))
uniq_count <- data.frame(uniq_list)

data_prop <- cbind(na_count, zero_count, uniq_count)
data_prop$variable <-rownames(data_prop)
rownames(data_prop) <- NULL
data_prop 


sum(Current$Attrition) # 1557
sum(Current$Available) # 1641


nlevels(Current$EmployeeCode); nlevels(Current$EmployeeName)
nlevels(Current$RptEmployeeCode); nlevels(Current$RptEmployeeName)
nlevels(Current$Rpt2EmployeeCode)
table(format(Current$DateofJoin, '%Y'))
table(Current$Status, format(Current$DateofJoin, '%Y'))
table(format(Current$DateOfRelieving, '%Y'))
table(format(Current$DateofJoin, '%Y'), format(Current$DateOfRelieving, '%Y'))
table(Current$Status, Current$JobRole)

sort(tapply(Current$Attrition, Current$RptEmployeeName, sum, na.rm=TRUE), decreasing = TRUE)
sort(tapply(Current$Attrition, Current$Rpt2EmployeeName, sum, na.rm=TRUE), decreasing = TRUE)
table(Current$Status, format(Current$DateofBirth, '%Y'))

table(Current$JobRole, Current$Status)

table(Current$Shift, Current$Status)


# table(Current$MaritalStatus, Current$JobRole, Current$Status)
table(Current$MaritalStatus, Current$Gender, Current$Status)
table(Current$TransportMode, Current$Status)
table(Current$WorkFacility, Current$Status)
table(Current$WorkLocation, Current$Status)
table(Current$PrevWorkFacility, Current$Status)
table(Current$PrevWorkLocation, Current$Status)
table(Current$Function, Current$Status)
table(Current$ReasonofLeaving)
table(Current$ExitType)

table(Current$ExperienceType, Current$Status)
table(Current$LastReviewType, Current$Status)
tapply(Current$ProdAvgLast3Months, Current$Status, mean, na.rm=TRUE)
tapply(Current$QualAvgLast3Months, Current$Status, mean, na.rm=TRUE)
table(Current$Course, Current$Status)
table(Current$CourseLevels, Current$Status)
table(Current$Specialization, Current$Status)
tapply(Current$Last30DaysLeaveCount, Current$Status, mean)
table(Current$EngagementIndex, Current$Status)
tapply(Current$TotalExtraHoursWorked, Current$Status, mean)
summary(Current$TotalExtraHoursWorked)
table(Current$StaffingEmployeeStatus, Current$Status)
tapply(Current$PreviousExperienceInMonths, Current$Status, mean, na.rm=TRUE)
table(Current$Process, Current$Status)

table(Current$LastReviewType)

table(Current$StaffingEmployeeStatus)
table(Current$LongLeave)

nlevels(Current$PrevEmployer)
head(Current$PrevEmployer)

table(Current$ExperienceType)

table(Current$CourseLevels)
table(Current$CourseLevels, Current$Status)
subset(Current, CourseLevels=="Professional")["Course"]

table(Current$EngagementIndex) ## 
table(Current$EngagementIndex, Current$Status)
table(Current$JobRole)
table(Current$Gender, Current$TransportMode)
tapply(Current$PreviousExperienceInMonths, Current$ExperienceType, mean, na.rm=TRUE)
sum(is.na(Current$PreviousExperienceInMonths))
sum(Current$PreviousExperienceInMonths==2)
table(Current$ExperienceType, Current$PreviousExperienceInMonths)
sum(is.na(dataset2$PreviousExperienceInMonths))
with(dataset2[is.na(dataset2$PreviousExperienceInMonths),],table(Status))

with(Current[Current$TotalExtraHoursWorked==0,],table(Status))
with(Current[Current$TotalExtraHoursWorked>0,],table(Status))

with(Current[Current$Last30DaysLeaveCount>0,], table(Status))

################ Graphical ##################
boxplot(Current$ProdAvgLast3Months ~ Current$Status) # 825 zeroes (ie 25.8 %) 241 Currrent and 584 past
hist(Current$ProdAvgLast3Months, xlab = "Prod Avg Last 3Months", main="Frequency Plot Prod Avg", breaks = 100)
library("Hmisc")
describe(Current$ProdAvgLast3Months ~ Current$Status ) 
sum(Current$ProdAvgLast3Months==0)
# install.packages("sm")
library("sm")
sm.density.compare(Current$ProdAvgLast3Months, Current$Status)
colfill <- c(2:(2+length(levels(Current$Status))))
legend("topleft", levels(Current$Status), fill=colfill)


dt2 <- subset(Current[c("QualAvgLast3Months", "Status")], QualAvgLast3Months==0)
describe(dt2$QualAvgLast3Months ~ dt2$Status)

boxplot(Current$QualAvgLast3Months ~ Current$Status )
hist(Current$QualAvgLast3Months, breaks = 100, xlab = "Qual Avg Last 3Months", main="Frequency Plot for Qual Avg ") # 716 zeroes (ie 22.4%) 191 Current and 525 past 

boxplot(Current$AGSExperienceInMonths)
boxplot(Current$AGSExperienceInMonths ~ Current$Status)
hist(Current$AGSExperienceInMonths, breaks = 50, xlab = "AGS Experience In Months", main="Frequency Plot for AGS Experience ")
hist(Current$LastReviewRating, breaks=50)
sum(is.na(Current$LastReviewRating)) # 1622 NA ie 50.72%
sm.density.compare(Current$QualAvgLast3Months, Current$Status)
colfill <- c(2:(2+length(levels(Current$Status))))
legend("topleft", levels(Current$Status), fill=colfill)
## create custom function to understand how many zeroes in a column

boxplot(Current$EmployeeAge)
boxplot(Current$EmployeeAge ~ Current$Status)
h <- hist(Current$EmployeeAge, breaks = 100, col="skyblue")
xfit <- seq(min(Current$EmployeeAge), max(Current$EmployeeAge), length=100)
yfit <- dnorm(xfit, mean=mean(Current$EmployeeAge), sd=sd(Current$EmployeeAge))
yfit <- yfit*diff(h$mids[1:2])*length(Current$EmployeeAge)
lines(xfit, yfit, col="blue", lwd=2)

sm.density.compare(Current$EmployeeAge, Current$Status)
colfill <- c(2:(2+length(levels(Current$Status))))
legend("topleft", levels(Current$Status), fill=colfill)


boxplot(Current$Last30DaysLeaveCount)
boxplot(Current$Last30DaysLeaveCount ~ Current$Status)
hist(Current$Last30DaysLeaveCount, breaks = 30,xlab="Last 30 Days Leave Count", main="Freqency of Last 30 Days Leave Count", labels = TRUE)
# plot(density(Current$Last30DaysLeaveCount))  
# sm.density.compare(Current$Last30DaysLeaveCount, Current$Status) # not working
sum(Current$Last30DaysLeaveCount==0) # 1682 zeroes  ie 52.59%
sum(Current$Last30DaysLeaveCount==1) # 62 ones  ie 1.93%
sum(Current$Last30DaysLeaveCount==2) # 91 zeroes  ie 2.84%
sum(Current$Last30DaysLeaveCount==15) # 72 zeroes  ie 2.25%



boxplot(Current$TotalExtraHoursWorked)
boxplot(Current$TotalExtraHoursWorked ~ Current$Status)
hist(Current$TotalExtraHoursWorked,breaks=max(Current$TotalExtraHoursWorked), xlab = "Total Extra Hours Worked", main="Freqency of Total Extra Hours Worked", labels = TRUE) # 1682 zeroes  ie 52.59%
sum(Current$TotalExtraHoursWorked==0)
sum(Current$TotalExtraHoursWorked==1)
sum(Current$TotalExtraHoursWorked>1)


sum(Current$Distance > 50, na.rm = TRUE) # 86 (>100 #49)

head(Current$Distance)
boxplot(Current$Distance)
boxplot(Current$Distance ~ Current$Status)
hist(Current$Distance, breaks=100)

library(ggplot2)
ggplot(subset(Current, Distance>30), aes(x=Distance)) + geom_density()
# ggplot(Current, aes(x=Distance)) + geom_density()

toolong <- which(Current$Distance > 30)
length(toolong)
Current$Distance[toolong]=NA # 139 to already 1576 ie 1715 ie 53.62%
summary(Current$Distance)



dataset2 <- Current[c("EmployeeCode", "EmployeeName", "AGSExperienceInMonths", "EmployeeAge", "Gender"
                      , "MaritalStatus",  "WorkLocation", "ExperienceType", "ProdAvgLast3Months"
                      , "QualAvgLast3Months", "CourseLevels", "Last30DaysLeaveCount","TotalExtraHoursWorked"
                      , "Shift3", "TransportMode","EngagementIndex", "FunctionName", "Status", "Attrition", "Available")]

summary(dataset2)
levels(dataset2$TransportMode)[levels(dataset2$TransportMode)=="- No Transport -"] <- NA

#################   ###################
library(mice) # Multivariate Imputation by Chained Equations
simple <- dataset2[c("MaritalStatus", "CourseLevels", "EngagementIndex", "TransportMode")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)
dataset2$MaritalStatus = imputed$MaritalStatus
dataset2$CourseLevels = imputed$CourseLevels
dataset2$TransportMode = imputed$TransportMode
dataset2$EngagementIndex = imputed$EngagementIndex



continuous_vars <- c("AGSExperienceInMonths", "EmployeeAge", "ProdAvgLast3Months", 
                     "QualAvgLast3Months", "Last30DaysLeaveCount", 
                     "TotalExtraHoursWorked")


factor_vars <- c("Gender", "MaritalStatus", "WorkLocation", 
                 "ExperienceType", "CourseLevels", "Shift3", 
                 "TransportMode", "EngagementIndex", "FunctionName")

# # library(smbinning)
# 
# iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(16))
# 
# 
# for (factor_var in factor_vars) {
#   smb <- smbinning.factor(dataset2, y="Attrition", x=factor_var)
#   if (class(smb) != "character") {
#     iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
#   }
# }
# 
# for(continuous_var in continuous_vars){
#   smb <- smbinning(dataset2, y="Attrition", x=continuous_var) 
#   if(class(smb) != "character"){ 
#     iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
#   }
# }
# iv_df <- iv_df[order(-iv_df$IV), ]
# iv_df

######################## Randomly split data ##########################
library(caTools)
set.seed(88)
split = sample.split(dataset2$Attrition, SplitRatio = 0.75)
training=subset(dataset2, split==TRUE); nrow(training) #2298
testing = subset(dataset2, split==FALSE); nrow(testing) #766
# summary(training)
str(training)
dataset3 <- rbind(training, testing)
########### co-relation plot ###########

nums <- sapply(dataset2, is.numeric)
numdataset2 <- dataset2[,nums]
# chars <- sapply(dataset2, is.factor)
# numdataset3 <- dataset2[,chars]
# 
# str(numdataset3)
str(numdataset2)
cor(numdataset2)

# source("C:\\Users\\makarand.ghule\\Documents\\AttritionAnalysis\\20161201 P2 (Ok with Excel)\\vif_func.R")
# vif_func(numdataset2)
# # vif_func(numdataset3)

cor <- cor(numdataset2, use="pairwise", method="pearson")
cor
ord <- order(cor[1,])
ccor <- cor[ord, ord]
print(ccor)
library(corrplot)
corrplot(cor, mar=c(0,0,1,0))
title(main="Correlation Imputed Data.csv using Pearson Method")

################# 4. Model 1 (all IMP) #######################

################# Naive Data #################

nrows <- nrow(Current)
ncomplete <- sum(complete.cases(Current))
100*ncomplete/nrows # usable data %

####Naive model accuracy - 51.32%##### 50.06527% (after considering TM only)
100*max(table(Current$Status))/nrow(Current)
################################################

# null model
logit0 <- glm(Attrition ~ 1, data=training, family=binomial(link="logit"))
summary(logit0)
# cor(logit0$y, logit0$fitted.values)

# model
#testing model without Last30DaysLeaveCount accuracy on all data
# also removed TotalExtraHoursWorked as no current Employee worked > 0 hrs
logit1 <- glm(Attrition ~  EngagementIndex + AGSExperienceInMonths  + QualAvgLast3Months
              + CourseLevels  + FunctionName + EmployeeAge + Gender + TransportMode  + Shift3
              + WorkLocation + ExperienceType + MaritalStatus
              , data=training, family=binomial(link="logit"))

cor(logit1$y, logit1$fitted.values)
summary(logit1)

#full model
logit_full <- glm(Attrition ~  EngagementIndex + AGSExperienceInMonths  + QualAvgLast3Months  + ProdAvgLast3Months
                  + CourseLevels  + FunctionName + EmployeeAge + Gender + TransportMode  + Shift3
                  + WorkLocation + ExperienceType + MaritalStatus
                  , data=training, family=binomial(link="logit"))

# reduced model
stepwise <- step(logit1, scope = list(lower=formula(logit0), upper=formula(logit_full)), direction="both", trace=0)
summary(stepwise)
cor(stepwise$y, stepwise$fitted.values)
# 
# forward <- step(logit1, scope = list(lower=formula(logit0), upper=formula(logit_full)), direction="forward", trace=0)
# summary(forward)
# 
# backward <- step(logit1, scope = list(lower=formula(logit0), upper=formula(logit_full)), direction="backward", trace=0)
# summary(backward)
# anova(stepwise,  forward, test = 'Chisq')


# cat(sprintf("Chi-square p-value: %.8f\n", dchisq(logit1$null.deviance-logit1$deviance, logit1$df.null-logit1$df.residual)))
library(car)
vif(logit1)

############## Prdict on train data ############

predTrain1 <- predict(logit1, type="response")
tapply(predTrain1, training$Attrition, mean)

# Confusion matrix for threshold of 0.5
table(training$Attrition, predTrain1 > 0.5)
library(InformationValue)
1-misClassError(training$Attrition, predTrain1)# (886+916)/(916+232+264+886)
precision(training$Attrition, predTrain1) # (886)/(232+886)
sensitivity(training$Attrition, predTrain1) # 886/(886+264)
specificity(training$Attrition, predTrain1) # 916/(916+232)

plotROC(training$Attrition, predTrain1,Show.labels = TRUE)

########## Predict on test data ##############

predTest1 <- predict(logit1, type="response", newdata = testing)
tapply(predTest1, testing$Attrition, mean)

# Confusion matrix for threshold of 0.5
table(testing$Attrition, predTest1 > 0.5)
1-misClassError(testing$Attrition, predTest1)
precision(testing$Attrition, predTest1)
sensitivity(testing$Attrition, predTest1)
specificity(testing$Attrition, predTest1)

plotROC(testing$Attrition, predTest1, Show.labels = 1)


##################### prediciton on all data ####################
predAll1 <- predict(logit1, type="response", newdata=dataset3)
tapply(predAll1, dataset3$Attrition, mean)

table(dataset3$Attrition, predAll1 > 0.5)
1-misClassError(dataset3$Attrition, predAll1)
precision(dataset3$Attrition, predAll1)
sensitivity(dataset3$Attrition, predAll1)
specificity(dataset3$Attrition, predAll1)

################# 6 Export Model Score #################

t <- cbind(dataset3, predAll1 )
t$Prediction <- ifelse(predAll1>0.5, "Leave", "Stay"); names(t)
t3 <- t[,c(21,18,1:17, 22)]; names(t3)
write.csv(t2, file="AR_Logit_Score_R0.csv", row.names=FALSE)


