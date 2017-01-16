# install.packages("RODBC")
library(RODBC)
odbcCloseAll()
edw_bi <- odbcConnect("PulseDB_EDW_BI")
active <- sqlFetch(edw_bi, "AttAnalysis_tblActiveEmployees")
inactive <- sqlFetch(edw_bi, "AttAnalysis_tblInActiveEmployees")
dataa <- rbind(active, inactive)
AR <- subset(dataa, dataa$VerticalName == "Accounts Receivable")
TM <- subset(AR, JobRole == "Team Member")
Current <- subset(TM, as.numeric(format(TM$DateOfRelieving, '%Y')) == 1900 | as.numeric(format(TM$DateOfRelieving, '%Y'))  >= 2015 )

################ Attrition ##########

# only 2 > on the basis that we'll not have date for current employees as told by sid (his discussion with IT) on 23 Dec
Current$Attrition = ifelse(as.numeric(format(Current$DateOfRelieving, '%Y')) == 1900, 0, 1)
Current$Available = ifelse(as.numeric(format(Current$DateOfRelieving, '%Y')) != 1900, 0, 1)
Current$Status = as.factor(ifelse(as.numeric(format(Current$DateOfRelieving, '%Y')) == 1900, "Current", "Past"))

######### Create Distance ####

D_str <- as.character(Current$DistanceInKms)
D1 <- gsub("km","", D_str, fixed=FALSE)
D2 <- gsub(",", "", D1, fixed=FALSE)
Current$Distance <- as.numeric(D2)
summary(Current$Distance)
Current$DistanceInKms <- NULL
sum(length(which(Current$Distance>30)))/nrow(Current)


########## EmployeeAge #######
library(lubridate)

Current$EmployeeAge <- ifelse(Current$Status=="Current", interval(Current$DateofBirth, today())/duration(num=1, units = "years"), interval(Current$DateofBirth, Current$DateOfRelieving)/duration(num=1, units = "years"))
summary(Current$EmployeeAge)

############ modified variables ###############
levels(Current$TransportMode)[levels(Current$TransportMode)=="- No Transport -"] <- NA
Current$LastReviewDate2 <- as.Date(Current$LastReviewDate, '%Y-%m-%d')

############### New variables ###############
JMonth <- month(Current$DateofJoin)
RMonth <- month(Current$DateOfRelieving)
Current$JMonth <- as.factor(JMonth)
Current$RMonth <- as.factor(RMonth)

######### Create Shift ####
data.frame(table(Current$Shift))
table(Current$Shift, Current$Status)
Current$Shift2 <- substr(Current$Shift, 1, 17)
table(Current$Shift2, Current$Status)

Current$Shift3 <- factor(Current$Shift2)

levels(Current$Shift3) <- c("08:00 AM-05:00 PM", "06:00 PM-03:00 AM", "06:00 PM-03:00 AM", "06:00 PM-03:00 AM",
                            "08:00 AM-05:00 PM" ,"06:00 PM-03:00 AM", "08:00 AM-05:00 PM", "06:00 PM-03:00 AM",
                            "08:00 AM-05:00 PM" ,"08:00 AM-05:00 PM", "08:00 AM-05:00 PM", "08:00 AM-05:00 PM" )

table(Current$Shift3, Current$Status)
Current$Shift=NULL
Current$Shift2=NULL


##################### Use limited data #############

dataset2 <- Current[c(  "EmployeeCode", "EmployeeName", "AGSExperienceInMonths", "Gender", "EmployeeAge"
                      , "JMonth", "RMonth", "MaritalStatus",  "WorkLocation", "ExperienceType", "ProdAvgLast3Months"
                      , "QualAvgLast3Months", "CourseLevels", "Last30DaysLeaveCount","TotalExtraHoursWorked"
                      , "LastReviewType", "LastReviewRating", "Client", "SubClient" 
                      , "Shift3", "TransportMode","EngagementIndex", "FunctionName", "Status", "Attrition", "Available")]

summary(dataset2)


#################  Imputation ###################
library(mice) 
simple <- dataset2[c("MaritalStatus", "CourseLevels", "EngagementIndex", "TransportMode", "Client" )]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)
dataset2$MaritalStatus = imputed$MaritalStatus
dataset2$CourseLevels = imputed$CourseLevels
dataset2$TransportMode = imputed$TransportMode
dataset2$EngagementIndex = imputed$EngagementIndex
dataset2$Client = imputed$Client

# Randomly split data
library(caTools)
set.seed(88)
split = sample.split(dataset2_2$Availability_Filter, SplitRatio = 0.75)
training=subset(dataset2_2, split==TRUE); nrow(training)
testing = subset(dataset2_2, split==FALSE); nrow(testing)


table(training$WorkLocation, training$Availability_Filter)

dataset3 <- rbind(training, testing)

summary(training)
tapply(training$ExperienceInAGS, training$Availability_Filter, mean)
tapply(training$EmployeeAge, training$Availability_Filter, mean)
table(training$Availability_Filter, training$Gender)


################### 3. Inintal Analyis ##########
## Information Value
# install.packages("smbinning")
library(smbinning)

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(16))


for (factor_var in factor_vars) {
  smb <- smbinning.factor(dataset2_2, y="Attrition", x=factor_var)
  if (class(smb) != "character") {
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

for(continuous_var in continuous_vars){
  smb <- smbinning(dataset2_2, y="Attrition", x=continuous_var) 
  if(class(smb) != "character"){ 
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}
iv_df <- iv_df[order(-iv_df$IV), ]
iv_df


tapply(dataset2_2$Attrition, dataset2_2$WorkLocation, sum)
tapply(dataset2_2$Available, dataset2_2$WorkLocation, sum)
tapply(dataset2_2$Last30DaysLeaveCount, dataset2_2$Availability_Filter, mean)
table(dataset2_2$Shift, dataset2_2$Availability_Filter)
table(dataset2_2$MaritalStatus, dataset2_2$Availability_Filter)
MarriedLeft <- 168/(168+280); MarriedLeft
UnmarriedLeft <- 1116/(1316+1116); round(UnmarriedLeft,2)
table(dataset2_2$Course, dataset2_2$Availability_Filter)


nums <- sapply(dataset2_2, is.numeric)
numdataset2 <- dataset2_2[,nums]

cor(numdataset2)


library(corrplot)
# Correlations work for numeric variables only.

cor <- cor(numdataset2, use="pairwise", method="pearson")
summary(cor)
# Order the correlations by their strength.

ord <- order(cor[1,])
str(ord)
ccor <- cor[ord, ord]
print(ccor)
# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation Imputed Data.csv using Pearson",
      sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

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
sum(Current$Available) # 1500


nlevels(Current$EmployeeCode); nlevels(Current$EmployeeName) # 10642
nlevels(Current$RptEmployeeCode); nlevels(Current$RptEmployeeName) # 536
nlevels(Current$Rpt2EmployeeCode); nlevels(Current$Rpt2EmployeeName) # 179
table(format(Current$DateofJoin, '%Y'))
table(Current$Status, format(Current$DateofJoin, '%Y'), useNA="ifany")
table(format(Current$DateOfRelieving, '%Y'))
table(format(Current$DateofJoin, '%Y'), format(Current$DateOfRelieving, '%Y'), useNA="ifany")
table(Current$JobRole, Current$Status)

sort(tapply(Current$Attrition, Current$RptEmployeeName, sum, na.rm=TRUE), decreasing = TRUE)
data.frame(sort(tapply(Current$Attrition, Current$Rpt2EmployeeName, sum, na.rm=TRUE), decreasing = TRUE))
table(Current$Status, format(Current$DateofBirth, '%Y'))


# table(Current$MaritalStatus, Current$JobRole, Current$Status)
table(Current$MaritalStatus, Current$Gender, Current$Status)
table(Current$TransportMode, Current$Status)
table(Current$WorkFacility, Current$Status)
table(Current$WorkLocation, Current$Status)
table(Current$PrevWorkFacility, Current$Status)
table(Current$PrevWorkLocation, Current$Status)
data.frame(table(Current$Function, Current$Status))
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
data.frame(with(TM[TM$ProdAvgLast3Months==0,],table(AGSExperienceInMonths)))
data.frame(with(active[active$ProdAvgLast3Months==0,],table(AGSExperienceInMonths)))
data.frame(with(inactive[inactive$ProdAvgLast3Months==0,],table(AGSExperienceInMonths)))
table(format(TM$DateOfRelieving, '%Y'), useNA = "ifany")
sum(length(which(inactive$ProdAvgLast3Months==0 & inactive$JobRole == "Team Member" & inactive$VerticalName == "Accounts Receivable")))
sum(length(which(active$ProdAvgLast3Months==0 & active$JobRole == "Team Member" & active$VerticalName == "Accounts Receivable")))
################ Graphical ##################
boxplot(Current$ProdAvgLast3Months ~ Current$Status) # 825 zeroes (ie 25.8 %) 241 Currrent and 584 past
hist(Current$ProdAvgLast3Months, xlab = "Prod Avg Last 3Months", main="Frequency Plot Prod Avg", 
     breaks = 100, labels = TRUE)
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


library(Hmisc)
describe(dataset)
library(fBasics)
basicStats(dataset$Attrition, ci=0.95)

kurtosis(dataset[c("MaritalState" , "ExperienceInAGS", "QualAvgDuringNotice", "Last30DaysLeaveCount", "TotalExtraHoursWorked")])
skewness(dataset[c("MaritalState" , "ExperienceInAGS", "QualAvgDuringNotice", "Last30DaysLeaveCount", "TotalExtraHoursWorked")])

library(descr)

CrossTable(dataset$MaritalStatus, dataset$Availability_Filter, expected = TRUE)

chisq.test(table(dataset$MaritalStatus, dataset$Attrition))

chisq.test(table(dataset$Gender, dataset$Availability_Filter))
CrossTable(dataset$Course, dataset$Availability_Filter)
chisq.test(table(dataset$Course, dataset$Availability_Filter),simulate.p.value = TRUE)
str(dataset$ExperienceInAGS)




numeric <- data.frame(x=dataset[c("ExperienceInAGS", "EmployeeAge", "LastReviewRating", "ProdAvgDuringNotice",
             "QualAvgDuringNotice", "Last30DaysLeaveCount", "TotalExtraHoursWorked", "Distance",
             "TravelTime", "Gender")], y= as.factor(dataset$Gender))



str(numeric)
vif_func(numeric) # Toooooooooo Severe for the system
# fisher.test(dataset$Course, dataset$Availability_Filter,simulate.p.value=TRUE, B=1e7)

library(caret)


dummies <- predict(dummyVars(~ Gender, data = dataset), newdata = dataset)
head(dummies, n = 3)




binom <- data.frame(y=runif(1e5), x=runif(1e5), catVar=as.factor(sample(0:4,1e5,TRUE)))
head(binom)
c <- model.matrix(~ x + catVar,binom) 
head(c)
### Strength of Association
library(cramer)
# Toooooooooo Severe for the system
# cramer.test(dataset$ExperienceInAGS, dataset$Attrition)

library(vcd)
assocstats(table(dataset$MaritalStatus, dataset$Attrition))


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

# forward <- step(logit1, scope = list(lower=formula(logit0), upper=formula(logit_full)), direction="forward", trace=0)
# summary(forward)
# 
# backward <- step(logit1, scope = list(lower=formula(logit0), upper=formula(logit_full)), direction="backward", trace=0)
# summary(backward)
# anova(stepwise,  forward, test = 'Chisq')

# cat(sprintf("Chi-square p-value: %.8f\n", dchisq(logit1$null.deviance-logit1$deviance, logit1$df.null-logit1$df.residual)))






############## Chi Square Test ############
chisq.test(dataset2$EngagementIndex, dataset2$Attrition)
chisq.test(dataset2$AGSExperienceInMonths, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$Gender, dataset2$Attrition)
chisq.test(dataset2$QualAvgLast3Months, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$FunctionName, dataset2$Attrition)
chisq.test(dataset2$Shift3, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$WorkLocation, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$CourseLevels, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$ExperienceType, dataset2$Attrition)
chisq.test(dataset2$MaritalStatus, dataset2$Attrition)
chisq.test(dataset2$ProdAvgLast3Months, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$Last30DaysLeaveCount, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$TransportMode, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$EmployeeAge, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$JMonth, dataset2$Attrition)
chisq.test(dataset2$RMonth, dataset2$Attrition)
chisq.test(dataset2$Client, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$SubClient, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$TotalExtraHoursWorked, dataset2$Attrition, simulate.p.value = TRUE)

chisq.test(dataset2$LastReviewType, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$LastReviewRating, dataset2$Attrition, simulate.p.value = TRUE)

# chisq.test(dataset2$Distance, dataset2$Status, simulate.p.value = TRUE)
summary(sampledata2[c("MaritalStatus", "Course", "TransportMode", "EngagementIndex")])
library(descr)

# MaritalStatus
CrossTable(sampledata2$MaritalStatus, sampledata2$Availability_Filter, expected = TRUE)
chisq.test(table(sampledata2$MaritalStatus, sampledata2$Availability_Filter))

# Gender
CrossTable(sampledata2$Gender, sampledata2$Availability_Filter, expected = TRUE)
chisq.test(table(sampledata2$Gender, sampledata2$Availability_Filter))


# RptEmployeeCode
CrossTable(sampledata2$RptEmployeeCode, sampledata2$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
chisq.test(table(sampledata2$RptEmployeeCode, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# Rpt2EmployeeCode
CrossTable(sampledata2$Rpt2EmployeeCode, sampledata2$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
chisq.test(table(sampledata2$Rpt2EmployeeCode, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# ExperienceInAGS
CrossTable(sampledata2$ExperienceInAGS, sampledata2$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
chisq.test(table(sampledata2$ExperienceInAGS, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# Experience.Range
CrossTable(sampledata2$Experience.Range, sampledata2$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
chisq.test(table(sampledata2$Experience.Range, sampledata2$Availability_Filter))

# EmployeeAge
chisq.test(table(sampledata2$EmployeeAge, sampledata2$Availability_Filter), simulate.p.value=TRUE)

#Shift
# 08:00PM-05:00AM  have only 2 employee assigned to it
CrossTable(sampledata2$Shift, sampledata2$Availability_Filter, expected = TRUE)
chisq.test(table(sampledata2$Shift, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# TransportMode
chisq.test(table(sampledata2$TransportMode, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# WorkFacility
chisq.test(table(sampledata2$WorkFacility, sampledata2$Availability_Filter), simulate.p.value=TRUE)

#PrevShift
chisq.test(table(sampledata2$PrevShift, sampledata2$Availability_Filter), simulate.p.value=TRUE)

#  Prev WorkFacility
chisq.test(table(sampledata2$PrevWorkFacility, sampledata2$Availability_Filter), simulate.p.value=TRUE)

#  PrevWorkLocation
chisq.test(table(sampledata2$PrevWorkLocation, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# Job Role
chisq.test(table(sampledata2$JobRole, sampledata2$Availability_Filter))

# Designation
chisq.test(table(sampledata2$Designation, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# Work Location
chisq.test(table(sampledata2$WorkLocation, sampledata2$Availability_Filter))

# RptEmployeeCode
chisq.test(table(sampledata2$RptEmployeeCode, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# ReasonofLeaving
chisq.test(table(sampledata2$ReasonofLeaving, sampledata2$Availability_Filter))


# ExperienceType
chisq.test(table(sampledata2$ExperienceType, sampledata2$Availability_Filter))

# ExitType
chisq.test(table(sampledata2$ExitType, sampledata2$Availability_Filter))

# CurrentAddressCity
chisq.test(table(sampledata2$CurrentAddressCity, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# CurrentAddressPincode
chisq.test(table(sampledata2$CurrentAddressPincode, sampledata2$Availability_Filter), simulate.p.value=TRUE)


#  PermenantAddressCity
chisq.test(table(sampledata2$PermenantAddressCity, sampledata2$Availability_Filter), simulate.p.value=TRUE)


#  PermenantAddressPincode
chisq.test(table(sampledata2$PermenantAddressPincode, sampledata2$Availability_Filter), simulate.p.value=TRUE)



#  RptEffectiveFrom
chisq.test(table(sampledata2$RptEffectiveFrom, sampledata2$Availability_Filter), simulate.p.value=TRUE)


#  RptSpanofControl
chisq.test(table(sampledata2$RptSpanofControl, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# LastReviewType
chisq.test(table(sampledata2$LastReviewType, sampledata2$Availability_Filter))


chisq.test(table(sampledata2$LastReviewDate, sampledata2$Availability_Filter))

# LastReviewRating
chisq.test(table(sampledata2$LastReviewRating, sampledata2$Availability_Filter), simulate.p.value = TRUE)




#  LastButOneReviewType
chisq.test(table(sampledata2$LastButOneReviewType, sampledata2$Availability_Filter))


#  LastButOneReviewRating
chisq.test(table(sampledata2$LastButOneReviewRating, sampledata2$Availability_Filter), simulate.p.value=TRUE)


#  LastButOneReviewDate
chisq.test(table(sampledata2$LastButOneReviewDate, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# ProdAvgDuringNotice
CrossTable(sampledata2$ProdAvgDuringNotice, sampledata2$Availability_Filter, expected=TRUE)
chisq.test(table(sampledata2$ProdAvgDuringNotice, sampledata2$Availability_Filter))
chisq.test(table(sampledata2$ProdAvgDuringNotice, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# QualAvgDuringNotice
chisq.test(table(sampledata2$QualAvgDuringNotice, sampledata2$Availability_Filter), simulate.p.value=TRUE)



#  ProdAvgBeforeNotice
chisq.test(table(sampledata2$ProdAvgBeforeNotice, sampledata2$Availability_Filter), simulate.p.value=TRUE)



# QualAvgBeforeNotice
chisq.test(table(sampledata2$QualAvgBeforeNotice, sampledata2$Availability_Filter))




#  ProdAvgDuringNoticeRange
chisq.test(table(sampledata2$ProdAvgDuringNoticeRange, sampledata2$Availability_Filter))

# QualAvgDuringNoticeRange
chisq.test(table(sampledata2$QualAvgDuringNoticeRange, sampledata2$Availability_Filter), simulate.p.value = TRUE)


#  ProdAvgBeforeNoticeRange
chisq.test(table(sampledata2$ProdAvgBeforeNoticeRange, sampledata2$Availability_Filter))

#  QualAvgBeforeNoticeRange
chisq.test(table(sampledata2$QualAvgBeforeNoticeRange, sampledata2$Availability_Filter))



#  Process
chisq.test(table(sampledata2$Process, sampledata2$Availability_Filter), simulate.p.value = TRUE)




#  Client
chisq.test(table(sampledata2$Client, sampledata2$Availability_Filter))
chisq.test(table(sampledata2$Client, sampledata2$Availability_Filter), simulate.p.value = TRUE)


#  SubClient
chisq.test(table(sampledata2$SubClient, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# PrevVertical 
## AR support has only 1 employee left and 2 current employee
CrossTable(sampledata2$PrevVertical, sampledata2$Availability_Filter, expected = TRUE)
chisq.test(table(sampledata2$PrevVertical, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# PrevProcess
chisq.test(table(sampledata2$PrevProcess, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# PrevClient
chisq.test(table(sampledata2$PrevClient, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# PrevSubClient
chisq.test(table(sampledata2$PrevSubClient, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# PrevEmployer
# many employeer have 1 or 2 employee
CrossTable(sampledata2$PrevEmployer, sampledata2$Availability_Filter, expected=TRUE)
chisq.test(table(sampledata2$PrevEmployer, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# Course
CrossTable(sampledata2$Course, sampledata2$Availability_Filter, expected = TRUE, simulate.p.value=TRUE)
chisq.test(table(sampledata2$Course, sampledata2$Availability_Filter), simulate.p.value = TRUE)


chisq.test(table(sampledata2$Last30DaysLeaveCount, sampledata2$Availability_Filter), simulate.p.value=TRUE)




chisq.test(table(sampledata2$TravelTimeRange, sampledata2$Availability_Filter), simulate.p.value=TRUE)




chisq.test(table(sampledata2$TravelTime, sampledata2$Availability_Filter), simulate.p.value=TRUE)




#  EngagementIndex
CrossTable(sampledata2$EngagementIndex, sampledata2$Availability_Filter)
chisq.test(table(sampledata2$EngagementIndex, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# TotalExtraHoursWorked
chisq.test(table(sampledata2$TotalExtraHoursWorked, sampledata2$Availability_Filter))

# StaffingEmployeeStatus
chisq.test(table(sampledata2$StaffingEmployeeStatus, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# Shift_Name
chisq.test(table(sampledata2$Shift_Name, sampledata2$Availability_Filter), simulate.p.value=TRUE)

# Function
chisq.test(table(sampledata2$Function, sampledata2$Availability_Filter))

# Vertical
chisq.test(table(sampledata2$Vertical, sampledata2$Availability_Filter))



# Distance
chisq.test(table(sampledata2$Distance, sampledata2$Availability_Filter), simulate.p.value=TRUE)


# Course
chisq.test(table(sampledata2$Course, sampledata2$Availability_Filter), simulate.p.value=TRUE)




#  Designation
chisq.test(table(sampledata2$Designation, sampledata2$Availability_Filter), simulate.p.value=TRUE)


###################### Gender Vs Marital Status ###############
CrossTable(sampledata2$Gender, sampledata2$MaritalStatus, expected = TRUE)
chisq.test(sampledata2$Gender, sampledata2$MaritalStatus)


##################### VIF
significant1 <- sampledata2[c("QualAvgBeforeNotice", "ProdAvgDuringNotice", "ProdAvgBeforeNotice",  "LastReviewType",  "LastButOneReviewType",
                              "ExperienceInAGS", "WorkFacility", "Last30DaysLeaveCount", "TravelTime", "EngagementIndex", "LastButOneReviewRating", 
                              "JobRole", "QualAvgDuringNotice", "StaffingEmployeeStatus", "Function", "WorkLocation", "LastReviewRating", "Shift_Name", 
                              "PermenantAddressPincode", "CurrentAddressPincode")] 
str(significant1)
summary(significant1)
source("vif_func.R")
vif_func(sampledata2)


################# 4 Model 1 (all IMP) #######################
# LastReviewType + LastReviewRating
set.seed(144)
logit1 <- glm(Attrition ~  EngagementIndex + AGSExperienceInMonths + Gender
              + QualAvgLast3Months + FunctionName + Shift3 + WorkLocation + CourseLevels
              + ExperienceType + MaritalStatus + ProdAvgLast3Months  + Last30DaysLeaveCount
              + TransportMode + EmployeeAge + JMonth + Client 
              + TotalExtraHoursWorked
              , family = binomial(link = "logit"), 
              data = dataset2)
summary(logit1)
cor(logit1$y, logit1$fitted.values)
cor(logit1$y, logit1$fitted.values)
# library(car)
# car::vif(logit1) 
# ld_var <- attributes(alias(logit1)$Complete)$dimnames[[1]]
# summary(ld_var)
cat(sprintf("Chi-square p-value: %.8f\n", dchisq(logit1$null.deviance-logit1$deviance, logit1$df.null-logit1$df.residual)))
library(car)
vif(logit1)
# plot(logit1)

##
cat(sprintf("Log likelihood: %.3f (%d df)\n",
            logLik(logit1)[1],
            attr(logLik(logit1), "df")))
cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
            logit1$null.deviance-logit1$deviance,
            logit1$df.null-logit1$df.residual))
cat(sprintf("Chi-square p-value: %.8f\n",
            dchisq(logit1$null.deviance-logit1$deviance,
                   logit1$df.null-logit1$df.residual)))
cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
            cor(logit1$y, logit1$fitted.values)))
cat('\n==== ANOVA ====\n\n')
print(anova(logit1, test="Chisq"))
cat("\n")



################# ROC AND AUC
library(ROCR)

PredTrainROC1 = prediction(predTrain,  training$Availability_Filter)
PerfTrainROC1 = performance(PredTrainROC1, "tpr", "fpr")
plot(PerfTrainROC1, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), main="ROC Plot")
as.numeric(performance(PredTrainROC1, "auc")@y.values)

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
library(InformationValue)
precision(dataset3$Attrition, predAll1)
sensitivity(dataset3$Attrition, predAll1)
specificity(dataset3$Attrition, predAll1)
t3 <- t[,c(21,18,1:17, 22)]; names(t3)

##################### 5 prediciton on all data ####################
predAll1 <- predict(logit1, dataset2, "response" )

table(dataset2$Status, predAll1 > 0.5)

(1402+1382)*100/(1402+1382+166+215) # 87.96209

predAll1 <- predict(logit1, type="response", newdata=dataset3)
tapply(predAll1, dataset3$Attrition, mean)

table(dataset3$Attrition, predAll1 > 0.5)
library(InformationValue)
precision(dataset3$Attrition, predAll1)
sensitivity(dataset3$Attrition, predAll1)
specificity(dataset3$Attrition, predAll1)
t3 <- t[,c(21,18,1:17, 22)]; names(t3)
plotROC(testing$Attrition, predTest1, Show.labels = 1)
################# 6 Export Model Score #################

t <- cbind(predAll1, dataset3 )
head(t)

write.csv(t, file="C:/Users/makarand.ghule/Documents/AttritionAnalysis/ARFollowUp_Logistic_Score2.csv", row.names=FALSE)


TNA <- is.na(predAll1)
NAV <- predAll1[-TNA]
nrow(NAV)


########################## 7 Diagnostic #############################

#============================================================

# Evaluate model performance. 

# Generate an Error Matrix for the Linear model.

# Obtain the response from the Linear model.

MYpr <- as.vector(ifelse(predict(logit1, type="response", newdata=test) > 0.5, "TERMINATED", "ACTIVE"))
MYpr

# Generate the confusion matrix showing counts.

table(test$Attrition, MYpr,dnn=c("Actual", "Predicted"))


# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x)
  tbl <- cbind(x/length(actual),
               Error=sapply(1:nc,
                            function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(test$Attrition, MYpr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))



#============================================================

# Evaluate model performance. 
# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the glm model on MFG10YearTerminationData [test].

MYpr <- predict(logit1, type="response", newdata=test)

# Remove observations with missing target.

no.miss   <- na.omit(test$Attrition)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Linear test$Attrition")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(MYtest[c(MYinput, MYtarget)]$STATUS)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(MYpr[-miss.list], no.miss)
} else
{
  pred <- prediction(MYpr, no.miss)
}
performance(pred, "auc")




# Hosmer-Lemeshow Goodness of Fit

# How well our model fits depends on the difference between the model and the observed data.  

install.packages("ResourceSelection")
library(ResourceSelection)

hoslem.test(training$Available, fitted(logit1))


################# 6 Export Model Score #################

t <- cbind(dataset2, predAll1)
t$Prediction <- ifelse(predAll1>0.5, "Leave", "Stay")
write.csv(t, file="AR_Logit_Score_R0.csv", row.names=FALSE)





######################### random forest and decision Tree ###############


library(ggplot2)
pred = prediction(pred2,  dataset3$Availability_Filter)

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Linear 20161116ARFollowUp.csv Availability_Filter")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                  label=paste("AUC =", round(au, 2)))
print(p)

########################################### RANDOM FOREST ###################################


library(randomForest)
set.seed(200)
library(caret)
# install.packages("e1071")
library(e1071)

# Define cross-validation experiment
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = (1:50)*0.01) 


rforest1 <- randomForest(Availability_Filter ~ ExperienceInAGS + EmployeeAge + Gender + MaritalStatus + WorkLocation + JobRole + 
                           ExperienceType + ProdAvgDuringNotice + Course + Last30DaysLeaveCount + TotalExtraHoursWorked + 
                           Function + Shift + TransportMode + EngagementIndex, 
                         data=training, ntree=200, nodesize=25, method = "rpart", trControl = fitControl, tuneGrid = cartGrid  )

################## prediction on training dataset
set.seed(50)
predictForest <- predict(rforest1)
table(training$Availability_Filter, predictForest)
accTrain <- (1064+948)/(1064+133+90+948); accTrain
senTrain <- 948/(948+90); senTrain
speTrain <- 1064/(1064+133); speTrain


# predictionon Test dataset

set.seed(50)
predictForest2 <- predict(rforest1, newdata=testing)
table(testing$Availability_Filter, predictForest2)
accRFTest <- (302+190)/(302+27+24+190); accRFTest
sensRFTest <- 190/(190+24); sensRFTest
specRFTest <- 302/(302+27); specRFTest



############ prediciton on all data ##########
dataset3 <- rbind(training, testing)
predRF2 <- predict(rforest1, newdata = dataset3, type = "class")

tRF <- cbind(predRF2, dataset3 )
head(tRF)

write.csv(tRF, file="C:/Users/makarand.ghule/Documents/AttritionAnalysis/20161116ARFollowUp_all_scoreRF2.csv", row.names=FALSE)

library(ggplot2)
predRF = prediction(as.numeric(predRF2),  as.numeric(dataset3$Availability_Filter))

pe <- performance(predRF, "tpr", "fpr")
au <- performance(predRF, "auc")@y.values[[1]]


plot(au, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), main="ROC Plot")
as.numeric(performance(predAllROC2, "auc")@y.values)



randomForest::varImpPlot(rforest1, main="Variable Importance Random Forest")

# Plot the error rate against the number of trees.

plot(rforest1, main="Error Rates Random Forest")
legend("topright", c("OOB", "Current Employee", "Employee Left"), text.col=1:6, lty=1:3, col=1:3)




############################ Decision Tree ######################################


library(rpart)
library(rpart.plot)

rTree <- rpart(Availability_Filter ~ ExperienceInAGS + EmployeeAge + Gender + MaritalStatus + WorkLocation + JobRole + 
                 ExperienceType + ProdAvgDuringNotice + Course + Last30DaysLeaveCount + TotalExtraHoursWorked + 
                 Function + Shift + TransportMode + EngagementIndex, 
               data=training, method="class",  control=rpart.control(minbucket=25))
prp(rTree)
print(rTree)
printcp(rTree)
predictCARTTrain <- predict(rTree, type="class")
table(predictCARTTrain, training$Availability_Filter)
accCARTTrain <- (1032+940)/(1032+98+165+940); accCARTTrain
sensCARTTrain <- 940/(940+165); sensCARTTrain
specCARTTrain <- 1032/(1032+98); specCARTTrain

## testing Data ####
PredictROC = predict(rTree, newdata = testing, type="class")
table(PredictROC, testing$Availability_Filter)
accCARTTest <- (338+313)/(338+33+61+313); accCARTTest
sensCARTTest <- 313/(313+61); sensCARTTest
specCARTTest <- 338/(33+338); specCARTTest
library(ROCR)


## All Data ####
PredictROC = predict(rTree, newdata = dataset3, type="class")
table(PredictROC, dataset3$Availability_Filter)
accCARTAll <- (1370+1253)/(1370+131+226+1253); accCARTAll
sensCARTAll <- 1253/(1253+226); sensCARTAll
specCARTAll <- 1370/(1370+131); specCARTAll
library(ROCR)

dataset3 <- rbind(training, testing)

nrow(dataset3)
t <- cbind(PredictROC, dataset3 )
head(t)

write.csv(t, file="C:/Users/makarand.ghule/Documents/AttritionAnalysis/20161116ARFollowUp_ROCR.csv", row.names=FALSE)







predAllROC <- predict(rTree, newdata=dataset3, type="class")
predAllROC2 = prediction(as.numeric(predAllROC), as.numeric(dataset3$Availability_Filter))
perfTree <- performance(predAllROC2, "tpr", "fpr")

plot(perfTree, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), main="ROC Plot")
as.numeric(performance(predAllROC2, "auc")@y.values)





## model comparisons
plot(perfTree, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7), main="ROC Plot")








### Leverage 
lev=hat(model.matrix(logit1))
plot(lev)
infl <- training[lev>0.2,]
infl


cook  = cooks.distance(logit1)
plot(cook,ylab="Cooks distances")
points(infl, cook[infl], col="red")




##arrange df vars by position
##'vars' must be a named vector, e.g. c("var.name"=1)
arrange.vars <- function(data, vars){
                          ##stop if not a data.frame (but should work for matrices as well)
                          stopifnot(is.data.frame(data))
                          
                          ##sort out inputs
                          data.nms <- names(data)
                          var.nr <- length(data.nms)
                          var.nms <- names(vars)
                          var.pos <- vars
                          ##sanity checks
                          stopifnot( !any(duplicated(var.nms)), 
                                     !any(duplicated(var.pos)) )
                          stopifnot( is.character(var.nms), 
                                     is.numeric(var.pos) )
                          stopifnot( all(var.nms %in% data.nms) )
                          stopifnot( all(var.pos > 0), 
                                     all(var.pos <= var.nr) )
                          
                          ##prepare output
                          out.vec <- character(var.nr)
                          out.vec[var.pos] <- var.nms
                          out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
                          stopifnot( length(out.vec)==var.nr )
                          
                          ##re-arrange vars by position
                          data <- data[ , out.vec]
                          return(data)
}

t2 <- arrange.vars(t, c("Prediction"=1, "Status"=2))



























































