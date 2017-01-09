setwd("C:/Users/makarand.ghule/Documents/AttritionAnalysis/20161220 P3 (SQL)")
# install.packages("RODBC")
library(RODBC)
odbcCloseAll()# odbcClose(edw_bi)
edw_bi <- odbcConnect("PulseDB_EDW_BI")
# head(sqlTables(edw_bi, tableType = "TABLE", schema = "dbo"), 10)
active <- sqlFetch(edw_bi, "AttAnalysis_tblActiveEmployees"); nrow(active)
inactive <- sqlFetch(edw_bi, "AttAnalysis_tblInActiveEmployees"); nrow(inactive)
dataa <- rbind(active, inactive); nrow(dataa); nrow(active) +  nrow(inactive) 
AR <- subset(dataa, dataa$VerticalName == "Accounts Receivable");nrow(AR) # 4409
TM <- subset(AR, JobRole == "Team Member"); nrow(TM); # 4157
Current <- subset(TM, as.numeric(format(TM$DateOfRelieving, '%Y')) == 1900 | as.numeric(format(TM$DateOfRelieving, '%Y'))  >= 2015 ); nrow(Current) # 3072

names(Current)
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

levels(Current$Shift3) <- c("08:00 AM-05:00 PM", "06:00 PM-03:00 AM", "06:00 PM-03:00 AM", "06:00 PM-03:00 AM",
                            "08:00 AM-05:00 PM" ,"06:00 PM-03:00 AM", "08:00 AM-05:00 PM", "06:00 PM-03:00 AM",
                            "08:00 AM-05:00 PM" ,"08:00 AM-05:00 PM", "08:00 AM-05:00 PM", "08:00 AM-05:00 PM" )

table(Current$Shift3, Current$Status)
Current$Shift=NULL
Current$Shift2=NULL
########## EmployeeAge #######
library(lubridate)

Current$EmployeeAge <- ifelse(Current$Status=="Current", interval(Current$DateofBirth, today())/duration(num=1, units = "years"), interval(Current$DateofBirth, Current$DateOfRelieving)/duration(num=1, units = "years"))
summary(Current$EmployeeAge)

############ modified variables ###############
levels(Current$TransportMode)[levels(Current$TransportMode)=="- No Transport -"] <- NA
Current$LastReviewDate2 <- as.Date(Current$LastReviewDate, '%Y-%m-%d')

############### New variables ###############
Current$JMonth <- month(Current$DateofJoin)
Current$RMonth <- month(Current$DateOfRelieving)

##################### Use limited data #############

dataset2 <- Current[c(  "EmployeeCode", "EmployeeName", "AGSExperienceInMonths", "Gender", "EmployeeAge"
                      , "JMonth", "RMonth", "MaritalStatus",  "WorkLocation", "ExperienceType", "ProdAvgLast3Months"
                      , "QualAvgLast3Months", "CourseLevels", "Last30DaysLeaveCount","TotalExtraHoursWorked"
                      , "LastReviewType", "LastReviewRating", "Client", "SubClient" 
                      , "Shift3", "TransportMode","EngagementIndex", "FunctionName", "Status", "Attrition", "Available")]

summary(dataset2)


#################  Imputation ###################
library(mice) 
simple <- dataset2[c("MaritalStatus", "CourseLevels", "EngagementIndex", "TransportMode")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)
dataset2$MaritalStatus = imputed$MaritalStatus
dataset2$CourseLevels = imputed$CourseLevels
dataset2$TransportMode = imputed$TransportMode
dataset2$EngagementIndex = imputed$EngagementIndex

######################## 3 Randomly split data ##########################
library(caTools)
set.seed(88)
split = sample.split(dataset2$Attrition, SplitRatio = 0.75)
training=subset(dataset2, split==TRUE); nrow(training) # 2304
testing = subset(dataset2, split==FALSE); nrow(testing) # 768
# summary(training)
str(training)
dataset3 <- rbind(training, testing)
############## Chi Square Test ############
chisq.test(dataset2$EngagementIndex, dataset2$Attrition)
chisq.test(dataset2$AGSExperienceInMonths, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$QualAvgLast3Months, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$FunctionName, dataset2$Attrition)
chisq.test(dataset2$WorkLocation, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$ExperienceType, dataset2$Attrition)
chisq.test(dataset2$MaritalStatus, dataset2$Attrition)
chisq.test(dataset2$ProdAvgLast3Months, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$Gender, dataset2$Attrition)
chisq.test(dataset2$CourseLevels, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$TransportMode, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$LastReviewType, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$LastReviewRating, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$EmployeeAge, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$JMonth, dataset2$Attrition)
chisq.test(dataset2$RMonth, dataset2$Attrition)
chisq.test(dataset2$Client, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$SubClient, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$Shift3, dataset2$Attrition, simulate.p.value = TRUE)


################# 4 Model 1 (all IMP) #######################
# + Last30DaysLeaveCount+ TotalExtraHoursWorked + LastReviewType + LastReviewRating
logit1 <- glm(Attrition ~  EngagementIndex + AGSExperienceInMonths
              + QualAvgLast3Months + FunctionName + Shift3 + WorkLocation
              + ExperienceType + MaritalStatus + ProdAvgLast3Months 
              + TransportMode + EmployeeAge + JMonth + Client
              , family = binomial(link = "logit"), 
              data = training)
summary(logit1)

library(car)
vif(logit1)

##################### 5 prediciton on all data ####################
predAll1 <- predict(logit1, type="response", newdata=dataset3)
tapply(predAll1, dataset3$Attrition, mean, na.rm=TRUE)

table(dataset3$Attrition, predAll1 > 0.5)

(1258+1184)/(1258+316+389+1184) # 77.5977
(1331+1051)/(1331+243+324+1051) # 80.77314 after considering JMonth, Client

################# 6 Export Model Score #################

t <- cbind(dataset3, predAll1)
t$Prediction <- ifelse(predAll1>0.5, "Leave", "Stay"); names(t)
write.csv(t, file="AR_Logit_Score_R0.csv", row.names=FALSE)


