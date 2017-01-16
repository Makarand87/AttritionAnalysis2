
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

##################### 5 prediciton on all data ####################
predAll1 <- predict(logit1, dataset2, "response" )

table(dataset2$Status, predAll1 > 0.5)

(1402+1382)*100/(1402+1382+166+215) # 87.96209
################# 6 Export Model Score #################

t <- cbind(dataset2, predAll1)
t$Prediction <- ifelse(predAll1>0.5, "Leave", "Stay")
write.csv(t, file="AR_Logit_Score_R0.csv", row.names=FALSE)