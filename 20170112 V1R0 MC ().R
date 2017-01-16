#Not considered Shift and is.na(Client) <- 'AGS'
# install.packages("RODBC")
library(RODBC)
odbcCloseAll()
edw_bi <- odbcConnect("PulseDB_EDW_BI")
active <- sqlFetch(edw_bi, "AttAnalysis_tblActiveEmployees")
inactive <- sqlFetch(edw_bi, "AttAnalysis_tblInActiveEmployees")
dataa <- rbind(active, inactive)
AR <- subset(dataa, dataa$Department == "Medical Coding")
TM <- subset(AR, JobRole == "Team Member")
Current <- subset(TM, as.numeric(format(TM$DateOfRelieving, '%Y')) == 1900 | as.numeric(format(TM$DateOfRelieving, '%Y'))  >= 2015 )

################ Attrition ##########

# only 2 > on the basis that we'll not have date for current employees as told by sid (his discussion with IT) on 23 Dec
Current$Attrition = ifelse(as.numeric(format(Current$DateOfRelieving, '%Y')) == 1900, 0, 1)
Current$Available = ifelse(as.numeric(format(Current$DateOfRelieving, '%Y')) != 1900, 0, 1)
Current$Status = as.factor(ifelse(as.numeric(format(Current$DateOfRelieving, '%Y')) == 1900, "Current", "Past"))

########## EmployeeAge #######
library(lubridate)

Current$EmployeeAge <- ifelse(Current$Status=="Current", interval(Current$DateofBirth, today())/duration(num=1, units = "years"), interval(Current$DateofBirth, Current$DateOfRelieving)/duration(num=1, units = "years"))


############ modified variables ###############
y <- addNA(Current$Client)
levels(y)[is.na(levels(y))] <- "AGS" #  Maximum number of categories (50) exceeded
Current$Client <- y
summary(Current$Client)

############### New variables ###############
JMonth <- month(Current$DateofJoin)
RMonth <- month(Current$DateOfRelieving)
Current$JMonth <- as.factor(JMonth)
Current$RMonth <- as.factor(RMonth)

##################### Use limited data ############
# "Shift2", 

dataset2 <- Current[c(  "EmployeeCode", "EmployeeName", "AGSExperienceInMonths", "Gender", "EmployeeAge"
                      , "JMonth", "RMonth", "MaritalStatus",  "WorkLocation", "ExperienceType", "ProdAvgLast3Months"
                      , "QualAvgLast3Months", "CourseLevels", "Last30DaysLeaveCount","TotalExtraHoursWorked"
                      , "LastReviewType", "LastReviewRating", "Client", "SubClient" 
                      , "TransportMode","EngagementIndex", "FunctionName", "Status", "Attrition", "Available")]

summary(dataset2)

#################  Imputation ###################
library(mice) 
simple <- dataset2[c("MaritalStatus", "CourseLevels", "EngagementIndex")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)
dataset2$MaritalStatus = imputed$MaritalStatus
dataset2$CourseLevels = imputed$CourseLevels
dataset2$EngagementIndex = imputed$EngagementIndex

############## Chi Square Test ############
chisq.test(dataset2$EngagementIndex, dataset2$Attrition)
chisq.test(dataset2$AGSExperienceInMonths, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$Gender, dataset2$Attrition)
chisq.test(dataset2$QualAvgLast3Months, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$FunctionName, dataset2$Attrition, simulate.p.value = TRUE)
# chisq.test(dataset2$Shift2, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$WorkLocation, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$CourseLevels, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$ExperienceType, dataset2$Attrition)
chisq.test(dataset2$MaritalStatus, dataset2$Attrition)
chisq.test(dataset2$ProdAvgLast3Months, dataset2$Attrition, simulate.p.value = TRUE)
chisq.test(dataset2$Last30DaysLeaveCount, dataset2$Attrition, simulate.p.value = TRUE)
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
# LastReviewType + LastReviewRating + Shift3
set.seed(144)
logit1 <- glm(Attrition ~  EngagementIndex + AGSExperienceInMonths + Gender
              + QualAvgLast3Months + FunctionName + WorkLocation + CourseLevels
              + ExperienceType + MaritalStatus + ProdAvgLast3Months  + Last30DaysLeaveCount
              + EmployeeAge + JMonth + Client 
              + TotalExtraHoursWorked
              , family = binomial(link = "logit") 
              , data = dataset2)
summary(logit1)

##################### 5 prediciton on all data ####################
predAll1 <- predict(logit1, dataset2, "response" )

table(dataset2$Status, predAll1 > 0.5)

(1221+987)*100/(1221+69+123+987) # 92
################# 6 Export Model Score #################

t <- cbind(dataset2, predAll1)
t$Prediction <- ifelse(predAll1>0.5, "Leave", "Stay")
write.csv(t, file="MC_Logit_Score_R0.csv", row.names=FALSE) 
