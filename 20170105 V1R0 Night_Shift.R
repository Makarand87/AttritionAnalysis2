setwd("C:/Users/makarand.ghule/Documents/AttritionAnalysis/20161220 P3 (SQL)")
# install.packages("RODBC")
library(RODBC)
odbcCloseAll()# odbcClose(edw_bi)
edw_bi <- odbcConnect("PulseDB_EDW_BI")
# head(sqlTables(edw_bi, tableType = "TABLE", schema = "dbo"), 10)
active <- sqlFetch(edw_bi, "AttAnalysis_tblActiveEmployees"); nrow(active) #4969
inactive <- sqlFetch(edw_bi, "AttAnalysis_tblInActiveEmployees"); nrow(inactive) #5673
dataa <- rbind(active, inactive); nrow(dataa); nrow(active) +  nrow(inactive); ncol(dataa) #10642 # 84
AR <- subset(dataa, VerticalName == "Accounts Receivable");nrow(AR) #  4418
TM <- subset(AR, JobRole == "Team Member"); nrow(TM); # 4165
Current <- subset(TM, as.numeric(format(TM$DateOfRelieving, '%Y')) == 1900 | as.numeric(format(TM$DateOfRelieving, '%Y'))  >= 2015 ); nrow(Current) # 3072



################ Attrition ##########

Current$Attrition = ifelse(as.numeric(format(Current$DateOfRelieving, '%Y')) == 1900, 0, 1); sum(Current$Attrition) # 1572
Current$Available = ifelse(as.numeric(format(Current$DateOfRelieving, '%Y')) != 1900, 0, 1); sum(Current$Available) # 1500
Current$Status = as.factor(ifelse(as.numeric(format(Current$DateOfRelieving, '%Y')) == 1900, "Current", "Past")); table(Current$Status)


######### Create Shift ####
data.frame(table(Current$Shift))
table(Current$Shift, Current$Status)
Current$Shift2 <- substr(Current$Shift, 1, 17)
table(Current$Shift2, Current$Status)

Current$Shift3 <- factor(Current$Shift2)

# Combining Other shifts too as these shift have very few employee working 

levels(Current$Shift3) <- c("08:00 AM-05:00 PM", "06:00 PM-03:00 AM", "06:00 PM-03:00 AM", "06:00 PM-03:00 AM",
                            "08:00 AM-05:00 PM" ,"06:00 PM-03:00 AM", "08:00 AM-05:00 PM", "06:00 PM-03:00 AM",
                            "08:00 AM-05:00 PM" ,"08:00 AM-05:00 PM", "08:00 AM-05:00 PM", "08:00 AM-05:00 PM" )

table(Current$Shift3, Current$Status)
Current$Shift=NULL
Current$Shift2=NULL
Night <- subset(Current, Shift3=='06:00 PM-03:00 AM'); nrow(Night) #2614



##################### Use limited data #############

dataset2 <- Night[c("EmployeeCode", "EmployeeName", "AGSExperienceInMonths", "Gender"
                  , "MaritalStatus",  "WorkLocation", "ExperienceType", "ProdAvgLast3Months"
                  , "QualAvgLast3Months", "CourseLevels", "Last30DaysLeaveCount","TotalExtraHoursWorked"
                  , "Shift3", "TransportMode","EngagementIndex", "FunctionName", "Status", "Attrition", "Available")]

summary(dataset2)
levels(dataset2$TransportMode)[levels(dataset2$TransportMode)=="- No Transport -"] <- NA




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
training=subset(dataset2, split==TRUE); nrow(training) # 401
testing = subset(dataset2, split==FALSE); nrow(testing) # 133
# summary(training)
str(training); summary(training)
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

################# 4 Model 1 (all IMP) #######################

logit1 <- glm(Attrition ~  EngagementIndex + AGSExperienceInMonths + 
                QualAvgLast3Months  + FunctionName + WorkLocation + 
                ExperienceType + ProdAvgLast3Months + TransportMode
              , family = binomial(link = "logit"), 
              data = training)
summary(logit1)

library(car)
vif(logit1)

##################### 5 prediciton on all data ####################
predAll1 <- predict(logit1, type="response", newdata=dataset3)
tapply(predAll1, dataset3$Attrition, mean)

table(dataset3$Attrition, predAll1 > 0.5)

(917+1049)/(917+313+335+1049) # 75.21 %

################# 6 Export Model Score #################

t <- cbind(dataset3, predAll1)
t$Prediction <- ifelse(predAll1>0.5, "Leave", "Stay"); names(t)
write.csv(t, file="AR_Night_Logit_Score_R0.csv", row.names=FALSE)


