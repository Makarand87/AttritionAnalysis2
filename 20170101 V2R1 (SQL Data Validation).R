setwd("C:/Users/makarand.ghule/Documents/AttritionAnalysis/20161220 P3 (SQL)")

library(RODBC)
odbcCloseAll()
edw_bi <- odbcConnect("PulseDB_EDW_BI", uid = "edw", pwd = "BqSC&*UV^Skx6RtH")
head(sqlTables(edw_bi, tableType = "Table"))
# data <- sqlFetch(edw_bi, "AttAnalysis_tblActiveEmployees")
data <- (sqlFetch(edw_bi, "AttAnalysis_tblInActiveEmployees"))

attach(data)
count <- nrow(data); count

### indata Table ####
## 1.NA ####
na_list <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_list); #na_count

zero_list <-sapply(data, function(y) sum(length(which((y==0)))))
zero_count <- data.frame(zero_list); #zero_count

uniq_list <- sapply(data, function(y) sum(length(unique(y))))
uniq_count <- data.frame(uniq_list); #uniq_count

# merge all these tables
data_prop <- cbind(na_count, zero_count, uniq_count)
data_prop$variable <-rownames(data_prop)
rownames(data_prop) <- NULL
data_prop 


table(format(DateofBirth, '%Y')) 
count-sum(table(format(DateofBirth, '%Y')))

table(format(DateofJoin, '%Y'))
count-sum(table(format(DateofJoin, '%Y')))

table(format(DateOfRelieving, '%Y'))
sum(is.na(DateOfRelieving))

table(AttritionMonth)

table(ExpAGSRange, AGSExperienceInMonths)

table(Shift)
table(Shift, PrevShift)
table(PrevShift)
sum(table(PrevShift))

table(MaritalStatus); count-sum(table(MaritalStatus))
table(Gender); count-sum(table(Gender))
table(TransportMode); sum(table(TransportMode))

table(WorkFacility); sum(table(WorkFacility))
table(WorkLocation); sum(table(WorkLocation))
table(PrevWorkFacility); sum(table(PrevWorkFacility))
table(PrevWorkFacility, WorkFacility)
table(PrevWorkLocation); sum(table(PrevWorkLocation))
table(JobRole); sum(table(JobRole))
table(ReasonofLeaving); sum(table(ReasonofLeaving))
table(ExperienceType); sum(table(ExperienceType))
table(ExitType); sum(table(ExitType))

Fresher <- subset(data, ExperienceType=='Fresher')
sum(length(which(!is.na(Fresher$PreviousExperienceInMonths))))
sum(length(which(Fresher$PreviousExperienceInMonths>6)))
sum(length(which(!is.na(Fresher$PrevEmployer))))


IFresher <- subset(data, ExperienceType=='Industry Fresher')
sum(length(which(is.na(IFresher$PreviousExperienceInMonths))))
sum(length(which(is.na(IFresher$PrevEmployer))))
sum(length(which(IFresher$PreviousExperienceInMonths==0)))

Lateral <- subset(data, ExperienceType=='Lateral')
sum(length(which(is.na(Lateral$PreviousExperienceInMonths))))
sum(length(which(Lateral$PreviousExperienceInMonths==0)))
sum(length(which(is.na(Lateral$PrevEmployer))))


table(ExitType); sum(table(ExitType))
table(CurrentAddressCity); sum(table(CurrentAddressCity))

table(format(as.Date(RptEffectiveFrom), '%Y')); sum(table(format(as.Date(RptEffectiveFrom), '%Y')))

table(RptSpanofControl); sum(table(RptSpanofControl))

table(JobRole, Shift)

table(LastReviewType); sum(table(LastReviewType))

table(format(as.Date(LastReviewDate), '%Y')); sum(table(format(as.Date(LastReviewDate), '%Y')))
table(JobRole, format(as.Date(LastReviewDate), '%Y'))
# old <- subset(data, format(as.Date(LastReviewDate)) < 2016);nrow(old)
# table(old$JobRole, format(as.Date(old$LastReviewDate), '%Y'))


PrevEmployerA <- subset(data, !is.na(PrevEmployer))
table(PrevEmployerA$ExperienceType)
PrevEmployerNA <- subset(data, is.na(PrevEmployer))
table(PrevEmployerNA$ExperienceType)

tapply(ProdAvgLast3Months, JobRole, mean)
summary(ProdAvgLast3Months)
table(ProdAvgDuringNoticeRange); sum(table(ProdAvgDuringNoticeRange)); count-sum(table(ProdAvgDuringNoticeRange))

tapply(QualAvgLast3Months, JobRole, mean)
summary(QualAvgLast3Months)
table(QualAvgDuringNoticeRange); sum(table(QualAvgDuringNoticeRange)); count-sum(table(QualAvgDuringNoticeRange))

# tapply(ProdAvgBefore3MonthsNotice, data$Shift, mean);
# summary(ProdAvgBefore3MonthsNotice)
# table(ProdAvgBeforeNoticeRange)
# 
# tapply(QualAvgBefore3MonthsNotice, Shift, mean)
# summary(QualAvgBefore3MonthsNotice)
# table(QualAvgBeforeNoticeRange)

table(ProdAvgBeforeNoticeRange); sum(table(ProdAvgBeforeNoticeRange)); count-sum(table(ProdAvgBeforeNoticeRange))
table(QualAvgBeforeNoticeRange); sum(table(QualAvgBeforeNoticeRange)); count-sum(table(QualAvgBeforeNoticeRange))

table(ResignationType)
table(ResignationReason)
table(format(ExpectedDeparture), '%Y')
table(Termination)
table(LongLeave)
table(Client); sum(table(Client)); count-sum(table(Client))
table(SubClient, useNA = "ifany"); sum(table(SubClient)); count-sum(table(SubClient))
# table(format(as.Date(ProjectEffectiveFrom, '%m/%d/%Y')), '%Y')
# dateformat conversion
str(ProjectEffectiveFrom)
str(DateofBirth)
table(PrevProcess); count-sum(table(PrevProcess))
table(PrevClient); count-sum(table(PrevClient))
table(PrevSubClient); count-sum(table(PrevSubClient))
table(PrevEmployer)

table(CourseLevels); count-sum(table(CourseLevels))
table(Course); count-sum(table(Course))
table(Specialization); count-sum(table(Specialization))

summary(Last30DaysLeaveCount)
hist(Last30DaysLeaveCount, breaks = 30, labels = TRUE)
sum(length(which(Last30DaysLeaveCount>0)))
# table(Last30DaysLeaveCount)
# Getting percentages
histPercent <- function(x, ...) {
  H <- hist(x, plot = FALSE)
  H$density <- with(H, 100 * density* diff(breaks)[1])
  labs <- paste(round(H$density, 2), "%", sep="")
  plot(H, freq = FALSE, labels = labs, ylim=c(0, 1.08*max(H$density)),...)
}

histPercent(Last30DaysLeaveCount, col="gray")


table(TravelTimeRange)
table(EngagementIndex, useNA = "ifany")

summary(TotalExtraHoursWorked)
hist(TotalExtraHoursWorked, labels = TRUE, breaks = max(TotalExtraHoursWorked))

table(StaffingEmployeeStatus, useNA = "ifany")

table(format(DateOfResignation, '%Y-%m'))

summary(PreviousExperienceInMonths)
histPercent(PreviousExperienceInMonths, col="gray")

summary(AGSExperienceInMonths)
histPercent(AGSExperienceInMonths, col="gray")

table(Department, useNA = "ifany")
table(DepartmentGroup, useNA="ifany")
table(FunctionName, useNA="ifany")
table(VerticalName, useNA="ifany")
