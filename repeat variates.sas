OPTIONS COMPRESS=YES;

/***************** INACTIVE DATA PREP ****************************************/
DATA InactiveEmployees2(rename=(mappedlocation=worklocation));
SET SASEDWFW.AttAnalysis_tblInActiveEmployees(drop=WorkLocation);
where substr(EmployeeCode,1,1) ne 'C';
run;

data InactiveEmployees;
	DROP DateofBirth3 Dateofresignation3 DateofJoin3	DateOfRelieving3	ShiftEffectiveFrom3	FacilityEffectiveFrom3	RptEffectiveFrom3	LastReviewDate3	LastButOneReviewDate3	ProjectEffectiveFrom3;
	set InactiveEmployees2 (RENAME=(Dateofresignation = Dateofresignation3 DateofJoin = DateofJoin3 DateofBirth=DateofBirth3 DateOfRelieving = DateOfRelieving3 ShiftEffectiveFrom=ShiftEffectiveFrom3 FacilityEffectiveFrom=FacilityEffectiveFrom3 RptEffectiveFrom=RptEffectiveFrom3 LastReviewDate=LastReviewDate3 LastButOneReviewDate=LastButOneReviewDate3 ProjectEffectiveFrom=ProjectEffectiveFrom3));
		if DateofJoin3 ne '' then DateofJoin = input(DateofJoin3, yymmdd10.);
		if DateofBirth3 ne '' then DateofBirth = input(DateofBirth3, yymmdd10.);
		if DateOfRelieving3 ne '' then DateOfRelieving = input(DateOfRelieving3, yymmdd10.);
		if ShiftEffectiveFrom3 ne '' then ShiftEffectiveFrom=input(ShiftEffectiveFrom3, mmddyy10.);
		if FacilityEffectiveFrom3 ne '' then FacilityEffectiveFrom = input(FacilityEffectiveFrom3, yymmdd10.);
		if RptEffectiveFrom3 ne '' then RptEffectiveFrom = input(RptEffectiveFrom3,  yymmdd10.);
		if LastReviewDate3 ne '' then LastReviewDate = input(LastReviewDate3,yymmdd10.);
		if LastButOneReviewDate3 ne '' then LastButOneReviewDate=input(LastButOneReviewDate3,yymmdd10.);
		if ProjectEffectiveFrom3 ne '' then ProjectEffectiveFrom = input(ProjectEffectiveFrom3,mmddyy10.);
		if Dateofresignation3 ne '' then Dateofresignation = input(Dateofresignation3,yymmdd10.);
	format DateofJoin Dateofresignation DateofBirth DateOfRelieving ShiftEffectiveFrom FacilityEffectiveFrom RptEffectiveFrom LastReviewDate LastButOneReviewDate ProjectEffectiveFrom mmddyy10.;
run;

data inactive;
set inactiveemployees;
join_month=Month(DateofJoin);
join_year=Year(DateofJoin);
rel_month=Month(DateOfRelieving);
rel_year=Year(DateOfRelieving);
ind='I';
rel=compress(rel_month||rel_year);
join=compress(join_month||join_year);
Year=join_year;
Month=join_month;
res_month=Month(Dateofresignation);
res_year=Year(Dateofresignation);
run;


/***************** ACTIVE DATA PREP ****************************************/
data activeEmployees2(rename=(mappedlocation=WorkLocation));
set SASEDWFW.AttAnalysis_tblActiveEmployees(drop=WorkLocation);
where substr(EmployeeCode,1,1) ne 'C';
run;

data activeEmployees;
DROP DateofBirth3 Dateofresignation3	DateofJoin3	DateOfRelieving3	ShiftEffectiveFrom3	FacilityEffectiveFrom3	RptEffectiveFrom3	LastReviewDate3	LastButOneReviewDate3	ProjectEffectiveFrom3;
	set activeEmployees2 (RENAME=(Dateofresignation=Dateofresignation3 DateofJoin = DateofJoin3 DateofBirth=DateofBirth3 DateOfRelieving = DateOfRelieving3 ShiftEffectiveFrom=ShiftEffectiveFrom3 FacilityEffectiveFrom=FacilityEffectiveFrom3 RptEffectiveFrom=RptEffectiveFrom3 LastReviewDate=LastReviewDate3 LastButOneReviewDate=LastButOneReviewDate3 ProjectEffectiveFrom=ProjectEffectiveFrom3));
		if DateofJoin3 ne '' then DateofJoin = input(DateofJoin3, yymmdd10.);
		if DateofBirth3 ne '' then DateofBirth = input(DateofBirth3, yymmdd10.);
		if DateOfRelieving3 ne '' then DateOfRelieving = input(DateOfRelieving3, yymmdd10.);
		if ShiftEffectiveFrom3 ne '' then ShiftEffectiveFrom=input(ShiftEffectiveFrom3, mmddyy10.);
		if FacilityEffectiveFrom3 ne '' then FacilityEffectiveFrom = input(FacilityEffectiveFrom3, yymmdd10.);
		if RptEffectiveFrom3 ne '' then RptEffectiveFrom = input(RptEffectiveFrom3,  yymmdd10.);
		if LastReviewDate3 ne '' then LastReviewDate = input(LastReviewDate3,yymmdd10.);
		if LastButOneReviewDate3 ne '' then LastButOneReviewDate=input(LastButOneReviewDate3,yymmdd10.);
		if ProjectEffectiveFrom3 ne '' then ProjectEffectiveFrom = input(ProjectEffectiveFrom3,mmddyy10.);
		if Dateofresignation3 ne '' then Dateofresignation = input(Dateofresignation3,yymmdd10.);
format DateofJoin Dateofresignation DateofBirth DateOfRelieving ShiftEffectiveFrom FacilityEffectiveFrom RptEffectiveFrom LastReviewDate LastButOneReviewDate ProjectEffectiveFrom mmddyy10.;
run;

data active;
set activeemployees;
join_month=Month(DateofJoin);
join_year=Year(DateofJoin);
rel_month=Month(DateOfRelieving);
rel_year=Year(DateOfRelieving);
ind='A';
rel=compress(rel_month||rel_year);
join=compress(join_month||join_year);
Year=join_year;
Month=join_month;
res_month=Month(Dateofresignation);
res_year=Year(Dateofresignation);
if ProjectEffectiveFrom=. then ProjectEffectiveFrom=dateofjoin;
if substr(EmployeeCode,1,1) in('U','B') and DATEOFJOIN>TODAY() then delete;
run;

/***************** ATTRITION_R5 DATA PREPARATION ****************************************/
options mprint mlogic;

%macro head_count_details();

data hc_details_final;
set active(obs=0);
run;

%local start_yr end_yr Month;
proc  sql;
select min(join_year) into :start_yr from active;
/*select min(join_year),max(join_year) into :start_yr,:end_yr from active;*/
/*select max(Month(dateofjoin)) into :Month from active where join_year in (select max(join_year) from active);*/
quit;

%let Month= %sysfunc(month("&sysdate"d));
%let end_yr= %sysfunc(year("&sysdate"d));

%put &start_yr &end_yr &Month;

%do i=&start_yr %to &end_yr;
     %do y=1 %to 12;
           %if (&i<&end_yr or (&i=&end_yr and &y<=&Month)) %then %do;

                /*head count details*/
                proc sql noprint;
                create table hc_details1 as
                select * 
                from active
                where (join_year<&i or (join_year=&i and join_month<=&y));
                quit;

                proc sql noprint;
                create table hc_details2 as
                select *
                from inactive 
                where (join_year<&i or (join_year=&i and join_month<=&y)) 
                and (rel_year>&i or (rel_year=&i and rel_month>=&y));
                quit;

                data hc_details;
                set hc_details1 hc_details2;
                Year=&i;
                Month=&y;
                run;

                proc append base=hc_details_final data=hc_details force;
                run;

           %end;
           %else %do;
           %end;
     %end;
%end;

/*Attrition details*/
data hc_details_final;
retain Month Year MonthNo ;
set hc_details_final(RENAME=(Month=MonthNo));
Month = MDY(MonthNo, 1, Year);
FORMAT Month MONYY7.;
run;

data all;
set hc_details_final;
if year = rel_year and monthno = rel_month then do;end;
else do;
	ReasonofLeaving=''; 
	ExitType=''; 
	DateOfRelieving='';
end;
run;

%mend head_count_details;
%head_count_details();
