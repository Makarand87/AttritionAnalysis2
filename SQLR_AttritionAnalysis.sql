/*
drop table EDW_QRY.[dbo].[AttAnalysis] 

select * into EDW_QRY.[dbo].[AttAnalysis] from EDW_BI.[dbo].[AttAnalysis_tblInActiveEmployees] 
insert into EDW_QRY.[dbo].[AttAnalysis] select * from EDW_BI.[dbo].[AttAnalysis_tblActiveEmployees]

select count(*) from [AttAnalysis] 


select * into ARTeamMember from AttAnalysis
where VerticalName = 'Accounts Receivable' and JobRole = 'Team Member' and (year(DateOfRelieving) = 2016 or year(DateOfRelieving) = 2015 or year(DateOfRelieving) = 1900)
*/
execute sp_execute_external_script
		@language = N'R'
,		@script = N'OutputDataSet <- InputDataSet;',
@input_data_1 = N'SELECT * from EDW_QRY.dbo.AttAnalysis;'
 ,@input_data_1_name =N'X'	
 @output_data_1 = 
 ,@output_Data_1_name=N'OutputDataSet'
WITH RESULT SETS undefined;
