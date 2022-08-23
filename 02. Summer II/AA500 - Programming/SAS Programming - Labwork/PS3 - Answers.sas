/* Problem Session 3  */

/**************************** Scenario 1 **************************/

libname PS_data "/opt/sas/home/nhsuthar/EPG1V2/PS_data";

proc sort data=PS_DATA.Patients;
	by ID;
run;

proc sort data=PS_DATA.Measure;
	by ID;
run;

data work.merged;
    merge PS_DATA.Patients PS_DATA.Measure;
    by ID;
run;

proc print data=work.merged;
run;

data work.merged;
	set work.merged;
	where Age < 50;
run;

proc sort data=work.merged out=work.sortpatients;
	by Age;
run;

/*
1. What the value of the Age variable for observation 6 in Work.Sortpatients? - 48
2. What is the value of the Weight variable for observation 3 in Work.Sortpatients? - 157
*/

/**************************** Scenario 2 **************************/

data Work.FlightEmpData;                    
   set PS_DATA.Empdata PS_DATA.EMPDATU PS_DATA.EMPDATU2;        /* */   
run;        
         
%let Location = USA;

data work.FlightEmpData;
	set work.FlightEmpData;
	where Country = "&Location" and Salary >= 30000;
run;

proc sort data=work.FlightEmpData;
	by descending Salary;
run;

/*
1. What is the value of Salary in observation 4? - 33000
*/
                  



