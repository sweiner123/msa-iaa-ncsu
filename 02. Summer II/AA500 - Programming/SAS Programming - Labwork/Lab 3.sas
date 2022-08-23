/* Lab 3 */

libname orion "/opt/sas/home/nhsuthar/EPG1V2/Orion";


data work.employees;
	set orion.sales;

	FullName = cat(First_Name, Last_Name);
	Yrs2019 = YRDIF(Hire_Date,'01JAN2019'd);
	
	label Yrs2019 = 'Years of Employment as of 2019';
	format Hire_date ddmmyy10.;
run;

proc print data=work.employees(keep=FullName Hire_date Yrs2019) ;
run;

proc sort data=work.employees;
	by descending Yrs2019; 
run;

proc print data=work.employees;
run;

proc contents data=work.employees;
run;

/* 
Question 1. What is the value of Yrs2019 for observation 10? - 41
Question 2. What is the file size (in bytes) for the data set work.employees? - 131072
*/


proc format;
	value $GENDER
		'M' = 'Male'
		'F' = 'Female'
		other = "Invalid Code";
run;

proc print data=orion.nonsales;
	
run;

proc format;
	value SALRANGE
		20000 - < 100000 = "Below $100,000"
		100000 - 500000 = "$100,000 or more"
		. = "Missing Salary"
		other = "Invalid Salary";
run;

proc freq data=orion.nonsales;
	table Salary*Gender;
run;

data orion.nonsales;
	set orion.nonsales;
	format Gender $GENDER.;
	format Salary SALRANGE.;
run;

/*

Question 3. How many Female employees earn salaries below $100,000? - 108
Question 4. How many Male employees earn salaries above $100,000? - 6

*/

data orion.nonsales;
	set orion.nonsales;

run;



