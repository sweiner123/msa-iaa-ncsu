/* Lab 2 */

proc contents data=sashelp.stocks;
run;

proc means data=sashelp.stocks std;
	var Volume Date;
run;

proc univariate data=sashelp.stocks;
	var Date;
run;

proc freq data=sashelp.stocks;
run;

proc univariate data=sashelp.stocks;
	var Close Open;
run;

/* Part 2 */

/* Download Orion.zip and create a library */

libname orion "/opt/sas/home/nhsuthar/EPG1V2/Orion";

data work.bigdonations;
	set ORION.EMPLOYEE_DONATIONS;
	Total = sum(Qtr1, Qtr2, Qtr3, Qtr4);
run;

proc univariate data=work.bigdonations;
	var Total;
run;

data work.filtered_data;
	set work.bigdonations;
	
	where Total >= 50 and Paid_By = "Credit Card";
run;

data work.delays;
	set orion.orders;
	Order_Month = MONTH(Order_Date);
run;

proc contents data=work.delays;
run;

data work.last;
	set work.delays;
	where ((Delivery_Date - Order_Date) > 4 and Employee_ID = 99999999) and Order_Month = 8;
run;


