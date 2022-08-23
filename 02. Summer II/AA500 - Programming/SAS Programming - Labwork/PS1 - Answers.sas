/* Problem Session 1  */


/**************************** PART 1 **************************/
/* Scenario 1 */

libname PS_data "/opt/sas/home/nhsuthar/EPG1V2/PS_data";

data work.stress1;
	set PS_DATA.stress;
	where RestHR < 70;
run;

data work.stress1;
	set work.stress1;
	TotalTime = (TimeMin*60) + TimeSec;
run;

data work.stress1;
	set work.stress1;
	where TotalTime >= 600;
run;

proc sort data=work.stress1 out=work.sorted;
	by descending TotalTime;
run;

proc print data=work.sorted;run;

/* Test Your Code */
/* 1.- How many observations are in Work.Sorted? - 4 */

/* 2. What is the value of TotalTime for observation 3 in Work.Sorted?- 902 */


/* Scenario 2*/

data work.StaffReports;
	set PS_data.Staff;
	where WageCategory not eq 'H';
	format DOB mmddyy10.;
run;

data work.StaffReports;
	set work.StaffReports;
	Raise = WageRate * 0.03;
run;

proc print data=work.staffreports; run;

proc means data=work.StaffReports;
	var Raise;
run;

proc print data=work.stress1;run;

proc sort data=work.stress1 out=Work.StaffId;
	by ID;
run;

proc print data=Work.StaffId;run;
proc print data=Work.StaffReports;run;


/* 1. In the Work.StaffReports data set, for observation 5, what is the value of DOB? - 07/17/1951 */

/* 2. In the Work.StaffReports data set, for observation 15, what is the value of Raise? Round your */
/* answer to 2 decimal places.}  - 177.323 */

/* 3. What is the mean of the variable Raise? - 101.2026000 */


/**************************** PART 2 **************************/

/* Scenario 1 */

data work.aprilbills;
	drop Total EquipCost;
	set PS_data.aprbills;

	if(Days > 7) then 
		Discount=(RoomCharge) * 0.20;
	else Discount=0;

	TotalDue = Total - Discount;

	format DateIn DateOut DATE9.;
	format RoomRate RoomCharge Discount TotalDue DOLLAR10.2;
run;

proc print data=work.aprilbills;run;

/* 1. What is the value of the variable TotalDue in observation 4? - $437.41 */
/* 2. What is the value of the variable Discount in observation 5? - $280.00 */

/* Scenario 2 */

data Work.Scenario2;
	set PS_Data.Temp18;
	format Day DATE9.;

	Month = MONTH(Day);

run;

proc freq data=WORK.SCENARIO2;
	tables HighTemp / plots=(freqplot cumfreqplot);
run;

proc means data=work.scenario2 std;
	var AvgHighTemp AvgHighTemp;
run;

/* Test Your Code */
/* 1. What is the frequency for a HighTemp of 63? - 2 */
/* 2. What is the HighTemp on January 12, 2018? - 64 */
/* 3. What is the mean for AvgLowTemp for Month=1? (Round your answer to the nearest integer.) */ - 28.6129032
/* 4. What is the standard deviation (std) for AvgHighTemp for Month=3? (Round your answer to */
/* two decimal places.) */ - 3.1738176

proc means data=work.scenario2 mean std;
	var AvgHighTemp;
	where Month=3;
run;
