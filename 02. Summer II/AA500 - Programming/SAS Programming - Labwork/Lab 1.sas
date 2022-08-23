
/* Lab 1 */

/* Question 1 */

proc contents data=sashelp.junkmail;
run;


/* Question 2 */

FILENAME REFFILE DISK '/opt/sas/home/nhsuthar/EPG1V2/data/np_traffic.csv';


proc import datafile=REFFILE
	dbms=csv 
	out=work.np_traffic replace;
run;

proc contents data=work.np_traffic;
run;


/* proc */