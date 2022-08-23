/* SAS Programming Assessment - Nikhil Suthar */

/* Create the library */

libname Input "/opt/sas/home/nhsuthar/EPG1V2/input";

data work.scenario1;
	set INPUT.SCEN01;
	bloodtype = upcase(bloodtype);
run;

data work.scenario1;
	set work.scenario1;
	where bloodtype not eq 'O+';
run;

data work.scenario1;
	set work.scenario1;
	MI = scan(ID, 2, '-');
run;

data work.test;
	set work.scenario1;
	where bloodtype = 'A+' and MI = 'J';
run;

/* 111 */

proc means data=work.scenario1(where=(bloodtype='B+')) maxdec=1;
	var Centimeters;
run;

/* 171.5 */

/* Scenario 2 */

proc sort data=INPUT.SCEN02A;
	by ID;
run;

proc sort data=INPUT.SCEN02B;
	by ID;
run;

data work.match02;
    merge INPUT.SCEN02A(IN=apples) INPUT.SCEN02B(IN=Bananas);
    by ID;
	if apples = 1 & Bananas = 1;
run;

/* 1945 */

data work.nonmatch02;
    merge INPUT.SCEN02A(IN=apples) INPUT.SCEN02B(IN=Bananas);
    by ID;
	if (apples = 1 & Bananas = 0) OR (apples = 0 & Bananas = 1);
run;

/* 43 */

/* data work.alltest; */
/*     merge INPUT.SCEN02A(IN=apples) INPUT.SCEN02B(IN=Bananas); */
/*     by ID; */
/* 	if apples = 1 OR Bananas = 1; */
/* run; */

/* 1988 - Match + NonMatch */

/* Scenario 3 */

data work.scenario3;
	set INPUT.SCEN03;
	MIN = min(X1, X2, X3, X4, X5);
	TOTAL = sum(MIN, Y);
run;

proc means data=work.scenario3 maxdec=1;
	var TOTAL;
run;

/* 3.6 */

/* Scenario 4 */

data work.scenario4;
	set INPUT.SCEN04;

	GPA = strip(scan(GPA_LETTER, 1, 'and'));

	MAJOR_MINOR_NEW =tranwrd(MAJOR_MINOR,' and ','_');

	MAJOR = strip(scan(MAJOR_MINOR_NEW, 1, '_'));
	MINOR = strip(scan(MAJOR_MINOR_NEW, 2, '_'));

	keep ID GPA MAJOR_MINOR MAJOR MINOR;
run;

proc contents data=work.scenario4;
run;

proc print data=work.scenario4;
run;

/* 851968 */














