/* Problem Session 2  */

/**************************** Scenario 1 **************************/

libname PS_data "/opt/sas/home/nhsuthar/EPG1V2/PS_data";

data work.scenario1;
	set PS_DATA.ADDRESSES;

	ZipCode = COMPRESS(State,,'A');
	State = COMPRESS(State,,"d");

	Just_Number = COMPRESS(Street,,'A');

	if find(Street, "DRIVE","i") ge 1
		then Indicator=1;
	else 
		Indicator = 0;
run;

proc freq data=work.scenario1 order=freq;
	table State;
run;

proc freq data=work.scenario1;
	table Indicator;
run;

proc freq data=work.scenario1;
	table ZipCode;
run;

/*
1. How many Street names contain the word "Drive"? - 12
2. What is the frequency for the state of NY? - 3
3. Which observation contains ZipCode 85069? Give the street name. - 1861 Clarksburg Road
4. How many states have the frequency number of 4? - 2 */

/**************************** Scenario 2 **************************/

data work.mycars;
	set sashelp.cars;
	AvgMPG=mean(mpg_city, mpg_highway);
run;

title 'Cars With Average MPG Over 40';
proc print data=work.mycars;
	var make model type avgmpg;
	where AvgMPG>40;
run;

title 'Average MPG by Car Type';
proc means data=work.mycars mean min max maxdec=1;
	var avgmpg;
	class type;
run;
title;

proc freq data=work.mycars;
	table type;
run;

/*
1. What is the number of observations where the variable Type is Sedan? - 262
2. How many observations are printed to the report titled, "Cars With Average MPG Over 40"? - 4
*/

