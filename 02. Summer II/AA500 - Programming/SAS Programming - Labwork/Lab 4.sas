/* Lab 4 */

libname orion "/opt/sas/home/nhsuthar/EPG1V2/Orion";

data work.allemployees;
	set orion.sales orion.nonsales(rename=(First=First_Name Last=Last_Name));
	keep Employee_ID First_Name Last_Name Job_Title Salary;
run;

proc sort data=work.allemployees;
	by descending Employee_ID;
run;

proc contents data=work.allemployees;
run;

proc print data=work.allemployees(obs=5);
run;

/*
Question 1. What is the file size (in bytes) for the data set work.allemployees? - 131072
Question 2. What is the value of Salary in the first observation of the sorted dataset? - 52930
*/

proc sort data=ORION.PRODUCT_LEVEL;
	by Product_Level;
run;

proc sort data=ORION.PRODUCT_LIST;
	by Product_Level;
run;

data work.listlevel;
    merge ORION.PRODUCT_LEVEL ORION.PRODUCT_LIST;
    by Product_Level;
	where Product_Level = 3;
	keep Product_ID Product_Name Product_Level Product_Name;
run;

/* Question 3. How many observations does work.listlevel have? - 13 */

proc sort data=ORION.CUSTOMER;
	by Country;
run;

proc sort data=ORION.LOOKUP_COUNTRY(rename=(Start=Country));
	by Country;
run;

data work.allcustomer;
    merge ORION.CUSTOMER(IN=apples) ORION.LOOKUP_COUNTRY(IN=Bananas);
    by Country;
	if apples = 1 & Bananas = 1;
	keep Customer_ID Country Customer_Name Label;
run;

/* Question 4. How many observations does work.allcustomer have? - 77 */
