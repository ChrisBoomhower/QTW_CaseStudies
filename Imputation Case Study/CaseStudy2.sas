/*
 * Case Study 2 - Multiple Imputation
 * Cory Adams, Chris Boomhower, Alexandra Fisher, Alex Frye
 * MSDS 7333, September 6, 2017
 */

* Import csv data file ;
data CARS;
*infile '/folders/myshortcuts/SAS/carmpgdata_2.csv' dlm=',' DSD firstobs=2;
infile '/home/cboomhower0/sasuser.v94/MSDS7333/carmpgdata_2.csv' dlm=',' DSD firstobs=2;
* SAS does not properly recognize empty values for delimited data unless you use the dsd option. 
Need to use the dsd option on the infile statement if two consecutive delimiters are used to indicate 
missing values (e.g., two consecutive commas, two consecutive tabs). ;
input Auto $ MPG CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE ;
run; 
* Print imported carmpg data ;
proc print data=CARS; 
run;

* Render scatterplot matrix and correlation matrix;
* aka Descriptive stats (EDA) for non-imputed data ;
title "Descriptive Stats /EDA for Non-imputed Data";
proc corr data=CARS;
* Important Note: PROC CORR will not perform listwise unless NOMISS specified ;
var MPG CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE;
run;

* Scatterplot matrix ;
proc sgscatter data=CARS;
matrix MPG CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE / diagonal=(histogram normal);
run;

* Analyze missing patterns (monotone or non-monotone?) ;
title "Missing Data Pattern";
ods select misspattern;
proc mi data=CARS nimpute=0;
var MPG CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE;
run;

* Visualize missing data locations;
*proc iml;
	*varNames = {mpg cylinders size hp weight accel eng_type};
	*use CARS;                         /* open data set */
	*read all var varNames into X;              /* create numeric data matrix, X */
	*close CARS;

	*Count = countmiss(X,"row");            /* count missing values for each obs */
	*missRows = loc(Count > 0);                  /* which rows are missing? */

	*ods graphics / width=400px height=600px;
	*Y = X[missRows,];              /* extract missing rows   */
	*call HeatmapDisc( cmiss(Y) )   /* CMISS returns 0/1 matrix */
	     displayoutlines=0 
	     colorramp={white black}
	     xvalues=VarNames          /* variable names along bottom */
	     yvalues=missRows          /* use nonmissing rows numbers as labels */
	     showlegend=0 title="Missing Value Locations";

* Descriptive stats for listwise complete data ;
title "EDA on Listwise Complete Data";
proc corr data=CARS nomiss;
var MPG CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE;
run;

* Create imputed data files (specify nimpute=5) ;
title "Multiple Imputation (MI) Data";
ods graphics on;
proc mi data=CARS nimpute=5
out=miout seed=123;
var MPG CYLINDERS SIZE HP WEIGHT ACCEL ENG_TYPE;
run;
* Print Multiple Imputation (MI) data set output ;
title "MI out data set";
proc print data=miout; run;

* Analyze Multiple Imputation (MI) Data ;
title "Linear Regression on Multiple Imputation (MI) Data";
proc reg data=miout outest=outreg covout;
model mpg = CYLINDERS SIZE HP WEIGHT;
by _Imputation_;
run;
* Print Multiple Imputation (MI) data set output ;
title "Regression Output Data";
proc print data=outreg; run;

* Multiple Imputation Results Analysis ;
title "Multiple Imputation (MI) Results Analysis";
proc mianalyze data=outreg;
modeleffects CYLINDERS SIZE HP WEIGHT Intercept;
run;

* Analyze complete listwise data ;
title "Predicting MPG on Non Imputed Data (data with missing values) - Listwise Deletion";
proc reg data=CARS;
 model mpg = CYLINDERS SIZE HP WEIGHT;
run;