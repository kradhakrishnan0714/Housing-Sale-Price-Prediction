/* Loading the Housing Data */

%web_drop_table(WORK.Housing);


FILENAME REFFILE '/home/u63840537/sasuser.v94/6371-Final Project-House Prediction/train.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.Housing;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.Housing; RUN;


%web_open_table(WORK.Housing);

Proc print data=Housing;
run;

/* Filtering for specific Neighborhoods */
data Housing_Filtered;
    set Housing;
    where Neighborhood in ('NAmes', 'Edwards', 'BrkSide');
    GrLivArea100 = GrLivArea / 100;  /* Scale to 100 sq. ft. increments */
run;
proc print data=housing_filtered;
run;

/* Create a linear regression model - with interactions and flexible slopes */
proc glm data=Housing_Filtered plots=all;
    class Neighborhood;
    model SalePrice = GrLivArea100 | Neighborhood / solution clparm;
    estimate 'NAmes 100 sq.ft. increase' GrLivArea100 1 GrLivArea100*Neighborhood 1 0 0;
    estimate 'Edwards 100 sq.ft. increase' GrLivArea100 1 GrLivArea100*Neighborhood 0 1 0;
    estimate 'BrkSide 100 sq.ft. increase' GrLivArea100 1 GrLivArea100*Neighborhood 0 0 1;
run;

/* Do the log trasnform */
data logHousing_Filtered;
    set Housing_Filtered;
    log_SalePrice = log(SalePrice);
    log_GrLivArea100 = log(GrLivArea100);

/* Create a linear regression model for log transformed data - with interactions and flexible slopes */
proc glm data=logHousing_Filtered plots=all;
class Neighborhood;
model log_SalePrice =  log_GrLivArea100 | Neighborhood /solution clparm;
estimate 'BrkSide 100 sq.ft. increase' log_GrLivArea100 1 log_GrLivArea100*Neighborhood 1 0 0;
estimate 'Edwards 100 sq.ft. increase' log_GrLivArea100 1 log_GrLivArea100*Neighborhood 0 1 0;
estimate 'NAmes 100 sq.ft. increase' log_GrLivArea100 1 log_GrLivArea100*Neighborhood 0 0 1;
run;

proc glmselect data=Housing;
model SalePrice = GrLivArea FullBath / selection= stepwise(stop=cv)cvmethod=random(5)stat=adjrsq cvdetails include=2;
run;
