/* Load the preprocessed Housing data */

%web_drop_table(WORK.housing_data_processed);


FILENAME REFFILE '/home/u63840537/sasuser.v94/6371-Final Project-House Prediction/model_data_reduced.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.housing_data_processed;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.housing_data_processed; RUN;


%web_open_table(WORK.housing_data_processed);

proc contents data=housing_data_processed out=varlist(keep=name) noprint;
run;

proc sql noprint;
    select name into :varlist separated by ' '
    from varlist
    where name ne 'SalePrice';
quit;
/* Select the variables for model using Forward Selection */
proc glmselect data=housing_data_processed seed=47856852 plots=all;
    partition fraction(test = .2);
    model SalePrice = &varlist. / selection=Forward(stop=CV) cvmethod=random(5) stats=adjrsq cvdetails ;
run;
/* Select the variables for model using Backward Selection */
proc glmselect data=housing_data_processed seed=47856852 plots=all;
    partition fraction(test = .2);
    model SalePrice = &varlist. / selection=Backward(stop=CV) cvmethod=random(5) stats=adjrsq cvdetails ;
run;
/* Select the variables for model using Stepwise Selection */
proc glmselect data=housing_data_processed seed=47856852 plots=all;
    partition fraction(test = .2);
    model SalePrice = &varlist. / selection=stepwise(stop=CV) cvmethod=random(5) stats=adjrsq cvdetails ;
run;
/* Verify the assumptions using plots provided by proc glm */
proc glm data=housing_data_processed plots=all;
model SalePrice =  OverallQual GrLivArea BsmtFinSF1 GarageCars MSSubClass YearBuilt LotArea GarageYrBlt
OverallCond BedroomAbvGr BsmtFullBath Fireplaces /solution clparm;
run;
/* Do the log trasnform */
data loghousing;
set housing_data_processed;
log_SalePrice = log(SalePrice);
log_OverallQual = log(OverallQual);
log_GrLivArea = log(GrLivArea);
log_BsmtFinSF1 = log(BsmtFinSF1);
log_GarageCars = log(GarageCars);
log_MSSubClass = log(MSSubClass);
log_YearBuilt = log(YearBuilt);
log_LotArea = log(LotArea);
log_GarageYrBlt = log(GarageYrBlt);
log_OverallCond = log(OverallCond);
log_BedroomAbvGr = log(BedroomAbvGr);
log_BsmtFullBath = log(BsmtFullBath);
log_Fireplaces = log(Fireplaces);
log_FullBath = log(FullBath);
/* Verify the assumptions for log transformed data using plots provided by proc glm */
proc glm data=loghousing plots=all;
model log_SalePrice =  log_OverallQual log_GrLivArea log_BsmtFinSF1 log_GarageCars log_MSSubClass log_YearBuilt log_LotArea 
log_GarageYrBlt log_OverallCond log_BedroomAbvGr log_BsmtFullBath log_Fireplaces /solution clparm;
run;
/* Capture metrics such as CV Press, AIC using glmselect for this model */
proc glmselect data=loghousing plots=all;
model log_SalePrice =  log_OverallQual log_GrLivArea log_BsmtFinSF1 log_GarageCars log_MSSubClass log_YearBuilt log_LotArea 
log_GarageYrBlt log_OverallCond log_BedroomAbvGr log_BsmtFullBath log_Fireplaces /selection= stepwise(stop=cv)
cvmethod=random(5)stat=adjrsq cvdetails include=12;
run;
/* Import the test data */

%web_drop_table(WORK.testdata);

FILENAME REFFILE '/home/u63840537/sasuser.v94/6371-Final Project-House Prediction/test.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.testdata;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=testdata; RUN;


%web_open_table(WORK.testdata);
/* Impute missing values by mean */
proc stdize data=testdata out=test_data_imputed method=mean reponly;
    var BsmtFinSF1 BsmtFullBath GarageYrBlt GarageCars;
run;

/* Log transform the necessary variables in the test data */
data test_data_log;
    set test_data_imputed;
    log_OverallQual = log(OverallQual);
    log_GrLivArea = log(GrLivArea);
    log_BsmtFinSF1 = log(BsmtFinSF1 + 1); /* Adding 1 to handle zero values */
    log_GarageCars = log(GarageCars + 1);
    log_MSSubClass = log(MSSubClass);
    log_YearBuilt = log(YearBuilt);
    log_LotArea = log(LotArea);
    log_GarageYrBlt = log(GarageYrBlt);
    log_OverallCond = log(OverallCond);
    log_BedroomAbvGr = log(BedroomAbvGr+1);
    log_BsmtFullBath = log(BsmtFullBath + 1);
    log_Fireplaces = log(Fireplaces + 1);
    log_FullBath = log(FullBath+1);
run;


/* Create a score dataset with the coefficients from Model1 */
data model_coefficients;
    _TYPE_ = 'PARMS';
    _MODEL_ = 'MODEL1';
    _DEPVAR_ = 'log_SalePrice';
    Intercept = -37.944;
    log_OverallQual = 0.575;
    log_GrLivArea = 0.462;
    log_BsmtFinSF1 = 0.025;
    log_GarageCars = 0.217;
    log_MSSubClass = -0.041;
    log_YearBuilt = 8.427;
    log_LotArea = 0.078;
    log_GarageYrBlt = -2.560;
    log_OverallCond = 0.260;
    log_BedroomAbvGr = -0.136;
    log_BsmtFullBath = -0.229;
    log_Fireplaces = 0.027;
    output;
run;

/* Apply the model coefficients to predict log_SalePrice based on Model 1 */
proc score data=test_data_log score=model_coefficients out=predictions type=parms;
    var log_OverallQual log_GrLivArea log_BsmtFinSF1 log_GarageCars 
        log_MSSubClass log_YearBuilt log_LotArea log_GarageYrBlt 
        log_OverallCond log_BedroomAbvGr log_BsmtFullBath log_Fireplaces;
run;

/* Convert the predicted log_SalePrice back to SalePrice */
data final_predictions;
    set predictions;
    SalePrice = exp(MODEL1);
    keep Id SalePrice;
run;
/* Verify if there is any row has missing predictions */
data missing_predictions;
    set final_predictions;
    if missing(SalePrice) then output;
run;

proc print data=missing_predictions;
run;
proc sql;
    create table problem_rows as
    select a.*, b.*
    from test_data_log a
    inner join missing_predictions b
    on a.Id = b.Id;
quit;

proc print data=problem_rows;
run;
proc means data=testdata nmiss;
run;
proc means data=test_data_imputed nmiss;
run;

/* Export the predictions from Model 1 to a CSV file in a specific directory */
proc export data=final_predictions
    outfile="/home/u63840537/sasuser.v94/6371-Final Project-House Prediction/predicted_sale_prices_model1.csv"
    dbms=csv
    replace;
run;
/* Model 2 log_SalePrice =  log_GrLivArea~log_FullBath*/
proc glm data=loghousing plots=all;
model log_SalePrice =  log_GrLivArea log_FullBath  /solution clparm;
run;
/* Calculate Metrics for Model 2 */
proc glmselect data=loghousing plots=all;
model log_SalePrice =  log_GrLivArea log_FullBath  /selection= stepwise(stop=cv)
cvmethod=random(5)stat=adjrsq cvdetails include=2;
run;
/* Create a score dataset with the coefficients from Model2 */
data model_coefficients2;
    _TYPE_ = 'PARMS';
    _MODEL_ = 'MODEL2';
    _DEPVAR_ = 'log_SalePrice';
    Intercept = 6.874;
    log_GrLivArea = 0.695;
    log_FullBath = 0.246;
    output;
run;

/* Apply the model coefficients to predict log_SalePrice based on Model 2 */
proc score data=test_data_log score=model_coefficients2 out=predictions2 type=parms;
    var log_GrLivArea log_FullBath ;
run;

/* Convert the predicted log_SalePrice back to SalePrice */
data final_predictions2;
    set predictions2;
    SalePrice = exp(MODEL2);
    keep Id SalePrice;
run;
data missing_predictions;
    set final_predictions2;
    if missing(SalePrice) then output;
run;
/* Export the predictions from Model 2 to a CSV file in a specific directory */
proc export data=final_predictions2
    outfile="/home/u63840537/sasuser.v94/6371-Final Project-House Prediction/predicted_sale_prices_model2.csv"
    dbms=csv
    replace;
run;
/* Model 3 SLR*/
proc glm data=loghousing plots=all;
model log_SalePrice =  log_GrLivArea  /solution clparm;
run;
/* Calculate Metrics for Model 3 */
proc glmselect data=loghousing plots=all;
model log_SalePrice =  log_GrLivArea  /selection= stepwise(stop=cv)
cvmethod=random(5)stat=adjrsq cvdetails include=2;
run;
/* Create a score dataset with the coefficients from Model 3 */
data model_coefficients3;
    _TYPE_ = 'PARMS';
    _MODEL_ = 'MODEL3';
    _DEPVAR_ = 'log_SalePrice';
    Intercept = 5.668;
    log_GrLivArea = 0.875;
    output;
run;

/* Apply the model coefficients to predict log_SalePrice */
proc score data=test_data_log score=model_coefficients3 out=predictions3 type=parms;
    var log_GrLivArea ;
run;

/* Convert the predicted log_SalePrice back to SalePrice */
data final_predictions3;
    set predictions3;
    SalePrice = exp(MODEL3);
    keep Id SalePrice;
run;
data missing_predictions;
    set final_predictions3;
    if missing(SalePrice) then output;
run;
/* Export the predictions from Model 3 to a CSV file in a specific directory */
proc export data=final_predictions3
    outfile="/home/u63840537/sasuser.v94/6371-Final Project-House Prediction/predicted_sale_prices_model3.csv"
    dbms=csv
    replace;
run;

