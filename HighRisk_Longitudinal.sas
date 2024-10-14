/***************************************************************
TITLE: The Association of Discrimination, Race and Education Level of Household Head 
	   With High-Risk Social Ties among Adolescents Transitioning to Adulthood

DATA SOURCE: Panel Study of Income Dynamics: Transition to Adulthood Supplement (TAS) 
			 between 2011-2019

AUTHOR: CATHERINE RACHEL
****************************************************************/
/*Specify path*/
%let path = /home/u62726112/Longitudinal Analysis;

/* Set the working library */
libname proj "&path.";

/* Import the Excel file */
proc import datafile="&path.\Final.xlsx" 
    out=proj.PSID 
    dbms=xlsx 
    replace;
    getnames=yes;
run;

/* Create Unique ID */
proc sql;
    create table work.data as
    select *,
       (ER30001 * 1000 + ER30002) as PID, /*unique ID using interview ID and participant ID*/
       round(mean(ER34104),0.01) as age_mean /*mean age of the sample at the start of collection*/
    from proj.psid;
quit;

proc sort data=work.data 
		  nodupkey 
		  out=proj.data; *without duplicates;
	by PID;
run; 


/* Data Preparation: wide form to long form */
data proj.long_data;
	set proj.data;
	
	/*Pivot Longer */
	array high_risk{*} TA111020 TA131055 TA151095 TA171928 TA192089;
	array disc{*} TA111130 TA131222 TA151282 TA171977 TA192156;

	/*Outcome: Peer Drug Use i.e. High Risk Social Ties */
	do wave = 1 to dim(high_risk);
	peer_drug_use = high_risk{wave};
        if high_risk{wave} in (8, 9) then high_risk{wave} = .;* Recode 8 and 9 to missing ;
    peer_drug_use = put(high_risk{wave}, 1.);
        
    /* Time Variable: Mean Age of Sample */
    mean_age = age_mean + (2*(wave-1)); *Adjust mean age for each wave increment;
   
    /* Age of each PID at each wave increment*/
    age = ER34104 + (2*(wave-1));
	
    /* Time-Variant Predictor: Discrimination */
	discrimination = disc{wave};
        if disc{wave} in (8, 9) then disc{wave} = .; * Recode 8 and 9 to missing ;
    discrimination = put(disc{wave}, 1.);
    
    /* Time-Invariant Predictor: Race recorded at wave 1
    New Key: 
    1= Non-Hispanic White
    2= Non-Hispanic Black
    3= Non-Hispanic Asian
    4= Hispanic
    5= Non-Hispanic Other including more than one race */
    select;
    	when (TA111056 in (1:7)) race_ethnic = 4; *any documentation of hispanicity;
    	when (TA111057 = 1) race_ethnic = 1;
    	when (TA111057 = 2) race_ethnic = 2; 
    	when (TA111057 = 4) race_ethnic = 3; 
    	when (TA111057 in (3,5,7)) race_ethnic = 5;
    	otherwise race_ethnic = .;
    end;
    race_ethnic = put(race_ethnic, 1.);
    
    /* Time-Invariant Predictor: Education level of Household Head 
    New Key: convert years of education to categories
    1= <= High School or GED
    2= Some College
    3= College Degree
    4= Some Post-Graduate Studies */   	
	select;
		when (ER52405 <= 12) education_head = 1;
		when (ER52405 in (13:15)) education_head = 2;
		when (ER52405 = 16) education_head = 3; 
		when (ER52405 = 17) education_head = 4;
		otherwise education_head = .; 
	end;
	education_head = put(education_head, 1.);
	
   	output;
   		
   	keep PID wave mean_age age peer_drug_use discrimination race_ethnic education_head;
    end;
    
run;

/* Filter participants with atleast 2 non-missing entries  */
proc sql;
    create table proj.data_filtered as
    select * 
    from proj.long_data
    group by PID
    having sum(not missing(peer_drug_use)) >= 2 
           and sum(not missing(discrimination)) >= 2 
           and sum(not missing(race_ethnic)) >= 2 
           and sum(not missing(education_head)) >= 2
    order by PID, wave;
quit;

/* Descriptive Statistics for each wave of collection */
*reporting counts,frequencies and other summary statistics 
for all variables grouped by wave of data collection ;

%macro tabulate_by_wave(data=, wave_var=, out_format=HTML);
    /* Open ODS destination based on output format */
    %if &out_format = HTML %then 
        %do;
            ods html file="&path./descriptive_report.html"; 
        %end;

    /* Distinct waves */
    proc sql noprint;
        select distinct &wave_var into :wave_list separated by ' '
        from &data;
    quit;

    /* Loop through each wave */
    %let count = %sysfunc(countw(&wave_list));

    %do i = 1 %to &count;
        %let current_wave = %scan(&wave_list, &i);
        
        /* Generate frequency table for each wave and save it to a dataset */
        title "Summary Statistics for Wave &current_wave"; /* Title for the table */
       	proc means data=&data  min max median;
       		where &wave_var = &current_wave; 
       		var age;
   		run;

        proc freq data=&data; 
            where &wave_var = &current_wave; 
            tables peer_drug_use discrimination race_ethnic education_head / missing nocum;
        run;
    %end;


    ods _all_ close;
%mend tabulate_by_wave;

%tabulate_by_wave(data=proj.data_filtered, wave_var=wave, out_format=HTML);

/* Unconditional Mean Model */
proc mixed data=proj.data_filtered method=REML;
    class PID; 
    model peer_drug_use = / solution; * Intercept only model ;
    random intercept / subject=PID; * Random intercept for each participant ;
    ods output SolutionF=uc_mean_results; * output parameter estimates ;
run;

proc print data=uc_mean_results; 
run;

/* Unconditional Growth Model */
proc mixed data=proj.data_filtered method=REML;
    class PID;
    model peer_drug_use = mean_age / solution; * Including mean_age as the time variable ;
    random mean_age / subject=PID; * Random slope for mean age ;
    ods output SolutionF=uc_growth_results;
run;

proc print data=uc_growth_results;
run;

/* Conditional Growth Model with Main IV: Education Level of Household Head */
proc mixed data=proj.data_filtered method=REML;
    class PID;
    model peer_drug_use = education_head 
    					  mean_age 
    					  education_head*mean_age / solution; 
    random mean_age / subject=PID;
    ods output SolutionF=c_iv_ed;
run;

proc print data=c_iv_ed;
run;

/* Conditional Growth Model with all predictors */
proc mixed data=proj.data_filtered method=REML;
    class PID;
    model peer_drug_use = education_head 
    					  mean_age 
    					  race_ethnic 
    					  discrimination 
    					  education_head*mean_age / solution outpred=Pred; 
    random mean_age / subject=PID;
    ods output SolutionF=c_iv_all;
run;

proc print data=c_iv_all;
run;

data proj.pred;
	set work.pred;
	Pred = round(Pred, 0.01);
run;

proc sort data=proj.pred out=proj.pred;
	by mean_age;
run;


/* Visualize Individual Growth Curves in High-Risk Social Ties */
*For Education level of Household Head ;
proc sgpanel data=proj.pred;
	panelby education_head / rows= 4;
    series x=mean_age y=Pred / group=education_head markers;
    title "Predicted Change in High-Risk Social Ties given Education level of Household Head over Time ";
    colaxis label="Mean Age";
    rowaxis label="Predicted High-Risk Social Ties";
run;

proc sgplot data=proj.pred;
    series x=mean_age y=Pred / group=education_head;
    title "Predicted Change in High-Risk Social Ties given Education level of Household Head over Time ";
    xaxis label="Mean Age";
    yaxis label="Predicted High-Risk Social Ties";
run;

*For Race & Ethnicity ;
proc sgpanel data=proj.pred;
	panelby race_ethnic / columns= 5;
    series x=mean_age y=Pred / group=race_ethnic markers;
    title "Predicted Change in High-Risk Social Ties given Race over Time ";
    colaxis label="Mean Age";
    rowaxis label="Predicted High-Risk Social Ties";
run;

proc sgplot data=proj.pred;
    series x=mean_age y=Pred / group=race_ethnic;
    title "Predicted Change in High-Risk Social Ties given Race over Time ";
    xaxis label="Mean Age";
    yaxis label="Predicted High-Risk Social Ties";
run;

*For Everyday Discrimination ;
proc sgpanel data=proj.pred;
	panelby discrimination / columns= 6;
    series x=mean_age y=Pred / group=discrimination markers;
    title "Predicted Change in High-Risk Social Ties given Perceived Everyday Discrimination over Time ";
    colaxis label="Mean Age";
    rowaxis label="Predicted High-Risk Social Ties";
run;

proc sgplot data=proj.pred;
    series x=mean_age y=Pred / group= discrimination;
    title "Predicted Change in High-Risk Social Ties given Perceived Everyday Discrimination of Household Head over Time ";
    xaxis label="Mean Age";
    yaxis label="Predicted High-Risk Social Ties";
run;


