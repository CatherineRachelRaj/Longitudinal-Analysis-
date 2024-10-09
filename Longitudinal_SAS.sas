/* Set the working library */
libname proj "C:\Users\liangqiran\Desktop\longitudinal\finalproj";

/* Import the Excel file */
proc import datafile="C:\Users\liangqiran\Desktop\longitudinal\finalproj\Final.xlsx" 
    out=final 
    dbms=xlsx 
    replace;
    getnames=yes;
run;

/* Create a unique ID */
data final;
    set final;
    PID = ER30001 * 1000 + ER30002;
run;

/* Check for duplicates */
proc sort data=final nodupkey;
    by PID;
run;

/* Transform data into long format for peer_drug_use and create age variables */
data l_final;
    set final;
    array risk_vars[5] TA111126 TA131218 TA151278 TA171976 TA192157;
    array peer_vars[5] TA111020 TA131055 TA151095 TA171928 TA192089;
    array age_vars[5] ER34104 ER34204 ER34305 ER34504 ER34704;
    
    baseage = mean(of age_vars[*]);
    
    do i = 1 to 5;
        risk_behav = risk_vars[i];
        peer_drug_use = peer_vars[i];
        age = baseage + (i - 1) * 2;
        time_age = i;
        
        if peer_drug_use in (8, 9) then peer_drug_use = .;
        if age_vars[i] = 0 then age_vars[i] = .;
        
        output;
    end;
    
    keep PID risk_behav peer_drug_use age time_age baseage;
run;

/* Recode Discrimination Variable */
data l_final;
    set l_final;
    array discrim_vars[5] TA111130 TA131222 TA151282 TA171977 TA192156;
    
    do i = 1 to 5;
        discrimination = discrim_vars[i];
        if discrimination = 9 then discrimination = .;
        output;
    end;
run;

/* Create the race_ethnic variable */
data l_final;
    set l_final;
    array hisp_vars[5] TA111056 TA131091 TA151131 TA171960 TA192136;
    array race_vars[5] TA111057 TA131092 TA151132 TA171955 TA192131;
    
    do i = 1 to 5;
        hisp = ifn(hisp_vars[i] = 0, 0, ifn(1 <= hisp_vars[i] <= 7, 1, .));
        race_ethnic = ifn(race_vars[i] = 1 and hisp = 0, 1,
                         ifn(race_vars[i] = 2 and hisp = 0, 2,
                         ifn(race_vars[i] = 4 and hisp = 0, 3,
                         ifn(hisp = 1, 0, 4))));
        output;
    end;
    
    keep PID age race_ethnic;
run;

/* Education of Head of Household */
data l_final;
    set l_final;
    education_hd = ER52405;
    education_head = ifn(education_hd <= 12, 0,
                         ifn(13 <= education_hd <= 15, 1,
                         ifn(education_hd = 16, 2, 
                         ifn(education_hd = 17, 3, .))));
    output;
run;

/* Sex of individual */
data l_final;
    set l_final;
    sex = ER32000;
run;

/* Filter records with complete data */
proc sql;
    create table l_filtered as
    select *
    from l_final
    where (calculated sum(peer_drug_use is not null) >= 2) 
      and (calculated sum(discrimination is not null) >= 2)
      and (calculated sum(race_ethnic is not null) >= 2)
      and (calculated sum(education_head is not null) >= 2);
quit;

/* Descriptive Statistics */
proc means data=l_filtered mean min max;
    var Age;
    class time_age;
run;

/* Save Descriptive Table */
proc export data=l_filtered
    outfile="C:\Users\liangqiran\Desktop\longitudinal\finalproj\descriptive_table1.csv"
    dbms=csv
    replace;
run;
