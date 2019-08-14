/****************************************************************************
/ script6.sas
/ Repository: https://github/ChuckBV/ms-now-ppo-kairomone-2017-2yr
/
/ - Read processed 2017 sex ratio data for almonds and pistachios (sep files)
/ - Do PROC CORR with by-processing by MD status, attractant, and lure 
/ presence
/****************************************************************************/

/*--------------------------- import almond data -----------------------------*/
proc import out = almond
  datafile = "y17_alm_pooled.csv"
  dbms=csv replace;
run;

proc print data=Almond (obs=6);
run;

proc corr data=almond;
  by MD attractant phero_lure;
  var Prop_males julian;
run;

proc sort data=almond;
  by attractant;
run;
proc univariate data=almond;
  by attractant;
  var Prop_males;
run;

/*--------------------------- import pistachio data -----------------------------*/
proc import out = pist
  datafile = "y17_pis_pooled.csv"
  dbms=csv replace;
run;

proc print data=pist (obs=6);
run;

proc corr data=pist;
  by MD attractant phero_lure;
  var Prop_males julian;
run;
