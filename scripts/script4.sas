/****************************************************************************
/ script4.sas
/ Repository: https://github/ChuckBV/ms-now-ppo-kairomone-2017-2yr
/
/ - Read in june and july trap count sums produced by script4.R
/ - Perform GLIMMIX for each
/****************************************************************************/

/*--------------------------- import June data -----------------------------*/
proc import out = june
  datafile = "y18_traps_june.csv"
  dbms=csv replace;
run;

proc glimmix data = June;
  class Treatment Rep;
  model Count = Treatment / dist=nb;
  random Rep;
  lsmeans Treatment / adjust=tukey lines;
run;

/*--------------------------- import June data -----------------------------*/
*proc import out = july
  datafile = "y18_traps_july.csv"
  dbms=csv replace;
*run;

*-- Dump treatment with 0 response --;
*data july;
*  set july;
*  where Treatment ^= "WingPhero";
*run;

*proc glimmix data = July;
*  class Treatment Rep;
*  model Count = Treatment / dist=poisson;
*  random Rep;
*  lsmeans Treatment / adjust=tukey lines;
*run;

