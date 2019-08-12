/****************************************************************************
/ script5.sas
/ Repository: https://github/ChuckBV/ms-now-ppo-kairomone-2017-2yr
/
/ - Read in early summer delta trap test output by script5.R
/ - Perform GLIMMIX
/****************************************************************************/

/*--------------------------- import June data -----------------------------*/
proc import out = delta
  datafile = "Y19_delta_traps.csv"
  dbms=csv replace;
run;

proc glimmix data = delta;
  class Treatment Replicate;
  model Count = Treatment / dist=nb;
  random Replicate;
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

