/****************************************************************************
/ script3.sas
/ Repository: https://github/ChuckBV/ms-now-ppo-kairomone-2017-2yr
/
/ - Read in spring flight 1 trap count sums produced by script3.R
/ - Output to separate data sets by crop and mating disruption status (40 each)
/ - Perform GLIMMIX for each
/****************************************************************************/

/*--------------------------- import the data -----------------------------*/
proc import out = sumcounts
  datafile = "y18lures_to_sas.csv"
  dbms=csv replace;
run;

/*---------------------------  subset the data ----------------------------*/
data alm_nomd pi_md pi_nomd;
  set sumcounts;
  if Crop = "Alm" then do;
      if MD = "No" then output alm_nomd;
	    else put 'oops!';
	  end;
    else do;
	  if MD = "No" then output pi_nomd;
	    else output pi_md;
	  end;
run;

/*---------------------------   glimmix for each location  ----------------*/

proc mixed data = alm_nomd;
  class Treatment Rep;
  model Total = Treatment;
  random Rep;
  lsmeans Treatment / adjust=tukey;
run; 

proc glimmix data = pi_md;
  class Treatment Rep;
  model Total = Treatment / dist=nb;
  random Rep;
  lsmeans Treatment / adjust=tukey lines;
run; 

proc glimmix data = pi_nomd;
  class Treatment Rep;
  model Total = Treatment / dist=nb;
  random Rep;
  lsmeans Treatment / adjust=tukey lines;
run; 

