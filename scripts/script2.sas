/****************************************************************************
/ script2.sas
/ Repository: https://github/ChuckBV/ms-now-ppo-kairomone-2017-2yr
/
/ - Read in season-long sum of trap counts produced by script2.R
/ - Output to separate data sets by crop and mating disruption status (40 each)
/ - Perform GLIMMIX for each
/****************************************************************************/

/*--------------------------- import the data -----------------------------*/
proc import out = sumcounts
  datafile = "y17_to_sas.csv"
  dbms=csv replace;
run;

/*---------------------------  subset the data ----------------------------*/
data alm_md alm_nomd pi_md pi_nomd;
  set sumcounts;
  if Crop = "Alm" then do;
      if MD = "No" then output alm_nomd;
	    else output alm_md;
	  end;
    else do;
	  if MD = "No" then output pi_nomd;
	    else output pi_md;
	  end;
run;

/*---------------------------   glimmix for each location  ----------------*/

*proc glimmix data = alm_md;
  *class Treatment Rep;
  *model Total = Treatment / dist=nb;
  *random Rep;
  *lsmeans Treatment / adjust=tukey lines;
*run; 
*-- does not converage, use Welch ANOVA and games-howell posthoc via R --;

proc glimmix data = alm_nomd;
  class Treatment Rep;
  model Total = Treatment / dist=nb;
  random Rep;
  lsmeans Treatment / adjust=tukey lines;
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

