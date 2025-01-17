/****************************************************************************
/ script6.sas
/ Repository: https://github/ChuckBV/ms-now-ppo-kairomone-2017-2yr
/
/ - Read processed 2017 sex ratio data for almonds and pistachios (sep files)
/ - Do PROC CORR with by-processing by MD status, attractant, and lure 
/ presence
/****************************************************************************/

/*--------------------------- import 2017 almond data -----------------------*/
proc import out = almond
  datafile = "y17_alm_pooled.csv"
  dbms=csv replace;
run;

proc print data=Almond (obs=6);
run;

proc corr data=almond spearman;
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

/*--------------------------- import 2017 pistachio data ------------------------*/
proc import out = pist
  datafile = "y17_pis_pooled.csv"
  dbms=csv replace;
run;

proc print data=pist (obs=6);
run;

proc corr data=pist spearman;
  by MD attractant phero_lure;
  var Prop_males julian;
run;

/*--------------------------- import 2018 almond data -----------------------*/
proc import out = almond18
  datafile = "y18_alm_pooled.csv"
  dbms=csv replace;
run;

proc print data=Almond18 (obs=6);
run;

proc corr data=almond18 spearman;
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

/*--------------------------- import 2018 pistachio data ------------------------*/
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

/*--------------------------- import 2018 june lure data ------------------------*/
proc import out=y17junbuckets
  datafile="y18_june.csv"
  dbms=csv replace;
run;

proc sort data=y17junbuckets;
  by Treatment Rep;
run;
proc means n sum noprint data=y17junbuckets;
  by Treatment Rep;
  var Count Male;
  output out=junebucket (drop = _type_ _freq_)
         n(Count Male) = nCount nMale
		 sum(Count Male)=;
run;

data junebucket;
  set junebucket;
  where nCount=2;
run;

proc glimmix data=junebucket;
  class Treatment Rep;
  model Male/Count = Treatment / dist=binomial;
  random rep;
  lsmeans Treatment / adjust=tukey lines;
run;
