# Developing systems for monitoring navel orangeworm in the presence of mating disruption

Repository: https://github.com/chuckbv/ms-now-ppo-kairomone-2017-2yr

## Overview
Experiments between 2017 and 2019 compared PPO and kairomone blend, examined 
effect of trap type on PPO effectiveness, and examined sex ratio. Experiments
included:

![Experiments table in ./doc/Experiments.csv](/doc/Experiments.JPG)


## Scripts

### script1.R

Reads *.csv files for each of the experiments above into data frames

### script2.R 

Exploration and analysis of the 2017 lure trials. Imports the data into a 
dataframe, overview and ouput for analysis in SAS, produce plot in ggplot
for Figure 2, and peforms a nonparametric ANOVA and post-test for 
non-MD almonds (did not converge in SAS GLIMMIX)

### script2.sas

Uses GLIMMIX to perform GLMM with negative binomial error distribution
for the mating disruption almonds, and for MD and non-MD pistachios.

### script3.R

Examines the data set for April 6 to June 12, 2018. Shows number of
observations by site for three Sites (non-MD pistachio, non-MD almond, and
MD pistachio). Outputs the cumulative counts data set for SAS, and
creates a table pf treatment 



