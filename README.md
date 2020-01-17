# Developing systems for monitoring navel orangeworm in the presence of mating disruption

Repository: https://github.com/chuckbv/ms-now-ppo-kairomone-2017-2yr

## Overview

###Supports the paper:
Burks CS, Higbee BS, and Beck JR. 2020. Traps and Attractants for Monitoring
Navel Orangeworm (Lepidoptera: Pyralidae) in the Presence of Mating 
Disruption. J. Econ. Entomol. (Accepted for publication)

see "./doc/toz363.pdf"

Experiments between 2017 and 2019 compared PPO and kairomone blend, examined 
effect of trap type on PPO effectiveness, and examined sex ratio. Experiments
included:

![Experiments table in ./doc/Experiments.csv](/doc/Experiments.JPG)


## Scripts

### script1.R

Reads *.csv files for each of the experiments above into data frames

### script2.R, script2b.R 

Exploration and analysis of the 2017 lure trials. Imports the data into a 
dataframe, overview and ouput for analysis in SAS, produce plot in ggplot
for Figure 2, and peforms a nonparametric ANOVA and post-test for 
non-MD almonds (did not converge in SAS GLIMMIX)

script2b.R compares the percent of empty (zero-count) traps with PPO-only
and kairomone-only traps in 2017.

### script2.sas

Uses GLIMMIX to perform GLMM with negative binomial error distribution
for the mating disruption almonds, and for MD and non-MD pistachios.

### script3.R

Examines the data set for April 6 to June 12, 2018. Shows number of
observations by site for three Sites (non-MD pistachio, non-MD almond, and
MD pistachio). Outputs the cumulative counts data set for SAS, and
creates a table pf treatment 

The script is a mess.

### script3.sas

Performs MIX ANOVA for three sites associated with the spring 2018 lure 
comparison.

### script4.R

Two experiments on impact of trap design during the summer of 2018.
"June" expt -- Compare wing, delta, and bucket traps
"July-Sep" expt -- Continuation using a modified delta trap

### script5.R and script5.sas

Summer 2019 experiment on contribution of elements of trap design
and attractant to adults captured

### script5b.R

Examine proportion of empty traps in the summer 2019 trap comparisons

### script6.R

A long, disorganized slog through the data sets for which sex ratio data
are available. A disorganized mess, but primarily demonstrates that there
is "no there there" with respect to differences in sex ratio by other
factors involved in this study.

### script6b.R, script6.sas

Provides graphing and statistical analysis for the samples for which I
do believe I can show statistically significant trends (if just barely)




