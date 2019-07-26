#===========================================================================
# Program: Y18_abc_lures_kettleman_almond_sexratio.R
#
#===========================================================================

library(tibble)
library(lubridate)
library(dplyr)
library(ggplot2)

#------------------------------------------------------------------------
# Preliminary--vectors containing TrtCodes and Treatments

TrtCodeArray = c("A", "B", "C", "D", "E")
TreatmentArray = c("Biolure","PPO", "PPOBiolure", "StopNow", "StopNowBiolure")

#-----------------------------------------------------------------------
# Read in data file
Kettleman_alm <-  as_tibble(read.csv("./Data/Y18_KettlemanAlmData.csv"))

#-----------------------------------------------------------------------
# Examine the Kettleman Almond data set

## Drop columns accidentally imported
# No columns accidentally imported

Kettleman_alm$StartDate <- as.Date(mdy(Kettleman_alm$StartDate))
Kettleman_alm$EndDate <- as.Date(mdy(Kettleman_alm$EndDate))

## Look at Treatment column
unique(Kettleman_alm$Treatment)


Kettleman_alm$Female <- NULL # All NA, so drop the variable for now 

## Assign a numeric TrtLevel to Kettleman_alm$TrtCode
## 1 for A, 2 for B, etc.
Kettleman_alm$TrtLevel <- match(Kettleman_alm$TrtCode, TrtCodeArray)
## Assign Treatment based on TrtLevel
Kettleman_alm$Treatment <- as.character(TreatmentArray[Kettleman_alm$TrtLevel])
## Verify
Kettleman_alm %>% group_by(TrtLevel, TrtCode, Treatment) %>%
  summarise(n = sum(!is.na(Kettleman_alm$Count)))

## Specify the order
Kettleman_alm <- Kettleman_alm %>% group_by(TrtLevel)

## Specify the order of factor levels as in http://rcompanion.org/rcompanion/d_05.html
Kettleman_alm = Kettleman_alm %>% mutate(TrtFactor = factor(Treatment, levels = unique(Treatment)))

bytrt <- Kettleman_alm %>%
  group_by(EndDate, TrtFactor) %>%
  summarise(n = sum(!is.na(Count)), 
            mean = mean(Count, na.rm = TRUE), 
            sem = sd(Count, na.rm = TRUE)/sqrt(sum(!is.na(Count))))


## Find records with comments, and determine what the comments are
Kettleman_alm[Kettleman_alm$Comment != "", ]
## A tibble: 42 x 14
## Groups:   TrtLevel [5]
#     Rep TrapID TrtCode Treatment      Site   Crop  MD    Count  Male EndDate    StartDate  Comment  TrtLevel TrtFactor
#   <int>  <int> <fct>   <chr>          <fct>  <fct> <fct> <int> <int> <date>     <date>     <fct>       <int> <chr>    
# 1     1    311 B       PPO            Kettl~ Alm   No       39    NA 2018-04-27 2018-04-10 Free fo~        2 PPO      
# 2     5    351 E       StopNowBiolure Kettl~ Alm   No       68    NA 2018-04-27 2018-04-10 "\"Trea~        5 StopNowB~
# 3     5    352 D       StopNow        Kettl~ Alm   No       46    NA 2018-04-27 2018-04-10 "\"Trea~        4 StopNow  
# 4     5    353 C       PPOBiolure     Kettl~ Alm   No       65    NA 2018-04-27 2018-04-10 "\"Trea~        3 PPOBiolu~
# 5     5    354 B       PPO            Kettl~ Alm   No       14    NA 2018-04-27 2018-04-10 "\"Trea~        2 PPO      
# 6     5    355 A       Biolure        Kettl~ Alm   No       85    NA 2018-04-27 2018-04-10 "\"Trea~        1 Biolure  
# 7     6    361 E       StopNowBiolure Kettl~ Alm   No       67    NA 2018-04-27 2018-04-10 "\"Trea~        5 StopNowB~
# 8     6    362 D       StopNow        Kettl~ Alm   No       42    NA 2018-04-27 2018-04-10 "\"Trea~        4 StopNow  
# 9     6    363 C       PPOBiolure     Kettl~ Alm   No       78    NA 2018-04-27 2018-04-10 "\"Trea~        3 PPOBiolu~
# 10    6    364 B       PPO            Kettl~ Alm   No       20    NA 2018-04-27 2018-04-10 "\"Trea~        2 PPO      
# ... with 32 more rows

### Only six levels, which is relevant
unique(Kettleman_alm$Comment)
# [1] Free form comments about specific records except in this case, because this is the first record in this data set                  
# [2]                                                                                                                                   
# [3] "Treatment order identical on reps 5-8 do to errors in plot set-up. Numbers written on traps do not match actual position numbers"
# [4] Trap found on ground                                                                                                              
# [5] Treatment order from 4/10 to 5/26 for reps 5-8 were as shown in previous period                                                   
# [6] Treatments re-ordered 4/26 per planned randomization, and this was used in current monitoring period                              
# 6 Levels:  ...

### Nothing to fix for sex ratio, so drop comments and move on
Kettleman_alm$Comment <- NULL

## Ask for how many weeks we have sexing data
Kettleman_alm_sexes <- Kettleman_alm[!is.na(Kettleman_alm$Male), ]
### 2 records w/o sexing data, all weeks have sexing data


## Look at proportion male by treatment and week
Kettleman_alm_sexes$propMale <- Kettleman_alm_sexes$Male/Kettleman_alm_sexes$Count

p <-
  ggplot(Kettleman_alm_sexes, aes(x=TrtFactor, 
                                  y=propMale,
                                  group = TrtFactor,
                                  size=Count)) + 
  geom_jitter(shape=21, fill="white") + # 21 is filled circle
  xlab("End date") +
  ylab("Proportion males in traps") +
  scale_colour_hue() +                    # Use darker colors, lightness=40
  # expand_limits(y=0) + 
  facet_grid(~ EndDate) +
  theme(axis.title = element_text(color = "black", size = 7),
        axis.text =element_text(color = "black", size=7),
        strip.text.x = element_text(color = "black", size = 8))

p

ggsave(filename = "./Results/Y18_kettleman_alm_sexratio_by_wk.jpg", p,
       width = 5.83, height = 2.25, dpi = 300, units = "in", device='jpg')
