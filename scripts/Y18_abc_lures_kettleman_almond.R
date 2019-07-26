#===========================================================================
# Program: Y18_abc_lures_kettleman_almond.R
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
Kettleman_alm <-  as_tibble(read.csv("./Data/Y18a_KettlemanAlmData.csv"))

#-----------------------------------------------------------------------
# Examine the Kettleman Almond data set

## Drop columns accidentally imported
# No columns accidentally imported

Kettleman_alm$StartDate <- as.Date(mdy(Kettleman_alm$StartDate))
Kettleman_alm$EndDate <- as.Date(mdy(Kettleman_alm$EndDate))

## Look at Treatment column
unique(Kettleman_alm$Treatment)
# [1] NA
Kettleman_alm$Female <- NULL # All NA, so drop the variable for now 

## Determine how many levels of Treatment there are, and how many
## records do not have anything in this column
Kettleman_alm %>% group_by(Treatment) %>%
                  summarise(NmbrRcrds = n())
# A tibble: 6 x 2
# Treatment      NmbrRcrds
# <fct>              <int>
#   1 ""                   195
# 2 Biolure                1
# 3 PPO                    1
# 4 PPOBiolure             1
# 5 StopNow                1
# 6 StopNowBiolure         1


## Assign a numeric TrtLevel to Kettleman_alm$TrtCode
## 1 for A, 2 for B, etc.
Kettleman_alm$TrtLevel <- match(Kettleman_alm$TrtCode, TrtCodeArray)
## Assign Treatment based on TrtLevel
Kettleman_alm$Treatment <- as.character(TreatmentArray[Kettleman_alm$TrtLevel])
## Verify
Kettleman_alm %>% group_by(TrtLevel, TrtCode, Treatment) %>%
                  summarise(n = sum(!is.na(Kettleman_alm$Count)))
# 
# Summary: 
# TrtLevel TrtCode Treatment          n
#       <int> <fct>   <chr>          <int>
# 1        1 A       Biolure          198
# 2        2 B       PPO              198
# 3        3 C       PPOBiolure       198
# 4        4 D       StopNow          198
# 5        5 E       StopNowBiolure   198

## Concise crosstab table from base R
table(Kettleman_alm$EndDate,Kettleman_alm$TrtCode) # A will be rows, B will be columns
#
# Summary:
#            A B C D E
# 2018-04-27 8 8 8 8 8
# 2018-05-04 8 8 8 8 8
# 2018-05-16 8 8 8 8 8
# 2018-05-25 8 8 8 8 8
# 2018-05-29 8 8 8 8 8

## Plot by week

#--------------------------------------------------------------------------------------
# Get treatments as factors in desired order

## Specify the order
Kettleman_alm <- Kettleman_alm %>% group_by(TrtLevel)

## Specify the order of factor levels as in http://rcompanion.org/rcompanion/d_05.html
Kettleman_alm = Kettleman_alm %>% mutate(TrtFactor = factor(Treatment, levels = unique(Treatment)))

bytrt <- Kettleman_alm %>%
  group_by(EndDate, TrtFactor) %>%
  summarise(n = sum(!is.na(Count)), 
            mean = mean(Count, na.rm = TRUE), 
            sem = sd(Count, na.rm = TRUE)/sqrt(sum(!is.na(Count))))


bytrt

p <-
  ggplot(bytrt, aes(x=EndDate, y=mean, colour=TrtFactor, group=TrtFactor)) + 
  geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem), colour="black", width=1) +
  geom_line(size=1) +
  geom_point(size=3, shape=21, fill="white") + # 21 is filled circle
  xlab("End date") +
  ylab("NOW Adults") +
  scale_colour_hue() +                    # Use darker colors, lightness=40
  expand_limits(y=0) + 
  theme(axis.title = element_text(color = "black", size = 7),
        axis.text =element_text(color = "black", size=7),
        strip.text.x = element_text(color = "black", size = 8))

p

ggsave(filename = "./Results/Y18_kettleman_almond_by_wk.jpg", p,
       width = 5.83, height = 2.25, dpi = 300, units = "in", device='jpg')

