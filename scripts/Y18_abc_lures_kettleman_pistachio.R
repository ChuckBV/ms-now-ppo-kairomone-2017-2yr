#===========================================================================
# Program: Y18_abc_lures_kettleman_pistachio.R
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
Kettleman_pis <-  as_tibble(read.csv("./Data/Y18_KettlemanPistData.csv"))

#-----------------------------------------------------------------------
# Examine the Kettleman Pisachio data set

## Drop columns accidentally imported
Kettleman_pis <- Kettleman_pis %>% select(-X, -X.1, -X.2, -X.3)

Kettleman_pis$StartDate <- as.Date(mdy(Kettleman_pis$StartDate))
Kettleman_pis$EndDate <- as.Date(mdy(Kettleman_pis$EndDate))

## Look at Treatment column
unique(Kettleman_pis$Treatment)
# [1] NA
# Kettleman_pis$Female <- NULL # Some of them are NA and some are not, line copied over from Y18_abc_lures_vista_verde_pistachio.R file

## Determine how many levels of Treatment there are, and how many
## records do not have anything in this column
Kettleman_pis %>% group_by(Treatment) %>%
                  summarise(NmbrRcrds = n())
# A tibble: 5 x 2
# Treatment      NmbrRcrds
# <fct>              <int>
# 1 Biolure             48
# 2 PPO                 48
# 3 PPOBiolure          48
# 4 StopNow             48
# 5 StopNowBiolure      48


## Assign a numeric TrtLevel to Kettleman_pis$TrtCode
## 1 for A, 2 for B, etc.
Kettleman_pis$TrtLevel <- match(Kettleman_pis$TrtCode, TrtCodeArray)
## Assign Treatment based on TrtLevel
Kettleman_pis$Treatment <- as.character(TreatmentArray[Kettleman_pis$TrtLevel])
## Verify
Kettleman_pis %>% group_by(TrtLevel, TrtCode, Treatment) %>%
                  summarise(n = sum(!is.na(Kettleman_pis$Count)))
# 
# Summary: 
# A tibble: 5 x 4
# Groups:   TrtLevel, TrtCode [?]
#   TrtLevel TrtCode Treatment          n
#      <int> <fct>   <chr>          <int>
# 1        1 A       Biolure          239
# 2        2 B       PPO              239
# 3        3 C       PPOBiolure       239
# 4        4 D       StopNow          239
# 5        5 E       StopNowBiolure   239

## Concise crosstab table from base R
table(Kettleman_pis$EndDate,Kettleman_pis$TrtCode) # A will be rows, B will be columns
#
# Summary: 
#            A B C D E
# 2018-04-13 8 8 8 8 8
# 2018-04-27 8 8 8 8 8
# 2018-05-07 8 8 8 8 8
# 2018-05-17 8 8 8 8 8
# 2018-05-24 8 8 8 8 8
# 2018-05-29 8 8 8 8 8

## Plot by week

#--------------------------------------------------------------------------------------
# Get treatments as factors in desired order

## Specify the order
Kettleman_pis <- Kettleman_pis %>% group_by(TrtLevel)

## Specify the order of factor levels as in http://rcompanion.org/rcompanion/d_05.html
Kettleman_pis = Kettleman_pis %>% mutate(TrtFactor = factor(Treatment, levels = unique(Treatment)))

bytrt <- Kettleman_pis %>%
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

ggsave(filename = "./Results/Y18_kettleman_pistachio_by_wk.jpg", p,
       width = 5.83, height = 2.25, dpi = 300, units = "in", device='jpg')



