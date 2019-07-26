#===========================================================================
  # Program: Y18_abc_lures_kettleman_pistachio_sexratio.R
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

# Kettleman_pis$Female <- NULL # Some of them are NA and some are not, line copied over from Y18_abc_lures_vista_verde_pistachio.R file

## Assign a numeric TrtLevel to Kettleman_pis$TrtCode
## 1 for A, 2 for B, etc.
Kettleman_pis$TrtLevel <- match(Kettleman_pis$TrtCode, TrtCodeArray)
## Assign Treatment based on TrtLevel
Kettleman_pis$Treatment <- as.character(TreatmentArray[Kettleman_pis$TrtLevel])
## Verify
Kettleman_pis %>% group_by(TrtLevel, TrtCode, Treatment) %>%
  summarise(n = sum(!is.na(Kettleman_pis$Count)))

## Specify the order
Kettleman_pis <- Kettleman_pis %>% group_by(TrtLevel)

## Specify the order of factor levels as in http://rcompanion.org/rcompanion/d_05.html
Kettleman_pis = Kettleman_pis %>% mutate(TrtFactor = factor(Treatment, levels = unique(Treatment)))

bytrt <- Kettleman_pis %>%
  group_by(EndDate, TrtFactor) %>%
  summarise(n = sum(!is.na(Count)), 
            mean = mean(Count, na.rm = TRUE), 
            sem = sd(Count, na.rm = TRUE)/sqrt(sum(!is.na(Count))))

## Find records with comments, and determine what the comments are
Kettleman_pis[Kettleman_pis$Comment != "", ]
# A tibble: 1 x 15
# Groups:   TrtLevel [1]
# Rep TrapID TrtCode Treatment      Site      Crop  MD    Count  Male Female
# <int>  <int> <fct>   <chr>          <fct>     <fct> <fct> <int> <int>  <int>
#  1     4    443 E       StopNowBiolure Kettleman Pis   No       14    12     NA
# ... with 5 more variables: EndDate <date>, StartDate <date>, Comment <fct>,
#   TrtLevel <int>, TrtFactor <chr>

### Only two levels, which is relevant
unique(Kettleman_pis$Comment)
# [1]                                                                                                                 
# [2] Free form comments about specific records except in this case, because this is the first record in this data set
# 2 Levels:  ...

### Nothing to fix for sex ratio, so drop comments and move on
Kettleman_pis$Comment <- NULL

## Ask for how many weeks we have sexing data
Kettleman_pis_sexes <- Kettleman_pis[!is.na(Kettleman_pis$Male), ]
### Only one record without sexing data, no weeks w/o sexing data

## Look at proportion male by treatment and week
Kettleman_pis_sexes$propMale <- Kettleman_pis_sexes$Male/Kettleman_pis_sexes$Count

p <-
  ggplot(Kettleman_pis_sexes, aes(x=TrtFactor, 
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

ggsave(filename = "./Results/Y18_kettleman_pis_sexratio_by_wk.jpg", p,
       width = 5.83, height = 2.25, dpi = 300, units = "in", device='jpg')
