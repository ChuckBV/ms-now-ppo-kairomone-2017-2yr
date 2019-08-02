#============================================================================
# script1.R
#
# Read in PPO study data sets from 2017 to 2019
#
# Parts
# 1. Load 2017 lures test (line 31)
# 2. Load sex ratio data for the 2017 lures test (line 51)
# 3, Load the 2018 follow-up lures test (line 65)
# 4. Load the June and July trap design test (line 90)
# 5. Load the 2019 delta trap test (line 146)
#
#============================================================================

# load libaries
library(tidyverse)
library(lubridate)

# load functions
se <- function(number){ 
  sd(number, na.rm = TRUE)/sqrt(sum(!is.na(number), na.rm = TRUE))
}
library(stringr)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 
lttrextract <-function(string){
  str_extract(string, "\\-*\\D+\\.*\\D*")
} 

#== 1. Count data, 2017 lures test ==========================================

y17_lures <- read_csv("./data/Y17a_counts.csv")

### Pull out treatments for next data set, put in df
trts <- unique(y17_lures$Treatment)
trts # [1] "NowBiolure" "Ppo"   "PpoCombo"    "StopNow"   "StopNowCombo"
TrtCode <- LETTERS[1:5]
trt_df <- data.frame(TrtCode,trts, stringsAsFactors = TRUE)
trt_df <- as_tibble(trt_df)

### Treatments as ordered factors
y17_lures <- mutate(y17_lures, Treatment = factor(Treatment, levels = trts))
y17_lures$Site <- as.factor(y17_lures$Site)
y17_lures$Crop <- as.factor(y17_lures$Crop)
y17_lures$MD <- as.factor(y17_lures$MD)
y17_lures$Rep <- as.factor(y17_lures$Rep)
y17_lures$Date <- as.Date(mdy(y17_lures$Date))


#== 2. Sex data for 2017 lures test =========================================

y17_sexes <- read_csv("./data/Y17a_sex_ratio.csv")

unique(y17_sexes$Treatment) # Same as trts, different order
y17_sexes <- mutate(y17_sexes, Treatment = factor(Treatment, levels = trts))
levels(y17_sexes$Treatment)
y17_sexes$Site <- NULL
y17_sexes$Crop <- as.factor(y17_sexes$Crop)
y17_sexes$MD <- as.factor(y17_sexes$MD)
y17_sexes$Rep <- as.factor(y17_sexes$Rep)
y17_sexes$Date <- as.Date(mdy(y17_sexes$Date))
y17_sexes$Sex <- as.factor(y17_sexes$Sex)

#== 3. Spring 2018 lures test =========================================

y18_lures <- read_csv("./data/Y18_lures.csv")

## Get correct data type for other variables
y18_lures$Rep <- as.factor(y18_lures$Rep)
y18_lures$Treatment <- NULL
y18_lures$Crop <- as.factor(y18_lures$Crop)
y18_lures$MD <- as.factor(y18_lures$MD)
y18_lures$Female <- NULL
y18_lures$EndDate <- as.Date(y18_lures$EndDate)
y18_lures$StartDate <- as.Date(y18_lures$StartDate)

y18_lures$TrtCode <- as.factor(y18_lures$TrtCode)
y18_lures$Site <- as.factor(y18_lures$Site)
y18_lures$Crop <- as.factor(y18_lures$Crop)
y18_lures$MD <- as.factor(y18_lures$MD)

y18_lures <- left_join(trt_df,y18_lures)

y18_lures <- rename(y18_lures, Treatment = trts)

rm(trt_df)


#== 4. Summer/fall 2018 attractant and trap =================================

y18_june <- as_tibble(read.csv("./data/Y18b_ppo_buckets_june.csv"))

y18_june$EndDate <- as.Date(mdy(y18_june$EndDate))
y18_june$StartDate <- as.Date(mdy(y18_june$StartDate))
y18_june$IntDays <- as.integer(y18_june$EndDate - y18_june$StartDate)
y18_june$NowPrWk <- y18_june$Count/y18_june$IntDays*7
## Derive proportion of males captured in traps
y18_june$pMale <- y18_june$Male/(y18_june$Count - y18_june$CantDist)

## 
y18_june_trts <- c("WingPhero","WingPheroPpo","DeltPheroPpo","BuckPpo","BuckPheroPpo")  
y18_june <- mutate(y18_june, Treatment = factor(Treatment, levels = y18_june_trts))

unique(y18_june$StartDate)
# [1] "2018-06-21" "2018-06-29"
unique(y18_june$EndDate)
# [1] "2018-06-29" "2018-07-12"

y18_july <- as_tibble(read.csv("./data/Y18b_ppo_buckets_july.csv"))

y18_july %>%
  group_by(EndDate,Comment) %>%
  summarise(nObs = n())
# A tibble: 6 x 3
# Groups:   EndDate [6]
#   EndDate   Comment                                                           nObs
#   <fct>     <fct>                                                            <int>
# 1 7/25/2018 "Unable to find infromation about the evaluation for this date "    40
# 2 7/31/2018 ""                                                                  40
# 3 8/8/2018  ""                                                                  40
# 4 9/12/2018 ""                                                                  40
# 5 9/18/2018 ""                                                                  40
# 6 9/28/2018 ""                                                                  40

y18_july$Comment <- NULL

y18_july$EndDate <- as.Date(mdy(y18_july$EndDate))
y18_july$StartDate <- as.Date(mdy(y18_july$StartDate))
y18_july$IntDays <- as.integer(y18_july$EndDate - y18_july$StartDate)
y18_july$NowPrWk <- y18_july$Count/y18_july$IntDays*7
## Derive proportion of males captured in traps
y18_july$pMale <- y18_july$Male/(y18_july$Count - y18_july$CantDist)

### NB Although the treatment names of the June and July experiments are the 
### same, the form of the Delta Trap used was different
y18_july <- mutate(y18_july, Treatment = factor(Treatment, levels = y18_june_trts))


unique(y18_july$StartDate)
#[1] "2018-07-12" "2018-07-25" "2018-07-31" "2018-08-08" "2018-09-12" "2018-09-18"
unique(y18_july$EndDate)
#[1] "2018-07-25" "2018-07-31" "2018-08-08" "2018-09-12" "2018-09-18" "2018-09-28"

#== 5. 2019 Delta Trap Experiment ===========================================

y19_delta <- read_csv("./data/Y19_delta.csv")
y19_delta <- y19_delta[ ,3:8] 

# Make Treatment an ordered factor with the desired order
y19_delta <- y19_delta %>% 
  mutate(Treatment = factor(Treatment, 
                            levels = c("WingPhero","WingPpo","WingCombo","DeltaPpo","DeltaCombo","ModPpo","ModCombo")))
levels(y19_delta$Treatment)

y19_delta$StartDate <- as.Date(y19_delta$StartDate)
y19_delta$EndDate <- as.Date(y19_delta$EndDate)
strt_dats <- unique(y19_delta$StartDate)
end_dats <- unique(y19_delta$EndDate)

nmbr_wks <- as.numeric(sum(end_dats - strt_dats)/7)
