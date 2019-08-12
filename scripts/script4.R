#============================================================================
# script4.R
#
# Summer 2018 trap comparisons
#
# Parts
# 1. Count data, June 2018 trap test (line 25)
# 2. Count data, July 2018 trap test (line 98)
#
#============================================================================

# load libaries
library(tidyverse)
library(lubridate)
library(DescTools)
library(viridis)
library(multcompView)
library(userfriendlyscience)

# load functions
se <- function(number){ 
  sd(number, na.rm = TRUE)/sqrt(sum(!is.na(number), na.rm = TRUE))
}

#== 1. Count data, June 2018 trap test ======================================

june <- as_tibble(read.csv("./data/Y18b_ppo_buckets_june.csv"))

june$EndDate <- as.Date(mdy(june$EndDate))
june$StartDate <- as.Date(mdy(june$StartDate))
june$IntDays <- as.integer(june$EndDate - june$StartDate)

## 
june_trts <- c("WingPhero","WingPheroPpo","DeltPheroPpo","BuckPpo","BuckPheroPpo")  
june <- mutate(june, Treatment = factor(Treatment, levels = june_trts))

unique(june$StartDate)
# [1] "2018-06-21" "2018-06-29"
unique(june$EndDate)
# [1] "2018-06-29" "2018-07-12"

#weeks <-  

temp1 <- june %>%
  summarise(last = max(EndDate),
            first = min(StartDate)) %>%
  mutate(daze = (last - first)) %>%
  mutate(weeks = daze/7)
 
temp1
# A tibble: 1 x 4
#   last       first      daze    weeks 
#   <date>     <date>     <time>  <time>
# 1 2018-07-12 2018-06-21 21 days 3 days

june
# A tibble: 80 x 13
#      Rep TrapID TrtCode Treatment    Site       Crop   MD    Count  Male CantDist EndDate    StartDate  IntDays
#    <int>  <int> <fct>   <fct>        <fct>      <fct>  <fct> <int> <int>    <int> <date>     <date>       <int>
#  1     1   1011 B       WingPheroPpo VistaVerde Almond Yes      40    23        0 2018-06-29 2018-06-21       8
#  2     1   1012 D       DeltPheroPpo VistaVerde Almond Yes       4     2        0 2018-06-29 2018-06-21       8
#  3     1   1013 C       BuckPheroPpo VistaVerde Almond Yes      10     4        0 2018-06-29 2018-06-21       8
#  4     1   1014 E       BuckPpo      VistaVerde Almond Yes       2     1        0 2018-06-29 2018-06-21       8
#  5     1   1015 A       WingPhero    VistaVerde Almond Yes       0     0        0 2018-06-29 2018-06-21       8
#  6     2   1021 B       WingPheroPpo VistaVerde Almond Yes      32    21        0 2018-06-29 2018-06-21       8
#  7     2   1022 A       WingPhero    VistaVerde Almond Yes       0     0        0 2018-06-29 2018-06-21       8
#  8     2   1023 C       BuckPheroPpo VistaVerde Almond Yes      10     4        0 2018-06-29 2018-06-21       8
#  9     2   1024 E       BuckPpo      VistaVerde Almond Yes       7     2        0 2018-06-29 2018-06-21       8
# 10     2   1025 D       DeltPheroPpo VistaVerde Almond Yes       5     2        0 2018-06-29 2018-06-21       8


Desc(Count ~ EndDate, data = june) # no NAs, declining counts

Desc(Count ~ Treatment, data = june)

june <- june %>% # lump to get experiment-wide response by replicate
  group_by(Treatment, Rep) %>%
  summarise(Count = sum(Count))

june_table <- june %>%
  group_by(Treatment) %>%
  summarise(nObs = sum(!is.na(Count)),
            mn = mean(Count, na.rm = TRUE),
            sem = se(Count))
june_table
#   A tibble: 5 x 4
#   Treatment     nObs     mn   sem
#   <fct>        <int>  <dbl> <dbl>
# 1 WingPhero        8  0.125 0.125
# 2 WingPheroPpo     8 56.8   4.75 
# 3 DeltPheroPpo     8 14.4   2.53 
# 4 BuckPpo          8  8.75  1.70 
# 5 BuckPheroPpo     8 13.9   1.47 

### output june data set for SAS
write.csv(june,"./data/intermediate/y18_traps_june.csv", row.names = FALSE)

#== 2. Count data, July 2018 trap test ======================================

july <- as_tibble(read.csv("./data/Y18b_ppo_buckets_july.csv"))
july$Comment <- NULL # checked in script1

july$EndDate <- as.Date(mdy(july$EndDate))
july$StartDate <- as.Date(mdy(july$StartDate))
july$IntDays <- as.integer(july$EndDate - july$StartDate)


### NB Although the treatment names of the June and July experiments are the 
### same, the form of the Delta Trap used was different
july <- mutate(july, Treatment = factor(Treatment, levels = june_trts))


unique(july$StartDate)
#[1] "2018-07-12" "2018-07-25" "2018-07-31" "2018-08-08" "2018-09-12" "2018-09-18"
unique(july$EndDate)
#[1] "2018-07-25" "2018-07-31" "2018-08-08" "2018-09-12" "2018-09-18" "2018-09-28"

missing_obs <- july %>% 
  filter(is.na(Count)) %>%
  group_by(EndDate) %>%
  summarise(nObs = n()) 
missing_obs
#   A tibble: 2 x 2
#   EndDate     nObs
#   <date>     <int>
# 1 2018-09-12    17
# 2 2018-09-28     1

dont_use <- missing_obs$EndDate



july <- filter(july, !EndDate %in% dont_use)

Desc(Count ~ EndDate, data = july) # no NAs, declining counts

Desc(Count ~ Treatment, data = july)

july <- july %>% # lump to get experiment-wide response by replicate
  group_by(Treatment, Rep) %>%
  summarise(Count = sum(Count))

july_table <- july %>%
  group_by(Treatment) %>%
  summarise(nObs = sum(!is.na(Count)),
            mn = mean(Count, na.rm = TRUE),
            sem = se(Count))
july_table
#   A tibble: 5 x 4
#   Treatment     nObs    mn   sem
#   <fct>        <int> <dbl> <dbl>
# 1 WingPhero        8  0     0   
# 2 WingPheroPpo     8 43.2   4.13
# 3 DeltPheroPpo     8 25.6   3.26
# 4 BuckPpo          8  5.25  1.60
# 5 BuckPheroPpo     8  9     1.65

### output june data set for SAS
write.csv(july,"./data/intermediate/y18_traps_july.csv", row.names = FALSE)

jul_b <- read.csv("./data/intermediate/y18_traps_july.csv", stringsAsFactors = FALSE)
jul_b <- jul_b[jul_b$Treatment != "WingPhero", ]

julb_trts <- c("WingPheroPpo","DeltPheroPpo","BuckPpo","BuckPheroPpo")  
jul_b <- mutate(jul_b, Treatment = factor(Treatment, levels = julb_trts))


test <- oneway(jul_b$Count, jul_b$Treatment, posthoc = "games-howell", posthocLetters = TRUE)
test
