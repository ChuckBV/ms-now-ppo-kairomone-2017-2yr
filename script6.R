#============================================================================
# script6.R
#
# Sex ratios
#
# Parts
# 1. Import data set to data frame (line 32)
# 2. Characterize number of NAs, sampling dates, and weekly phenology 
#    (line 65)
# 3. Pool data across dates, output for SAS, and obtain a table of 
#    mean and SE
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

### Use script1 to load each of the data sets into the global environment
### These include
###  - y17_lures
###  - y17_sexes
###  - y18_july
###  - y18_june
###  - y18_lures
###  - y10_delta

y17_sexes
# A tibble: 2,224 x 12
#    TrtID TrtCode Treatment Crop  MD    Rep    Trap SiteID  Week Date       Sex    Count
#    <dbl> <chr>   <fct>     <fct> <fct> <fct> <dbl>  <dbl> <dbl> <date>     <fct>  <dbl>
#  1     3 C       PpoCombo  Alm   Yes   1       111      1    24 2017-06-15 Female     5
#  2     3 C       PpoCombo  Alm   Yes   1       111      1    24 2017-06-15 Male       8
#  3     3 C       PpoCombo  Alm   Yes   1       111      1    25 2017-06-23 Female     0
#  4     3 C       PpoCombo  Alm   Yes   1       111      1    25 2017-06-23 Male      10
#  5     3 C       PpoCombo  Alm   Yes   1       111      1    27 2017-07-05 Female     5
#  6     3 C       PpoCombo  Alm   Yes   1       111      1    27 2017-07-05 Male      18
#  7     3 C       PpoCombo  Alm   Yes   1       111      1    28 2017-07-14 Female     4
#  8     3 C       PpoCombo  Alm   Yes   1       111      1    28 2017-07-14 Male       5
#  9     3 C       PpoCombo  Alm   Yes   1       111      1    29 2017-07-18 Female     2
# 10     3 C       PpoCombo  Alm   Yes   1       111      1    29 2017-07-18 Male       9