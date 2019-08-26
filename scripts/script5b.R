#============================================================================
# script5b.R
#
# Examine proportion of empty traps in the summer 2019 trap comparisons
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

#== 1. Count data, 2019 trap test ======================================

delta <- read_csv("./data/Y19_delta.csv")
delta <- delta[ ,3:8] # Drop Experiment and Location contants

# Make Treatment an ordered factor with the desired order
delta <- delta %>% 
  mutate(Treatment = factor(Treatment, 
                            levels = c("WingPhero","WingPpo","WingCombo","DeltaPpo","DeltaCombo","ModPpo","ModCombo")))
levels(delta$Treatment)

delta$StartDate <- as.Date(delta$StartDate)
delta$EndDate <- as.Date(delta$EndDate)
strt_dats <- unique(delta$StartDate)
end_dats <- unique(delta$EndDate)

nmbr_wks <- as.numeric(sum(end_dats - strt_dats)/7)

delta

#== 2. Determine proportion 0 counts ========================================

delta %>%
  group_by(Treatment) %>%
  summarise(nZero = sum(Count == 0, na.rm = TRUE),
            nObs = sum(!is.na(Count)),
            pct_zero = 100*nZero/nObs)

### Answer Brad Higbee's query about 40%
dltppo <- delta %>% 
  filter(Treatment == "DeltaPpo") %>%
  arrange(Count)
# Traps with 0 counts in 18 out of 47 rows

dltppo
# Compare this with delta2, the sums data set used in script5.$ to generate
# Fig. 3

dltppo2 <- delta2 %>% 
  filter(Treatment == "DeltaPpo") %>%
  arrange(Count)
dltppo2
# A tibble: 8 x 4
# Groups:   Treatment [1]
#   Treatment Replicate Count perwk
#   <fct>         <dbl> <dbl> <dbl>
# 1 DeltaPpo          2     4 0.519
# 2 DeltaPpo          7     4 0.519
# 3 DeltaPpo          8     4 0.519
# 4 DeltaPpo          6     8 1.04 
# 5 DeltaPpo          3     9 1.17 
# 6 DeltaPpo          4    12 1.56 
# 7 DeltaPpo          5    13 1.69 
# 8 DeltaPpo          1    16 2.07 