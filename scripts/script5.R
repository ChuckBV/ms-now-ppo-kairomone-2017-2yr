#============================================================================
# script5.R
#
# Summer 2019 trap comparisons
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
library(stringr)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 
lttrextract <-function(string){
  str_extract(string, "\\-*\\D+\\.*\\D*")
} 

#== 1. Count data, 2019 trap test ======================================

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
