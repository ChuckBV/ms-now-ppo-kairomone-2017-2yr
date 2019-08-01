#============================================================================
# script1.R
#
# Read in PPO study data sets from 2017 to 2019
#
# Parts
# 1. 
# 2. 
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

lures17 <- read_csv("./data/Y17a_counts.csv")
trts <- unique(lures17$Treatment)
trts # [1] "NowBiolure" "Ppo"   "PpoCombo"    "StopNow"   "StopNowCombo"
lures17 <- mutate(lures17, Treatment = factor(Treatment, levels = trts))
lures17$Site <- as.factor(lures17$Site)
lures17$Crop <- as.factor(lures17$Crop)
lures17$MD <- as.factor(lures17$MD)
lures17$Rep <- as.factor(lures17$Rep)
lures17$Date <- as.Date(mdy(lures17$Date))


#== 2. Sex data for 2017 lures test =========================================

sexes17 <- read_csv("./data/Y17a_sex_ratio.csv")

unique(sexes17$Treatment) # Same as trts, different order
sexes17 <- mutate(sexes17, Treatment = factor(Treatment, levels = trts))
levels(sexes17$Treatment)
sexes17$Site <- NULL
sexes17$Crop <- as.factor(sexes17$Crop)
sexes17$MD <- as.factor(sexes17$MD)
sexes17$Rep <- as.factor(sexes17$Rep)
sexes17$Date <- as.Date(mdy(sexes17$Date))
sexes17$Sex <- as.factor(sexes17$Sex)

#== 3. Spring 2018 lures test =========================================

lures18a <- read_csv("./data/Y18a_KettlemanAlmData.csv")
## Create "decoder" to translate TrtCode to Treatment
TrtCode <- LETTERS[1:5]
Treatment <- trts
trts_df <- as_tibble(data.frame(TrtCode,Treatment))
trts_df <- mutate(trts_df, Treatment = factor(Treatment, levels = trts))
## Get correct data type for other variables
lures18a$Rep <- as.factor(lures18a$Rep)
lures18a$Treatment <- NULL
lures18a$Crop <- as.factor(lures18a$Crop)
lures18a$MD <- as.factor(lures18a$MD)
lures18a$Female <- NULL
lures18a$EndDate <- as.Date(mdy(lures18a$EndDate))
lures18a$StartDate <- as.Date(mdy(lures18a$StartDate))

lures18a %>% 
  filter(!is.na(Comment)) %>%
  group_by(Comment) %>%
  select(TrapID,EndDate,Comment) %>%
  summarise(nObs = n())

lures18a$Comment <- NULL

lures18b <- read_csv("./data/Y18a_KettlemanPistData.csv")
lures18b$Rep <- as.factor(lures18b$Rep)
lures18b$Treatment <- NULL
lures18b$Crop <- as.factor(lures18b$Crop)
lures18b$MD <- as.factor(lures18b$MD)
lures18b$Female <- NULL
lures18b$EndDate <- as.Date(mdy(lures18b$EndDate))
lures18b$StartDate <- as.Date(mdy(lures18b$StartDate))

lures18b %>% 
  filter(!is.na(Comment)) %>%
  group_by(Comment) %>%
  select(TrapID,EndDate,Comment) %>%
  summarise(nObs = n())

lures18b$Comment <- NULL
lures18b <- lures18b[ ,1:10]

lures18c <- read_csv("./data/Y18a_VistaPistData.csv")
lures18c$Rep <- as.factor(lures18c$Rep)
lures18c$Treatment <- NULL
lures18c$Crop <- as.factor(lures18c$Crop)
lures18c$MD <- as.factor(lures18c$MD)
lures18c$Female <- NULL
lures18c$EndDate <- as.Date(mdy(lures18c$EndDate))
lures18c$StartDate <- as.Date(mdy(lures18c$StartDate))

lures18c %>% 
  filter(!is.na(Comment)) %>%
  group_by(Comment) %>%
  select(TrapID,EndDate,Comment) %>%
  summarise(nObs = n())

lures18c <- lures18c[ ,1:10]

lures18 <- rbind(lures18a,lures18b,lures18c)
lures18 <- left_join(trts_df,lures18)

#== 4. Summer/fall 2018 attractant and trap =================================

