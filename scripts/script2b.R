#============================================================================
# script2b.R
#
# Use histograms to compare the performance of kairomone and PPO
#
# Parts
# 1. Load 2017 lures test (line 31)
# 2. Examination, output for SAS, and perform non-parametric ANOVA (line 52)
# 3. Plot figure 2 (line 115)
#
#============================================================================

# load libaries
library(tidyverse)
library(lubridate)
library(DescTools)
library(userfriendlyscience)

#== 1. Count data, 2017 lures test ==========================================

### In script 1 this is "y17_lures" Here Counts because it is easier

counts <- read_csv("./data/Y17a_counts.csv")

### Pull out treatments for next data set, put in df
trts <- unique(counts$Treatment)
trts # [1] "NowBiolure" "Ppo"   "PpoCombo"    "StopNow"   "StopNowCombo"
TrtCode <- LETTERS[1:5]
trt_df <- data.frame(TrtCode,trts, stringsAsFactors = TRUE)
trt_df <- as_tibble(trt_df)

### Treatments as ordered factors
counts <- mutate(counts, Treatment = factor(Treatment, levels = trts))
counts$Site <- as.factor(counts$Site)
counts$Crop <- as.factor(counts$Crop)
counts$MD <- as.factor(counts$MD)
counts$Rep <- as.factor(counts$Rep)
counts$Date <- as.Date(mdy(counts$Date))

#== 2. Directly as proportion of traps with 0 captures =====================

trts <- c("Ppo","StopNow")

zoro <- counts %>%
  filter(Treatment %in% trts & MD == "Yes") %>%
  group_by(Crop,MD,Treatment) %>%
  summarise(nZero = sum(Count == 0, na.rm = TRUE),
            nObs = sum(!is.na(Count)),
            pct_blnk = 100*nZero/nObs) %>%
  arrange(Treatment)
zoro
# A tibble: 4 x 6
# Groups:   Crop, MD [2]
#   Crop  MD    Treatment nZero  nObs pct_blnk
#   <fct> <fct> <fct>     <int> <int>    <dbl>
# 1 Alm   Yes   Ppo          21   101     20.8
# 2 Pis   Yes   Ppo          24   104     23.1
# 3 Alm   Yes   StopNow      63   101     62.4
# 4 Pis   Yes   StopNow      46   103     44.7

ppo_kairo <- counts %>% filter(Treatment %in% trts & MD == "Yes")

ggplot(ppo_kairo, aes(x = Count)) +
  geom_histogram() +
  facet_grid(fct_c(Crop,Treatment))
