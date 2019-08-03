#============================================================================
# script2.R
#
# Explore Expt, create Figure 2, and produce minimally sufficient data
# for SAS
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

#== 2. Examination with DescTools, output for SAS, perform nonparametric ====

### Examine distribution of counts by date and treatment

Desc(Count ~ Date, data = counts) # minimual NAs, no unbelievable values
Desc(Count ~ Treatment, data = counts) # consistent with data notebooks

counts %>% 
  filter(is.na(Count)) %>%
  group_by(Week,MD,Crop) %>%
  summarise(nObs = n())
# A tibble: 15 x 4
# Groups:   Week, MD [11]
#      Week MD    Crop   nObs
#    <dbl> <fct> <fct> <int>
#  1    26 Yes   Alm       1
#  2    27 No    Pis       1
#  3    28 Yes   Alm       1
#  4    33 Yes   Alm       1
#  5    34 Yes   Alm      40 # Missed a week at Vista Verde
#  6    34 Yes   Pis      40 # Missed a week at Vista Verde
#  7    35 No    Alm       2
#  8    35 Yes   Alm       4
#  9    36 No    Alm       1
# 10    36 No    Pis       1
# 11    36 Yes   Alm       1
# 12    37 No    Alm       1
# 13    37 No    Pis       3
# 14    37 Yes   Alm       1
# 15    37 Yes   Pis       1


### Pool counts across all weeks to avoid pseudoreplication
counts <- counts[complete.cases(counts), ] 
# -- Not using na.rm, so can't forget the step above!
y17_to_sas <- counts %>% 
  group_by(Crop, MD, Treatment, Rep, Trap) %>%
  summarise(nObs = n(),
            Total = sum(Count))

### Show number of observations (traps) by number of weeks
y17_to_sas %>%
  group_by(nObs) %>%
  summarise(instances = n())
# A tibble: 4 x 2
# nObs instances
#   <int>     <int>
# 1    11         3
# 2    12         7
# 3    13        76
# 4    14        74

write.csv(y17_to_sas, "./data/intermediate/y17_to_sas.csv", row.names = FALSE)

alm_md <- y17_to_sas %>%
  filter(Crop == "Alm" & MD == "Yes")

one.way <- oneway(alm_md$Treatment, y = alm_md$Total, posthoc = 'games-howell')
one.way



#== 3. Plot figure ==========================================================

### Get factor names needed for plot labels
levels(counts$Crop)[levels(counts$Crop) == "Alm"] <- "Almond"
levels(counts$Crop)[levels(counts$Crop) == "Pis"] <- "Pistachio"
levels(counts$MD)[levels(counts$MD) == "No"] <- "Non-disrupted"
levels(counts$MD)[levels(counts$MD) == "Yes"] <- "Mating Disruption"

levels(counts$Treatment)[levels(counts$Treatment) == "StopNow"] <- "Kairomone"
levels(counts$Treatment)[levels(counts$Treatment) == "StopNowCombo"] <- "KairomoneCombo"
unique(counts$Treatment)

FSA::headtail(counts)

### Generate Season-long trap means
counts <- counts[complete.cases(counts), ] # drops from 2240 records to 2141

trapmeans <- counts %>% 
  group_by(Crop, MD, Treatment, Trap) %>%
  summarise(nObs = n(),
            avg = mean(Count))

p2 <-
  ggplot(trapmeans, aes(x = Treatment, y = avg)) +
  geom_boxplot() + 
  theme_bw() +
  facet_grid(Crop ~ MD) +
  
  #ylim(0,100) +
  xlab("") +
  ylab("NOW per trap per week") +
  theme(axis.text.x = element_blank(),
        #axis.text.x = element_text(color = "black", size = 7, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 14))

p2


ggsave(filename = "Y17a2_season_means.eps", p2, path = "./output",
       width = 5.83, height = 5.83, dpi = 300, units = "in", device='eps')

### imported into PowerPoint, where treatment titles and multiple
### range test letter labels are added. Can then be output to postscript


