#============================================================================
# script3.R
#
# Explore results and generate table for spring 2018 experiment
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

#== 1. Count data, 2017 lures test ==========================================

counts <- read_csv("./data/Y18_lures.csv")
### in script1 this data set is assigned to y18_lures. Here generic "counts" 
### is used

## Get correct data type for other variables
counts$Rep <- as.factor(counts$Rep)
counts$Treatment <- NULL
counts$Crop <- as.factor(counts$Crop)
counts$MD <- as.factor(counts$MD)
counts$Female <- NULL
counts$EndDate <- as.Date(mdy(counts$EndDate))
counts$StartDate <- as.Date(mdy(counts$StartDate))

counts$TrtCode <- as.factor(counts$TrtCode)
counts$Site <- as.factor(counts$Site)
counts$Crop <- as.factor(counts$Crop)
counts$MD <- as.factor(counts$MD)

### Pull in treatment names
trts <- c("NowBiolure","Ppo","PpoCombo","StopNow","StopNowCombo")
TrtCode <- LETTERS[1:5]
trt_df <- data.frame(TrtCode,trts, stringsAsFactors = TRUE)
trt_df <- as_tibble(trt_df)

counts <- left_join(trt_df,counts)

counts <- rename(counts, Treatment = trts)

### Clean up 
rm(trt_df,trts,TrtCode)

#== 2. Characterize number of NAs, sampling dates, and weekly phenology =====
counts %>%
  group_by(Site,Crop,MD) %>%
  summarise(nObs = n())
# A tibble: 3 x 4
# Groups:   Site, Crop [3]
#   Site       Crop  MD     nObs
#   <fct>      <fct> <fct> <int>
# 1 Kettleman  Alm   No      240
# 2 Kettleman  Pis   No      280
# 3 VistaVerde Pis   Yes     240

# Number of NAs in the above is respectively 2, 1, and 8
counts %>%
  group_by(Site,Crop,MD) %>%
  summarise(Begin = min(StartDate),
            End = max(EndDate))
# A tibble: 3 x 5
# Groups:   Site, Crop [3]
#   Site       Crop  MD    Begin      End       
#   <fct>      <fct> <fct> <date>     <date>    
# 1 Kettleman  Alm   No    2018-04-10 2018-06-07
# 2 Kettleman  Pis   No    2018-04-06 2018-06-07
# 3 VistaVerde Pis   Yes   2018-04-09 2018-06-12



alm_nomd <- counts %>% filter(Crop == "Alm" & MD == "No")
pis_nomd <- counts %>% filter(Crop == "Pis" & MD == "No")
pis_md <- counts %>% filter(Crop == "Pis" & MD == "Yes")

### Get means and weekly plot for almonds, no mating disruption
alm_nomd <- alm_nomd %>%
  group_by(Treatment,EndDate) %>%
  summarise(nObs = sum(!is.na(Count)),
            avg = mean(Count, na.rm = TRUE),
            sem = se(Count))

p1 <- ggplot(alm_nomd, aes(x = EndDate, y = avg, colour = Treatment)) +
  geom_point(size = 3, shape = 21, fill = "white") +
  geom_errorbar(data = alm_nomd, mapping = aes(x = EndDate, 
                                              ymin = avg - sem, ymax = avg + sem), 
                size = 1 , width = 0.4) +
  geom_line() +
  ggtitle("2018, almond, no mating disruption") +
  scale_colour_viridis(discrete = TRUE) +
  xlab("") +
  ylab("Males per trap") +
  theme(legend.position = c(0.85, 0.7),
        # moved legend inside with this, see https://rpubs.com/folias/A-simple-example-on-ggplot2-legend-options     
        axis.text.x = element_text(color = "black", size = 7), 
        axis.text.y = element_text(color = "black", size = 7),
        axis.title.x = element_text(color = "black", size = 7),
        axis.title.y = element_text(color = "black", size = 7),
        legend.title = element_text(color = "black", size = 7),
        legend.text = element_text(color = "black", size = 7))

p1

ggsave(filename = "y18-lures-wkly-alm-nomd.jpg", plot = p1, device = "jpg", path = "./output", 
   dpi = 300, width = 5.83, height = 3.0, units = "in")



### Get means and weekly plot for pistachios, no mating disruption
pis_nomd <- pis_nomd %>%
  group_by(Treatment,EndDate) %>%
  summarise(nObs = sum(!is.na(Count)),
            avg = mean(Count, na.rm = TRUE),
            sem = se(Count))

p2 <- ggplot(pis_nomd, aes(x = EndDate, y = avg, colour = Treatment)) +
  geom_point(size = 3, shape = 21, fill = "white") +
  geom_errorbar(data = pis_nomd, mapping = aes(x = EndDate, 
                                               ymin = avg - sem, ymax = avg + sem), 
                size = 1 , width = 0.4) +
  geom_line() +
  ggtitle("2018, pistachio, no mating disruption") +
  scale_colour_viridis(discrete = TRUE) +
  xlab("") +
  ylab("Males per trap") +
  theme(legend.position = c(0.85, 0.7),
        # moved legend inside with this, see https://rpubs.com/folias/A-simple-example-on-ggplot2-legend-options     
        axis.text.x = element_text(color = "black", size = 7), 
        axis.text.y = element_text(color = "black", size = 7),
        axis.title.x = element_text(color = "black", size = 7),
        axis.title.y = element_text(color = "black", size = 7),
        legend.title = element_text(color = "black", size = 7),
        legend.text = element_text(color = "black", size = 7))

p2

ggsave(filename = "y18-lures-wkly-pis-nomd.jpg", plot = p2, device = "jpg", path = "./output", 
       dpi = 300, width = 5.83, height = 3.0, units = "in")

  

### Get means and weekly plot for pistachios, with mating disruption
pis_md <- pis_md %>%
  group_by(Treatment,EndDate) %>%
  summarise(nObs = sum(!is.na(Count)),
            avg = mean(Count, na.rm = TRUE),
            sem = se(Count))

p3 <- ggplot(pis_md, aes(x = EndDate, y = avg, colour = Treatment)) +
  geom_point(size = 3, shape = 21, fill = "white") +
  geom_errorbar(data = pis_md, mapping = aes(x = EndDate, 
                                               ymin = avg - sem, ymax = avg + sem), 
                size = 1 , width = 0.4) +
  geom_line() +
  ggtitle("2018, pistachio, with mating disruption") +
  scale_colour_viridis(discrete = TRUE) +
  xlab("") +
  ylab("Males per trap") +
  theme(legend.position = c(0.85, 0.7),
        # moved legend inside with this, see https://rpubs.com/folias/A-simple-example-on-ggplot2-legend-options     
        axis.text.x = element_text(color = "black", size = 7), 
        axis.text.y = element_text(color = "black", size = 7),
        axis.title.x = element_text(color = "black", size = 7),
        axis.title.y = element_text(color = "black", size = 7),
        legend.title = element_text(color = "black", size = 7),
        legend.text = element_text(color = "black", size = 7))

p3

ggsave(filename = "y18-lures-wkly-pis-md.jpg", plot = p3, device = "jpg", path = "./output", 
       dpi = 300, width = 5.83, height = 3.0, units = "in")

#== 3. Pool males captured across all dates, output to SAS, make table ======

### Examine extent to which NAs mess up the data
test <- counts %>% 
  filter(is.na(Count)) %>%
  arrange(Crop,MD,Treatment,Rep,EndDate)

rmv_these <- unique(test$TrapID)

### Remove traps from consideration if there were NAs some weeks
counts2 <- counts %>%
  filter(!TrapID %in% rmv_these) 

totals <- counts2 %>%
  group_by(Crop, MD, Treatment,Rep) %>%
  summarise(nObs = sum(!is.na(Count)),
            total = sum(Count, na.rm = TRUE))

totals %>%
  group_by(nObs) %>%
  summarise(nTimes = n())
# A tibble: 2 x 2
#    nObs nTimes
#   <int>  <int>
# 1     6     76
# 2     7     39

write.csv(totals, "./data/intermediate/y18lures_to_sas.csv", row.names = FALSE)

### Get tables of mean and SE
### Start by expressing as weekly rather than total
totals <- totals %>%
  mutate(perwk = total/nObs)

### Get table
avg_wkly <- totals %>%
  group_by(Crop,MD,Treatment) %>%
  summarise(nObs = sum(!is.na(perwk)),
            avg = mean(perwk, na.rm = TRUE),
            sem = se(perwk)) 
avg_wkly

write.csv(avg_wkly,"./output/y18-lures-cumulative.csv", row.names = FALSE)

### Isolate Almond No-mating disruption and use histogram to examine 
### frequency dist
alm_no_sums <- totals %>%
  filter(Crop == "Alm" & MD == "No")

hist(alm_no_sums$total)
###  -- The histogram and the residuals test say that MIXED is appropriate
 
pist_no_sums <- totals %>%
  filter(Crop == "Pis" & MD == "No")

hist(pist_no_sums$total)
###  -- Troubling residuals for MIXED, and troubling Dev/df for GLMM with
###  -- either poi or nb

ggplot(pist_no_sums, aes(x = Treatment, y = total)) +
  geom_boxplot()

Desc(total ~ Treatment, data = pist_no_sums)
# Kruskal-Wallis rank sum test:
#  Kruskal-Wallis chi-squared = 18.157, df = 4, p-value = 0.00115
### This Box plot suggests heteroscadisty more than the ggplot boxplot

test <- oneway(pist_no_sums$total, pist_no_sums$Treatment, posthoc = "games-howell", posthocLetters = TRUE)
test

pist_sums <- totals %>%
  filter(Crop == "Pis" & MD == "Yes")

pist_sums
hist(pist_sums$total)
Desc(total ~ Treatment, data = pist_sums)
