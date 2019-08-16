#============================================================================
# script6b.R
#
# Sex ratios: sript6.R provides a large, messy, exhaustive examination 
# showing that the only significant trends are a negative correlation between
# proportion males in PPO traps in almonds in spring 2018, and a higher 
# of males in wing traps compared to delta or bucket traps in June 2018.
# this script obtains figures for reporting in a more succinct manner.
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

#== Descriptive for the 2017 trap experiment (median, interquartile...) =====

y17all <- read_csv("./data/intermediate/y17_sex_ratio_all_processed.csv")

summary(y17all$prop_males)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.6250  0.7500  0.7214  0.8750  1.0000 

hist(y17all$prop_males)

Desc(prop_males ~ Treatment, data = y17all)

#== Figure for Spring 2018 almonds ==========================================

y18_lures <- read_csv("data/intermediate/y18_lures_processed.csv")
y18alm <- y18_lures %>%
  filter(Crop == "Alm") %>%
  select(-c(TrtCode,Site,Crop,MD))

y18alm$Treatment <- as.factor(y18alm$Treatment)
y18alm$Rep <- as.factor(y18alm$Rep)
y18alm$attractant <- as.factor(y18alm$attractant)
y18alm$phero_lure <- as.factor(y18alm$phero_lure)



y18_alm_pooled <- read_csv("data/intermediate/y18_alm_pooled.csv")

### Assign date data type
y18_alm_pooled$EndDate <- as.Date(y18_alm_pooled$EndDate)

### Assign factor data types
y18_alm_pooled$MD <- NULL
y18_alm_pooled$attractant  <- as.factor(y18_alm_pooled$attractant)
y18_alm_pooled$phero_lure <- as.factor(y18_alm_pooled$phero_lure)

p3 <- ggplot(y18alm) +
  geom_jitter(data = y18alm,
              mapping =  aes(x = EndDate, y = prop_males, size = Count), 
              position=position_jitter(w=0.1, h=0.), shape = 21) +
  geom_point(data = y18_alm_pooled, 
             mapping = aes(x = EndDate, y = Prop_males),
             colour = "red", size = 2) +
  facet_grid(attractant ~ phero_lure) +
  ylim(0,1) +
  theme_bw() + 
  xlab("") +
  ylab("Males as proportion\nof adults captured") +
  theme(axis.text.x = element_text(color = "black", size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 9),
        legend.text = element_text(color = "black", size = 9))

p3

ggsave(filename = "y18alm_sex_ratios.eps", plot = p3, device = "eps", path = "./output", 
       dpi = 300, width = 5.83, height = 5.83, units = "in")

ggsave(filename = "y18alm_sex_ratios.jpg", plot = p3, device = "jpg", path = "./output", 
       dpi = 300, width = 5.83, height = 5.83, units = "in")

#== Figure for June Trap Type comparison ====================================

y18_june <- as_tibble(read.csv("./data/Y18b_ppo_buckets_june.csv"))

y18_june$EndDate <- as.Date(mdy(y18_june$EndDate))
y18_june$StartDate <- as.Date(mdy(y18_june$StartDate))
y18_june$IntDays <- as.integer(y18_june$EndDate - y18_june$StartDate)
y18_june$NowPrWk <- y18_june$Count/y18_june$IntDays*7
## Derive proportion of males captured in traps
y18_june$pMale <- y18_june$Male/(y18_june$Count - y18_june$CantDist)

## Reorder treatment factor
y18_june_trts <- c("WingPhero","WingPheroPpo","DeltPheroPpo","BuckPpo","BuckPheroPpo")  
y18_june$Treatment <- fct_relevel(y18_june$Treatment,y18_june_trts)


y18_june <- y18_june %>% filter(Count > 0 & Treatment != "WingPhero")

p5 <- ggplot(y18_june, aes(x = Treatment, y = pMale))+
  geom_boxplot() +
  ylim(0,1) +
  theme_bw() + 
  xlab("") +
  ylab("Males as proportion\nof adults captured") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 9),
        legend.text = element_text(color = "black", size = 9))

p5

ggsave(filename = "y18junbucket_sex_ratios.eps", plot = p5, device = "eps", path = "./output", 
       dpi = 300, width = 2.83, height = 2.13, units = "in")

ggsave(filename = "y18junbucket_sex_ratios.jpg", plot = p5, device = "jpg", path = "./output", 
       dpi = 300, width = 2.83, height = 2.13, units = "in")