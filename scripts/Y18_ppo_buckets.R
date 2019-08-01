#============================================================================
# Y18_ppo_buckets.R
# 
# Objectives
# 1. Describe weekly and cumulative total count per trap treatment
#    a. Box plot by week
#    b. Table of weekly cumulative capture
#    c. Statiscal analysis
# 2. Examine overal sex ratio and weekly variation
#============================================================================

#-- 0. Declare libraries and functions -------------------------------------
library(tibble)
library(FSA)
library(DescTools)
library(lubridate)
library(dplyr)
library(ggplot2)

# Uses only base stat package
se <- function(number){ 
  sd(number, na.rm = TRUE)/sqrt(sum(!is.na(number), na.rm = TRUE))
} 

#-- 1. Infile data and clean up --------------------------------------------

library(tibble)
Bucket <- as_tibble(read.csv("./Data/Y18_vistaverde_ppo_buckets.csv"))
headtail(Bucket)
#     Rep TrapID TrtCode    Treatment       Site   Crop  MD Count Male CantDist   EndDate StartDate Comment
# 1     1   1011       B WingPheroPpo VistaVerde Almond Yes    40   23        0 6/29/2018 6/21/2018        
# 2     1   1012       D DeltPheroPpo VistaVerde Almond Yes     4    2        0 6/29/2018 6/21/2018        
# 3     1   1013       C BuckPheroPpo VistaVerde Almond Yes    10    4        0 6/29/2018 6/21/2018        
# 278   8   1083       D DeltPheroPpo VistaVerde Almond Yes     8   NA       NA 9/18/2018 9/12/2018        
# 279   8   1084       C BuckPheroPpo VistaVerde Almond Yes     3   NA       NA 9/18/2018 9/12/2018        
# 280   8   1085       E      BuckPpo VistaVerde Almond Yes     3   NA       NA 9/18/2018 9/12/2018         

### As of 9/23/2018, sex data is more sparse after July, and not avaiable for 9/12 to 9/18

Desc(Bucket$Count)
# Bucket$Count (integer)
# 
# length      n    NAs  unique     0s   mean  meanCI
# 280    263     17      38     89   5.65    4.49
# 93.9%   6.1%          31.8%           6.82
# 
# .05    .10    .25  median    .75    .90     .95
# 0.00   0.00   0.00    2.00   7.00  16.80   27.90
# 
# range     sd  vcoef     mad    IQR   skew    kurt
# 51.00   9.63   1.70    2.97   7.00   2.63    7.13
# 
# lowest : 0 (89), 1 (40), 2 (20), 3 (14), 4 (16)
# highest: 40 (2), 44 (2), 48, 50, 51

library(lubridate)
Bucket$EndDate <- as.Date(mdy(Bucket$EndDate))
Bucket$StartDate <- as.Date(mdy(Bucket$StartDate))
Bucket$IntDays <- as.integer(Bucket$EndDate - Bucket$StartDate)
Bucket$NowPrWk <- Bucket$Count/Bucket$IntDays*7
## Derive proportion of males captured in traps
Bucket$pMale <- Bucket$Male/(Bucket$Count - Bucket$CantDist)


# Trts             
#   TrtCode Treatment     nObs
#   <fct>   <fct>        <int>
# 1 A       WingPhero        8
# 2 B       WingPheroPpo     8
# 3 C       BuckPheroPpo     8
# 4 D       DeltPheroPpo     8
# 5 E       BuckPpo          8

### Get Treatment in desired order
#harv$Replicate <- factor(harv$Replicate, levels = c("T1","T2","T3"))
Bucket$Treatment <- factor(Bucket$Treatment, 
                           levels = c("WingPhero","WingPheroPpo","BuckPheroPpo","DeltPheroPpo","BuckPpo"))

unique(Bucket$Treatment)


#-- 2. Output means and plots for the first two weeks -----------------------

### Note that the experiment is different after the first two weeks--SEE THE 
### README!! Get dates for StartDate and EndDate
unique(Bucket$StartDate)
# [1] "2018-06-21" "2018-06-29" "2018-07-12" "2018-07-25" "2018-07-31"
unique(Bucket$EndDate)
# [1] "2018-06-29" "2018-07-12" "2018-07-25" "2018-07-31" "2018-08-08"

library(dplyr)
June <- filter(Bucket, EndDate <= as.Date("2018-07-12"))
headtail(June)
# Rep TrapID TrtCode    Treatment       Site   Crop  MD Count Male CantDist    EndDate  StartDate Comment IntDays
# 1    1   1011       B WingPheroPpo VistaVerde Almond Yes    40   23        0 2018-06-29 2018-06-21               8
# 2    1   1012       D DeltPheroPpo VistaVerde Almond Yes     4    2        0 2018-06-29 2018-06-21               8
# 3    1   1013       C BuckPheroPpo VistaVerde Almond Yes    10    4        0 2018-06-29 2018-06-21               8
# 78   8   1083       D DeltPheroPpo VistaVerde Almond Yes     7    5        1 2018-07-12 2018-06-29              13
# 79   8   1084       C BuckPheroPpo VistaVerde Almond Yes     6    4        0 2018-07-12 2018-06-29              13
# 80   8   1085       E      BuckPpo VistaVerde Almond Yes     4    2        0 2018-07-12 2018-06-29              13
# NowPrWk     pMale
# 1  35.000000 0.5750000
# 2   3.500000 0.5000000
# 3   8.750000 0.4000000
# 78  3.769231 0.8333333
# 79  3.230769 0.6666667
# 80  2.153846 0.5000000

Desc(June$Count)
### Two weeks, no NAs


PlotsumsJune <- Bucket %>% 
  filter(EndDate <= as.Date("2018-07-12")) %>%
  group_by(Treatment, Rep) %>%
  summarise(nObs = sum(!is.na(Count)),
            Total = sum(Count, na.rm = TRUE))


write.csv(PlotsumsJune, "./Results/Y18_ppo_bucket_plotsums_june.csv", row.names = FALSE)

TrtmeansJune <- PlotsumsJune %>% 
  group_by(Treatment) %>% 
  summarise(nObs = sum(!is.na(Total)),
            mean = mean(Total, na.rm = TRUE),
            se = se(Total))  
TrtmeansJune$meansep <- c("c","a","b","b","bc")
TrtmeansJune
# A tibble: 5 x 4
# Treatment     nObs   mean    se
#   <fct>        <int>  <dbl> <dbl>
# 1 WingPhero        8  0.125 0.125
# 2 WingPheroPpo     8 66.2   6.07 
# 3 DeltPheroPpo     8 23.1   4.28 
# 4 BuckPheroPpo     8 16.8   1.94 
# 5 BuckPpo          8 10.9   1.84 

Desc(Total ~ Treatment, data = PlotsumsJune)
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 31.504, df = 4, p-value = 2.416e-06

PT = dunnTest(Total ~ Treatment,
              data=PlotsumsJune,
              method="bh")    # Can adjust p-values; 
# See ?p.adjust for options 

round(PT,3)

library(multcompView)

multcompLetters(PT, 
                compare="<", 
                threshold=0.05,
                Letters=letters,
                reversed = FALSE)




library(ggplot2)
p1 <- ggplot(data = PlotsumsJune, aes(x = Treatment, y = Total)) + 
  geom_jitter(position=position_jitter(w=0.1, h=0.), size = 1.5, shape = 21) +
  geom_errorbar(data = TrtmeansJune, mapping = aes(x = Treatment, ymin = mean - se, ymax = mean + se), 
                size = 1 , width = 0.4, inherit.aes = FALSE) +
  geom_text(data = TrtmeansJune, aes(label=meansep, x = Treatment, y = mean, hjust=0, vjust=-2),  inherit.aes = FALSE) +
  ylim(0,100) +
  theme_bw() + 
  xlab("") +
  ylab("Total males captured\nin plots") +
  theme(axis.text.x = element_text(color = "black", size = 7, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 9),
        legend.text = element_text(color = "black", size = 9))

p1

### png for draft
ggsave(filename = "Fig_june_bucket_expt.png", plot = p1, device = "png", path = "./Results", 
       dpi = 300, width = 2.83, height = 2.0, units = "in")





Desc(pMale ~ Treatment, data = Bucket)
# Summary: 
#   n pairs: 40, valid: 32 (80.0%), missings: 8 (20.0%), groups: 5
#
# 
#               A        B        C        D        E
# mean         NA    0.644    0.371    0.511    0.496
# median       NA    0.640    0.400    0.519    0.472
# sd           NA    0.051    0.143    0.189    0.230
# IQR          NA    0.040    0.149    0.160    0.121
# n             0        8        8        8        8
# np       0.000%  25.000%  25.000%  25.000%  25.000%
# NAs           8        0        0        0        0
# 0s           NA        0        0        0        0
# 
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 12.744, df = 3, p-value = 0.005225

#-- 3. Follow trend for Bucket traps in July after delta mods ---------------

BucketsJuly <- Bucket[Bucket$EndDate > as.Date("2018-07-12"), ]

ggplot(BucketsJuly,  aes(x = EndDate, y = Count, colour = Treatment, shape = Treatment)) +
       geom_jitter(position=position_jitter(w=0.5, h=0.5))

### Pool data over weeks and get mean and SE

Buckets_july_pooled <- BucketsJuly %>%
  group_by(Treatment, TrapID) %>%
  summarise(nObs = sum(!is.na(Count)),
             Total = sum(Count, na.rm = TRUE))

July_means <- Buckets_july_pooled %>%
  group_by(Treatment) %>%
  summarise(nObs = sum(!is.na(Total)),
            mean = mean(Total),
            se = se(Total))

July_means$meansep <- c()
July_means
# A tibble: 5 x 4
# Treatment     nObs  mean    se
#   <fct>        <int> <dbl> <dbl>
# 1 WingPhero        8  0    0    
# 2 WingPheroPpo     8  9.5  1.92 
# 3 DeltPheroPpo     8  8.75 2.06 
# 4 BuckPheroPpo     8  2.88 0.718
# 5 BuckPpo          8  2.12 0.915

Desc(Total ~ Treatment, data = Buckets_july_pooled)
# Kruskal-Wallis rank sum test:
#   Kruskal-Wallis chi-squared = 35.185, df = 4, p-value = 4.257e-07

PT2 = dunnTest(Total ~ Treatment,
              data=PlotsumsJune,
              method="bh")    # Can adjust p-values; 
# See ?p.adjust for options 

PT2

p2 <- ggplot(data = Buckets_july_pooled, aes(x = Treatment, y = Total)) + 
  geom_jitter(position=position_jitter(w=0.1, h=0.), size = 1.5, shape = 21) +
  geom_errorbar(data = July_means, mapping = aes(x = Treatment, ymin = mean - se, ymax = mean + se), 
                size = 1 , width = 0.4, inherit.aes = FALSE) +
  #geom_text(data = TrtmeansJune, aes(label=meansep, x = Treatment, y = mean, hjust=0, vjust=-2),  inherit.aes = FALSE) +
  #ylim(0,100) +
  theme_bw() + 
  xlab("") +
  ylab("Total males captured\nin plots") +
  theme(axis.text.x = element_text(color = "black", size = 7, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 8),
        axis.title.x = element_text(color = "black", size = 9),
        axis.title.y = element_text(color = "black", size = 9),
        legend.title = element_text(color = "black", size = 9),
        legend.text = element_text(color = "black", size = 9))

p2

### png for draft
ggsave(filename = "Fig_july_bucket_expt.png", plot = p2, device = "png", path = "./Results", 
       dpi = 300, width = 2.83, height = 2.0, units = "in")

