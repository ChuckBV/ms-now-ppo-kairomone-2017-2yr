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

#== 1. Review sex ratio information in the data sets ========================

### Use script1 to load each of the data sets into the global environment
### These include
###  - y17_sexes
###  - y18_lures
###  - y18_june
###  - y18_july

y17_sexes
# A tibble: 2,224 x 12
#    TrtID TrtCode Treatment Crop  MD    Rep    Trap SiteID  Week Date       Sex    Count
#    <dbl> <chr>   <fct>     <fct> <fct> <fct> <dbl>  <dbl> <dbl> <date>     <fct>  <dbl>
#  1     3 C       PpoCombo  Alm   Yes   1       111      1    24 2017-06-15 Female     5
#  2     3 C       PpoCombo  Alm   Yes   1       111      1    24 2017-06-15 Male       8
#  3     3 C       PpoCombo  Alm   Yes   1       111      1    25 2017-06-23 Female     0
#  4     3 C       PpoCombo  Alm   Yes   1       111      1    25 2017-06-23 Male      10

head(y18_lures,4)
#  A tibble: 4 x 11
#   TrtCode Treatment  Rep   TrapID Site      Crop  MD    Count  Male EndDate    StartDate 
#   <fct>   <fct>      <fct>  <dbl> <fct>     <fct> <fct> <dbl> <dbl> <date>     <date>    
# 1 A       NowBiolure 1        312 Kettleman Alm   No       88    88 2018-04-27 2018-04-10
# 2 A       NowBiolure 2        323 Kettleman Alm   No       94    94 2018-04-27 2018-04-10
# 3 A       NowBiolure 3        331 Kettleman Alm   No       73    73 2018-04-27 2018-04-10
# 4 A       NowBiolure 4        344 Kettleman Alm   No       85    85 2018-04-27 2018-04-10

head(y18_june,4)
# A tibble: 4 x 15
# Rep TrapID TrtCode Treatment   Site     Crop  MD    Count  Male CantDist EndDate    StartDate  IntDays NowPrWk pMale
# <int>  <int> <fct>   <fct>       <fct>    <fct> <fct> <int> <int>    <int> <date>     <date>       <int>   <dbl> <dbl>
# 1     1   1011 B       WingPheroP~ VistaVe~ Almo~ Yes      40    23        0 2018-06-29 2018-06-21       8   35    0.575
# 2     1   1012 D       DeltPheroP~ VistaVe~ Almo~ Yes       4     2        0 2018-06-29 2018-06-21       8    3.5  0.5  
# 3     1   1013 C       BuckPheroP~ VistaVe~ Almo~ Yes      10     4        0 2018-06-29 2018-06-21       8    8.75 0.4  
# 4     1   1014 E       BuckPpo     VistaVe~ Almo~ Yes       2     1        0 2018-06-29 2018-06-21       8    1.75 0.5 

head(y18_july,4)
# A tibble: 4 x 15
#     Rep TrapID TrtCode Treatment   Site     Crop  MD    Count  Male CantDist EndDate    StartDate  IntDays NowPrWk pMale
#   <int>  <int> <fct>   <fct>       <fct>    <fct> <fct> <int> <int> <lgl>    <date>     <date>       <int>   <dbl> <dbl>
# 1     1   1011 B       WingPheroP~ VistaVe~ Almo~ Yes       1     0 NA       2018-07-25 2018-07-12      13   0.538    NA
# 2     1   1012 D       DeltPheroP~ VistaVe~ Almo~ Yes       0    NA NA       2018-07-25 2018-07-12      13   0        NA
# 3     1   1013 C       BuckPheroP~ VistaVe~ Almo~ Yes       1    NA NA       2018-07-25 2018-07-12      13   0.538    NA
# 4     1   1014 E       BuckPpo     VistaVe~ Almo~ Yes       0    NA NA       2018-07-25 2018-07-12      13   0        NA

#=== 3. Wrangle y17_sexes to allow more direct examination of sex ration ====

### Strategy: 1) Obtain a summary data set showing total NOW for each 
### trap x data. 2) Merge this summary back with the male data set. Now have
### total and males, like the other data set.

### carefully obtain pooled data set
y17_sexes %>% 
  group_by(Trap,Date) %>%
  summarise(nObs = n()) %>%
  filter(nObs != 2) # shows 0

y17both <- y17_sexes %>% 
  group_by(Treatment,Crop,MD,Trap,Date) %>%
  summarise(Count = sum(Count)) # purposely left na.rm to default of FALSE

y17both %>% filter(is.na(Count)) # 4 observations

y17both <- rename(y17both, total_now = Count)

### Create a reduced version of y17_sexes to merge males back with both
#y17males <-

y17males <- y17_sexes %>%
  filter(Sex == "Male") %>%
  select(Trap,Date,Count) 

y17males <- rename(y17males, males = Count)

y17all <- inner_join(y17both,y17males)

### No NAs, all complete cases
y17all <- filter(y17all, !is.na(total_now))

y17all %>%
  mutate(fem = total_now - males) %>%
  filter(fem < 0) # This ridiculous situation does not exist

### create a proportion variable
y17all$prop_males <- y17all$males/y17all$total_now

### Drop pheromone lure
y17all <- filter(y17all, Treatment != "NowBiolure")

### Create variable for attractant only
y17all <- mutate(y17all,attractant = ifelse(str_detect(Treatment,"Ppo",),"PPO","Kairomone"))
y17all$attractant <- as.factor(y17all$attractant)

### Create variable for co-presentation with pheromone
y17all <- mutate(y17all, phero_lure = ifelse(str_detect(Treatment,"Combo"),"Pheromone lure","No pheromone lure"))
y17all$phero_lure <- as.factor(y17all$phero_lure)

### Split by crop
y17alm <- filter(y17all, Crop == "Alm")
y17pis <- filter(y17all, Crop == "Pis")

y17_alm_pooled <- y17alm %>%
  group_by(MD,attractant,phero_lure,Date) %>%
  summarise(total_now = sum(total_now),
            males = sum(males))
y17_alm_pooled <- mutate(y17_alm_pooled, Prop_males = males/total_now) 



#== 2. Generate ggplot2 plots from these data sets ==========================


### y17_sexes
ggplot(y17alm) +
  geom_jitter(data = y17alm,
              mapping =  aes(x = Date, y = prop_males, size = total_now), 
              position=position_jitter(w=0.1, h=0.), shape = 21) +
  geom_point(data = y17_alm_pooled, 
             mapping = aes(x = Date, y = Prop_males),
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

cor.test(~ Prop_males + Date,
         data = y17_alm_pooled,
         method = "pearson",
         conf.level = 0.95)

  
