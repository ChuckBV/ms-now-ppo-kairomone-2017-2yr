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
#   A tibble: 336 x 6
#    TrapID Replicate Treatment  StartDate  EndDate    Count
#     <dbl>     <dbl> <fct>      <date>     <date>     <dbl>
#  1     11         1 ModCombo   2019-05-17 2019-05-23     1
#  2     12         1 WingPhero  2019-05-17 2019-05-23     0
#  3     13         1 WingPpo    2019-05-17 2019-05-23     1
#  4     14         1 WingCombo  2019-05-17 2019-05-23     0
#  5     15         1 DeltaPpo   2019-05-17 2019-05-23     0
#  6     16         1 ModPpo     2019-05-17 2019-05-23     1
#  7     17         1 DeltaCombo 2019-05-17 2019-05-23     0
#  8     21         2 WingPpo    2019-05-17 2019-05-23     1
#  9     22         2 WingPhero  2019-05-17 2019-05-23     0
# 10     23         2 ModCombo   2019-05-17 2019-05-23     0
# ... with 326 more rows

Desc(Count ~ EndDate, data = delta)

Desc(Count ~ Treatment, data = delta)

#== 2. Collapse across weeks ================================================

delta2 <- delta %>%
  group_by(Treatment,Replicate) %>%
  summarise(Count = sum(Count, na.rm = TRUE))

delta2

write.csv(delta2,"./data/intermediate/y19_delta_traps.csv", row.names = FALSE)

#== 3. Plot data ============================================================

delta2$perwk <- delta2$Count/nmbr_wks
delta2

p1 <-
  ggplot(delta2, aes(x = Treatment, y = perwk)) +
  geom_boxplot() + 
  theme_bw() +

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

p1


ggsave(filename = "Y19_delta_trap_comp.eps", p1, path = "./output",
       width = 5.83, height = 2.91, dpi = 300, units = "in", device='eps')

ggsave(filename = "Y19_delta_trap_comp.jpg", p1, path = "./output",
       width = 5.83, height = 2.91, dpi = 300, units = "in", device='jpg')
