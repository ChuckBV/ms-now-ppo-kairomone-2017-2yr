#============================================================================
# y19monitoring1.R
#
# Obtain, as ordered factors, Treatment labels for  monitoring experiment
# Store a key table match TrapID for the two experiment with treatments
#============================================================================

library(tidyverse)
library(readxl)

### Read trap labels and treatmens for experiments 1 and 2
trapids <- read_excel("./data/y19-vistaverde-trapID.xlsx")
head(trapids,3)
# A tibble: 3 x 4
#   TrapID TrapType  Attractant Comments                    
#    <dbl> <chr>     <chr>      <chr>                       
# 1    101 Wing      PPO        "\"Red Suterra wing traps\""
# 2    102 TreceWing L2LPPO     NA                          
# 3    103 Peterson  Peterson   NA

### Drop comments, add unitary treatment label and Experiment variable
trapids$Comments <- NULL
trapids$Treatment <- paste0(trapids$TrapType,"-",trapids$Attractant)
trapids <- mutate(trapids, Experiment = ifelse(TrapID > 100,2,1))

### Obtain unique list of treatment labels for experiments 1
expt1 <- filter(trapids, Experiment == 1)
expt2 <- filter(trapids, Experiment == 2)

expt1_trts <- unique(expt1$Treatment)
expt1_trts <- expt1_trts[c(2,3,4,5,7,6,1)]
expt1_trts

# Levels = c("Wing-Phero","Wing-PPO","Wing-Combo","Delta-PPO","Delta-Combo","Mod-PPO","Mod-Combo)

### Obtain unique list of treatment labels for experiments 1
expt2_trts <- unique(expt2$Treatment)
expt2_trts <- expt2_trts[c(7,1,4,2,5,8,9,10,6,3)]
expt2_trts
# Levels = c("Wing-Phero","Wing-PPO","Wing-Combo","TreceWing-L2LPPO","Delta-L2LPPO","259-L2LPPO","260-L2LPPO","261-L2LPPO","EggTrap-Pherocon","Peterson-Peterson")

#############################################################################
### Save treatment key
trapids <- trapids[ ,c(5,1,4)]
write.csv(trapids,"trt-key-expt1-expt2.csv", row.names = FALSE)
#############################################################################

### To deal with no trts in expt2, get that key in form and order desired
trts2 <- as_tibble(read.csv("./data/trt-key-expt1-expt2.csv"))
trts2 <- filter(trts2, Experiment == 2)

write.csv(trts2,"./data/trt-key-expt2.csv", row.names = FALSE)
### From here, ignomineously use Excel