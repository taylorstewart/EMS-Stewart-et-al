##############################################################
##############################################################
##  EMS (Stewart et al.) manuscript
##
##  PREY SIZE SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get ems.diet data.frame
## ===========================================================
source("data_init.R")
ems.diet
ems.benthos
ems.zoop

ems.diet.mean <- ems.diet %>% group_by(month,basin,food.item) %>% 
  summarize(mean.size.prey = mean(mean.size))

