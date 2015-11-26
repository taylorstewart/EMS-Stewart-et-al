##############################################################
##############################################################
##  EMS (Taylor Stewart et al.) manuscript
##
##  ZOOPLANKTON ANALYSIS SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get zoop_biomass data.frame
## ===========================================================
source("data_init.R")
zoop_biomass

zoop_test <- zoop_biomass %>% group_by(month,suborder) %>% 
  summarize(total_bio = sum(total_biom)) %>% 
  filter(!is.na(suborder))

ggplot(zoop_test,aes(suborder,total_bio)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~month)
