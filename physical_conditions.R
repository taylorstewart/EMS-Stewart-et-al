##############################################################
##############################################################
##  EMS (Stewart et al.) manuscript
##
##  PHYSICAL CONDITIONS SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get ems.pc data.frame
## ===========================================================
source("data_init.R")
ems.pc

## -----------------------------------------------------------
## 
## -----------------------------------------------------------
ems.pc %<>% filter(serial %in% c(as.character(unique(ems.diet$serial)))) %>% 
  mutate(basin=factor(basin,levels = c('Western','Central','Eastern'),ordered = TRUE))

ggplot(ems.pc,aes(temp.mean,depth.mean)) +
  geom_line() +
  scale_y_reverse() +
  facet_wrap(~serial)

## -----------------------------------------------------------
## 
## -----------------------------------------------------------
ems.pc.mean <- ems.pc %>% group_by(month,basin) %>% 
  summarize(depth.max=max(depth.mean),
            temp.mean=mean(temp.mean),
            do.percent.mean=min(do.percent.mean),
            do.ppm.mean=min(do.ppm.mean))
