##############################################################
##############################################################
##  EMS (Taylor Stewart et al.) manuscript
##
##  DIET ANALYSIS SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get ems_diet data.frame
## ===========================================================
source("data_init.R")
ems_diet

## -----------------------------------------------------------
## Filter into each seasons and basin, remove empties
## -----------------------------------------------------------
diet_may_west <- filter(ems_diet,month=="May",basin=="Western",food_item!="Empty") %>% droplevels()
diet_may_cen <- filter(ems_diet,month=="May",basin=="Central",food_item!="Empty") %>% droplevels()
diet_may_east <- filter(ems_diet,month=="May",basin=="Eastern",food_item!="Empty") %>% droplevels()
diet_sept_west <- filter(ems_diet,month=="September",basin=="Western",food_item!="Empty") %>% droplevels()
diet_sept_cen <- filter(ems_diet,month=="September",basin=="Central",food_item!="Empty") %>% droplevels()
diet_sept_east <- filter(ems_diet,month=="September",basin=="Eastern",food_item!="Empty") %>% droplevels()

## -----------------------------------------------------------
## Creat lists of prey types and fid's found for both seasons and basin
## -----------------------------------------------------------
diet_may_west_list <- unique(diet_may_west$food_item)
diet_may_cen_list <- unique(diet_may_cen$food_item)
diet_may_east_list <- unique(diet_may_east$food_item)
diet_sept_west_list <- unique(diet_sept_west$food_item)
diet_sept_cen_list <- unique(diet_sept_cen$food_item)
diet_sept_east_list <- unique(diet_sept_east$food_item)
fid_may_west_list <- unique(diet_may_west$fid)
fid_may_cen_list <- unique(diet_may_cen$fid)
fid_may_east_list <- unique(diet_may_east$fid)
fid_sept_west_list <- unique(diet_sept_west$fid)
fid_sept_cen_list <- unique(diet_sept_cen$fid)
fid_sept_east_list <- unique(diet_sept_east$fid)

##############################################################
##############################################################
##
##  PERCENT OCCURRENCE
##
##############################################################
##############################################################
## ===========================================================
## SPRING (Western, Central and Eastern Basin)
## ===========================================================
## -----------------------------------------------------------
## Calculate the number of fish found with each prey taxa
## -----------------------------------------------------------
## Western
diet_may_west_n <- as.data.frame(do.call(rbind,lapply(diet_may_west_list,function(i) {
  diet_may_west %<>% filter(food_item == i)
  nrow(diet_may_west)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
diet_may_west_freq <- as.data.frame(do.call(rbind,lapply(1:nrow(diet_may_west_n),function(j) {
  round(as.numeric(((diet_may_west_n[j,1])/length(unique(diet_may_west)))*100),1)
})))
## Add prey names to data frame
diet_may_west_freq %<>% transmute(prey_type = diet_may_west_list,
                                  percent_occur = V1,
                                  month = "May",
                                  basin = "Western")

## -----------------------------------------------------------
## Calculate the number of fish found with each prey taxa
## -----------------------------------------------------------
## Central
diet_may_cen_n <- as.data.frame(do.call(rbind,lapply(diet_may_cen_list,function(i) {
  diet_may_cen %<>% filter(food_item == i)
  nrow(diet_may_cen)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
diet_may_cen_freq <- as.data.frame(do.call(rbind,lapply(1:nrow(diet_may_cen_n),function(j) {
  round(as.numeric(((diet_may_cen_n[j,1])/length(unique(diet_may_cen)))*100),1)
})))
## Add prey names to data frame
diet_may_cen_freq %<>% transmute(prey_type = diet_may_cen_list,
                                  percent_occur = V1,
                                  month = "May",
                                  basin = "Central")

## -----------------------------------------------------------
## Calculate the number of fish found with each prey taxa
## -----------------------------------------------------------
## Eastern
diet_may_east_n <- as.data.frame(do.call(rbind,lapply(diet_may_east_list,function(i) {
  diet_may_east %<>% filter(food_item == i)
  nrow(diet_may_east)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
diet_may_east_freq <- as.data.frame(do.call(rbind,lapply(1:nrow(diet_may_east_n),function(j) {
  round(as.numeric(((diet_may_east_n[j,1])/length(unique(diet_may_east)))*100),1)
})))
## Add prey names to data frame
diet_may_east_freq %<>% transmute(prey_type = diet_may_east_list,
                                  percent_occur = V1,
                                  month = "May",
                                  basin = "Eastern")

## ===========================================================
## AUTUMN (Western and Eastern Basin)
## ===========================================================
## -----------------------------------------------------------
## Calculate the number of fish found with each prey taxa
## -----------------------------------------------------------
## Western
diet_sept_west_n <- as.data.frame(do.call(rbind,lapply(diet_sept_west_list,function(i) {
  diet_sept_west %<>% filter(food_item == i)
  nrow(diet_sept_west)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
diet_sept_west_freq <- as.data.frame(do.call(rbind,lapply(1:nrow(diet_sept_west_n),function(j) {
  round(as.numeric(((diet_sept_west_n[j,1])/length(unique(diet_sept_west)))*100),1)
})))
## Add prey names to data frame
diet_sept_west_freq %<>% transmute(prey_type = diet_sept_west_list,
                                   percent_occur = V1,
                                   month = "September",
                                   basin = "Western")

## -----------------------------------------------------------
## Calculate the number of fish found with each prey taxa
## -----------------------------------------------------------
## Central
diet_sept_cen_n <- as.data.frame(do.call(rbind,lapply(diet_sept_cen_list,function(i) {
  diet_sept_cen %<>% filter(food_item == i)
  nrow(diet_sept_cen)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
diet_sept_cen_freq <- as.data.frame(do.call(rbind,lapply(1:nrow(diet_sept_cen_n),function(j) {
  round(as.numeric(((diet_sept_cen_n[j,1])/length(unique(diet_sept_cen)))*100),1)
})))
## Add prey names to data frame
diet_sept_cen_freq %<>% transmute(prey_type = diet_sept_cen_list,
                                   percent_occur = V1,
                                   month = "September",
                                   basin = "Central")

## -----------------------------------------------------------
## Calculate the number of fish found with each prey taxa
## -----------------------------------------------------------
## Eastern
diet_sept_east_n <- as.data.frame(do.call(rbind,lapply(diet_sept_east_list,function(i) {
  diet_sept_east %<>% filter(food_item == i)
  nrow(diet_sept_east)
})))

## Calculate the frequency of occurency (percent) for each prey taxa
diet_sept_east_freq <- as.data.frame(do.call(rbind,lapply(1:nrow(diet_sept_east_n),function(j) {
  round(as.numeric(((diet_sept_east_n[j,1])/length(unique(diet_sept_east)))*100),1)
})))
## Add prey names to data frame
diet_sept_east_freq %<>% transmute(prey_type = diet_sept_east_list,
                                   percent_occur = V1,
                                   month = "September",
                                   basin = "Eastern")

## -----------------------------------------------------------
## Combine into a final data frame
## -----------------------------------------------------------
diet_freq <- data.frame(rbind(diet_may_west_freq,diet_may_cen_freq,diet_may_east_freq,
                              diet_sept_west_freq,diet_sept_cen_freq,diet_sept_east_freq))

## Clean up environment
rm(diet_may_east_n,diet_may_cen_n,diet_may_west_n,diet_sept_east_n,diet_sept_cen_n,diet_sept_west_n,
   diet_may_west_freq,diet_may_cen_freq,diet_may_east_freq,diet_sept_west_freq,diet_sept_cen_freq,diet_sept_east_freq)

##############################################################
##############################################################
##
##  PERCENT DRY WEIGHT
##
##############################################################
##############################################################
## ===========================================================
## SPRING (Western and Eastern Basin)
## ===========================================================
## -----------------------------------------------------------
## Calculate the mean percent by dry weight for each prey taxa
## -----------------------------------------------------------
## Western
diet_may_west_perc <- as.data.frame(do.call(cbind,lapply(fid_may_west_list,function(i) {
  fish <- filter(diet_may_west,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet_may_west_list,function(j) {
    round((sum(filter(fish,food_item == j)$mean_biomass_mg)/sum(fish$mean_biomass_mg))*100,1)
  })))
})))

## Calculate row means
diet_may_west_perc %<>% transform(mean = apply(diet_may_west_perc,1,mean))

## Add prey names to data frame
diet_may_west_perc %<>% transmute(percent_dry = mean,
                                  prey_type = diet_may_west_list,
                                  month = "May",
                                  basin = "Western") 

## -----------------------------------------------------------
## Calculate the mean percent by dry weight for each prey taxa
## -----------------------------------------------------------
## Central
diet_may_cen_perc <- as.data.frame(do.call(cbind,lapply(fid_may_cen_list,function(i) {
  fish <- filter(diet_may_cen,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet_may_cen_list,function(j) {
    round((sum(filter(fish,food_item == j)$mean_biomass_mg)/sum(fish$mean_biomass_mg))*100,1)
  })))
})))

## Calculate row means
diet_may_cen_perc %<>% transform(mean = apply(diet_may_cen_perc,1,mean))

## Add prey names to data frame
diet_may_cen_perc %<>% transmute(percent_dry = mean,
                                  prey_type = diet_may_cen_list,
                                  month = "May",
                                  basin = "Central") 

## -----------------------------------------------------------
## Calculate the mean percent by dry weight for each prey taxa
## -----------------------------------------------------------
## Eastern
diet_may_east_perc <- as.data.frame(do.call(cbind,lapply(fid_may_east_list,function(i) {
  fish <- filter(diet_may_east,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet_may_east_list,function(j) {
    round((sum(filter(fish,food_item == j)$mean_biomass_mg)/sum(fish$mean_biomass_mg))*100,1)
  })))
})))

## Calculate row means
diet_may_east_perc %<>% transform(mean = apply(diet_may_east_perc,1,mean))

## Add prey names to data frame
diet_may_east_perc %<>% transmute(percent_dry = mean,
                                  prey_type = diet_may_east_list,
                                  month = "May",
                                  basin = "Eastern") 

## ===========================================================
## AUTUMN (Western and Eastern Basin)
## ===========================================================
## -----------------------------------------------------------
## Calculate the mean percent by dry weight for each prey taxa
## -----------------------------------------------------------
## Western
diet_sept_west_perc <- as.data.frame(do.call(cbind,lapply(fid_sept_west_list,function(i) {
  fish <- filter(diet_sept_west,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet_sept_west_list,function(j) {
    round((sum(filter(fish,food_item == j)$mean_biomass_mg)/sum(fish$mean_biomass_mg))*100,1)
  })))
})))

## Calculate row means
diet_sept_west_perc %<>% transform(mean = apply(diet_sept_west_perc,1,mean))

## Add prey names to data frame
diet_sept_west_perc %<>% transmute(percent_dry = mean,
                                  prey_type = diet_sept_west_list,
                                  month = "September",
                                  basin = "Western") 

## -----------------------------------------------------------
## Calculate the mean percent by dry weight for each prey taxa
## -----------------------------------------------------------
## Central
diet_sept_cen_perc <- as.data.frame(do.call(cbind,lapply(fid_sept_cen_list,function(i) {
  fish <- filter(diet_sept_cen,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet_sept_cen_list,function(j) {
    round((sum(filter(fish,food_item == j)$mean_biomass_mg)/sum(fish$mean_biomass_mg))*100,1)
  })))
})))

## Calculate row means
diet_sept_cen_perc %<>% transform(mean = apply(diet_sept_cen_perc,1,mean))

## Add prey names to data frame
diet_sept_cen_perc %<>% transmute(percent_dry = mean,
                                   prey_type = diet_sept_cen_list,
                                   month = "September",
                                   basin = "Central") 

## -----------------------------------------------------------
## Calculate the mean percent by dry weight for each prey taxa
## -----------------------------------------------------------
## Eastern
diet_sept_east_perc <- as.data.frame(do.call(cbind,lapply(fid_sept_east_list,function(i) {
  fish <- filter(diet_sept_east,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet_sept_east_list,function(j) {
    round((sum(filter(fish,food_item == j)$mean_biomass_mg)/sum(fish$mean_biomass_mg))*100,1)
  })))
})))

## Calculate row means
diet_sept_east_perc %<>% transform(mean = apply(diet_sept_east_perc,1,mean))

## Add prey names to data frame
diet_sept_east_perc %<>% transmute(percent_dry = mean,
                                  prey_type = diet_sept_east_list,
                                  month = "September",
                                  basin = "Eastern") 

## -----------------------------------------------------------
## Combine into a final data frame
## -----------------------------------------------------------
diet_perc <- data.frame(rbind(diet_may_west_perc,diet_may_cen_perc,diet_may_east_perc,
                              diet_sept_west_perc,diet_sept_cen_perc,diet_sept_east_perc))

## Clean up environment
rm(diet_may_west_perc,diet_may_cen_perc,diet_may_east_perc,diet_sept_west_perc,diet_sept_cen_perc,diet_sept_east_perc)

##############################################################
##############################################################
##
##  JOIN PERCENT OCCURENCE AND PERCENT DRY WEIGHT
##
##############################################################
##############################################################
ems_diet_summary <- left_join(diet_freq,diet_perc) %>% 
  select(month,basin,prey_type,percent_occur,percent_dry) %>% 
  mutate(basin=factor(basin,levels = c('Western','Central','Eastern'),ordered = TRUE))

## Clean up environment
rm(diet_may_west,diet_may_cen,diet_may_east,diet_sept_west,diet_sept_cen,diet_sept_east,diet_freq,diet_perc,
   diet_may_west_list,diet_may_cen_list,diet_may_east_list,diet_sept_west_list,diet_sept_cen_list,diet_sept_east_list,
   fid_may_west_list,fid_may_cen_list,fid_may_east_list,fid_sept_west_list,fid_sept_cen_list,fid_sept_east_list)

##############################################################
##############################################################
##
##  VISUALIZATIONS
##
##############################################################
##############################################################
## -----------------------------------------------------------
## 
## -----------------------------------------------------------
ggplot(ems_diet_summary,aes(prey_type,percent_dry)) +
  geom_point(aes(colour=month)) +
  theme_bw() +
  facet_wrap(~basin)
