##############################################################
##############################################################
##  EMS (Stewart et al.) manuscript
##
##  DIET COMPOSITION ANALYSIS SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get ems.diet data.frame
## ===========================================================
source("data_init.R")
ems.diet

## -----------------------------------------------------------
## Filter into each seasons and basin, remove empties
## -----------------------------------------------------------
diet.may.west <- filter(ems.diet,month=="May",basin=="Western",food.item!="Empty") %>% droplevels()
diet.may.cen <- filter(ems.diet,month=="May",basin=="Central",food.item!="Empty") %>% droplevels()
diet.may.east <- filter(ems.diet,month=="May",basin=="Eastern",food.item!="Empty") %>% droplevels()
diet.sept.west <- filter(ems.diet,month=="September",basin=="Western",food.item!="Empty") %>% droplevels()
diet.sept.cen <- filter(ems.diet,month=="September",basin=="Central",food.item!="Empty") %>% droplevels()
diet.sept.east <- filter(ems.diet,month=="September",basin=="Eastern",food.item!="Empty") %>% droplevels()

## -----------------------------------------------------------
## Create a list of prey types found for both seasons, uniquely combine, and order by functional group and alphabetically
## -----------------------------------------------------------
diet.may.west.list <- distinct(diet.may.west,food.item)
diet.may.cen.list <- distinct(diet.may.cen,food.item)
diet.may.east.list <- distinct(diet.may.east,food.item)
diet.sept.west.list <- distinct(diet.sept.west,food.item)
diet.sept.cen.list <- distinct(diet.sept.cen,food.item)
diet.sept.east.list <- distinct(diet.sept.east,food.item)
diet.list <- bind_rows(diet.may.west.list,diet.may.cen.list,diet.may.east.list,
                       diet.sept.west.list,diet.sept.cen.list,diet.sept.east.list) %>% 
  arrange(food.item) %>% 
  distinct(food.item) %>% 
  select(food.item)
diet.list <- as.character(diet.list$food.item)

## -----------------------------------------------------------
## Creat lists of fid's found for both seasons and basin
## -----------------------------------------------------------
fid.may.west.list <- unique(diet.may.west$fid)
fid.may.cen.list <- unique(diet.may.cen$fid)
fid.may.east.list <- unique(diet.may.east$fid)
fid.sept.west.list <- unique(diet.sept.west$fid)
fid.sept.cen.list <- unique(diet.sept.cen$fid)
fid.sept.east.list <- unique(diet.sept.east$fid)

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
diet.may.west.perc <- as.data.frame(do.call(cbind,lapply(fid.may.west.list,function(i) {
  fish <- filter(diet.may.west,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet.list,function(j) {
    round((sum(filter(fish,food.item == j)$biomass)/sum(fish$biomass))*100,2)
  })))
})))

## Calculate row means
diet.may.west.perc %<>% transform(mean = apply(diet.may.west.perc,1,mean),
                                  sd = apply(diet.may.west.perc,1,sd)) %>% 
  mutate(n=rowSums(diet.may.west.perc > 0),
         se=sd/(sqrt(n)))

## Add prey names to data frame
diet.may.west.perc %<>% transmute(percent.dry = mean,
                                  dry.se = se,
                                  prey.type = diet.list,
                                  month = "May",
                                  basin = "Western") 

## -----------------------------------------------------------
## Calculate the mean percent by dry weight for each prey taxa
## -----------------------------------------------------------
## Central
diet.may.cen.perc <- as.data.frame(do.call(cbind,lapply(fid.may.cen.list,function(i) {
  fish <- filter(diet.may.cen,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet.list,function(j) {
    round((sum(filter(fish,food.item == j)$biomass)/sum(fish$biomass))*100,2)
  })))
})))

## Calculate row means
diet.may.cen.perc %<>% transform(mean = apply(diet.may.cen.perc,1,mean),
                                 sd = apply(diet.may.cen.perc,1,sd)) %>% 
  mutate(n=rowSums(diet.may.cen.perc > 0),
         se=sd/(sqrt(n)))

## Add prey names to data frame
diet.may.cen.perc %<>% transmute(percent.dry = mean,
                                 dry.se = se,
                                 prey.type = diet.list,
                                 month = "May",
                                 basin = "Central") 

## -----------------------------------------------------------
## Calculate the mean percent by dry weight for each prey taxa
## -----------------------------------------------------------
## Eastern
diet.may.east.perc <- as.data.frame(do.call(cbind,lapply(fid.may.east.list,function(i) {
  fish <- filter(diet.may.east,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet.list,function(j) {
    round((sum(filter(fish,food.item == j)$biomass)/sum(fish$biomass))*100,2)
  })))
})))

## Calculate row means
diet.may.east.perc %<>% transform(mean = apply(diet.may.east.perc,1,mean),
                                  sd = apply(diet.may.east.perc,1,sd)) %>% 
  mutate(n=rowSums(diet.may.east.perc > 0),
         se=sd/(sqrt(n)))

## Add prey names to data frame
diet.may.east.perc %<>% transmute(percent.dry = mean,
                                  dry.se = se,
                                  prey.type = diet.list,
                                  month = "May",
                                  basin = "Eastern") 

## ===========================================================
## AUTUMN (Western and Eastern Basin)
## ===========================================================
## -----------------------------------------------------------
## Calculate the mean percent by dry weight for each prey taxa
## -----------------------------------------------------------
## Western
diet.sept.west.perc <- as.data.frame(do.call(cbind,lapply(fid.sept.west.list,function(i) {
  fish <- filter(diet.sept.west,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet.list,function(j) {
    round((sum(filter(fish,food.item == j)$biomass)/sum(fish$biomass))*100,1)
  })))
})))

## Calculate row means
diet.sept.west.perc %<>% transform(mean = apply(diet.sept.west.perc,1,mean),
                                   sd = apply(diet.sept.west.perc,1,sd)) %>% 
  mutate(n=rowSums(diet.sept.west.perc > 0),
         se=sd/(sqrt(n)))

## Add prey names to data frame
diet.sept.west.perc %<>% transmute(percent.dry = mean,
                                   dry.se = se,
                                   prey.type = diet.list,
                                   month = "September",
                                   basin = "Western") 

## -----------------------------------------------------------
## Calculate the mean percent by dry weight for each prey taxa
## -----------------------------------------------------------
## Central
diet.sept.cen.perc <- as.data.frame(do.call(cbind,lapply(fid.sept.cen.list,function(i) {
  fish <- filter(diet.sept.cen,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet.list,function(j) {
    round((sum(filter(fish,food.item == j)$biomass)/sum(fish$biomass))*100,1)
  })))
})))

## Calculate row means
diet.sept.cen.perc %<>% transform(mean = apply(diet.sept.cen.perc,1,mean),
                                  sd = apply(diet.sept.cen.perc,1,sd)) %>% 
  mutate(n=rowSums(diet.sept.cen.perc > 0),
         se=sd/(sqrt(n)))

## Add prey names to data frame
diet.sept.cen.perc %<>% transmute(percent.dry = mean,
                                  dry.se = se,
                                  prey.type = diet.list,
                                  month = "September",
                                  basin = "Central") 

## -----------------------------------------------------------
## Calculate the mean percent by dry weight for each prey taxa
## -----------------------------------------------------------
## Eastern
diet.sept.east.perc <- as.data.frame(do.call(cbind,lapply(fid.sept.east.list,function(i) {
  fish <- filter(diet.sept.east,fid == i)
  mean <- as.data.frame(do.call(rbind,lapply(diet.list,function(j) {
    round((sum(filter(fish,food.item == j)$biomass)/sum(fish$biomass))*100,1)
  })))
})))

## Calculate row means
diet.sept.east.perc %<>% transform(mean = apply(diet.sept.east.perc,1,mean),
                                   sd = apply(diet.sept.east.perc,1,sd)) %>% 
  mutate(n=rowSums(diet.sept.east.perc > 0),
         se=sd/(sqrt(n)))

## Add prey names to data frame
diet.sept.east.perc %<>% transmute(percent.dry = mean,
                                   dry.se = se,
                                   prey.type = diet.list,
                                   month = "September",
                                   basin = "Eastern") 

## -----------------------------------------------------------
## Combine into a final data frame
## -----------------------------------------------------------
ems.diet.perc <- data.frame(rbind(diet.may.west.perc,diet.may.cen.perc,diet.may.east.perc,
                              diet.sept.west.perc,diet.sept.cen.perc,diet.sept.east.perc)) %>% 
  mutate(basin=factor(basin,levels = c('Western','Central','Eastern'),ordered = TRUE))

## Clean up environment
rm(diet.may.west.perc,diet.may.cen.perc,diet.may.east.perc,diet.sept.west.perc,diet.sept.cen.perc,diet.sept.east.perc,
   diet.may.west,diet.may.cen,diet.may.east,diet.sept.west,diet.sept.cen,diet.sept.east,
   diet.may.west.list,diet.may.cen.list,diet.may.east.list,diet.sept.west.list,diet.sept.cen.list,diet.sept.east.list,
   fid.may.west.list,fid.may.cen.list,fid.may.east.list,fid.sept.west.list,fid.sept.cen.list,fid.sept.east.list)

## -----------------------------------------------------------
## abbreviate taxon
## -----------------------------------------------------------
ems.diet.perc$prey.type <- gsub('Calanoida','CA',ems.diet.perc$prey.type)
ems.diet.perc$prey.type <- gsub('Cyclopoida','CY',ems.diet.perc$prey.type)
ems.diet.perc$prey.type <- gsub('Chironomid Larvae','CL',ems.diet.perc$prey.type)
ems.diet.perc$prey.type <- gsub('Bosminidae','BO',ems.diet.perc$prey.type)
ems.diet.perc$prey.type <- gsub('Leptodoridae','LE',ems.diet.perc$prey.type)
ems.diet.perc$prey.type <- gsub('Nematoda','NE',ems.diet.perc$prey.type)
ems.diet.perc$prey.type <- gsub('Chironomid Pupae','CP',ems.diet.perc$prey.type)
ems.diet.perc$prey.type <- gsub('Daphnidae','DA',ems.diet.perc$prey.type)
ems.diet.perc$prey.type <- gsub('Oligochaeta','OL',ems.diet.perc$prey.type)

ems.diet.perc %<>% arrange(month,basin,prey.type) %>% 
  filter(prey.type %in% c('BO','CA','CL','CY','DA','LE'))

##############################################################
##############################################################
##
##  VISUALIZATIONS
##
##############################################################
##############################################################
may.diet <- ggplot(filter(ems.diet.perc,month=='May'),aes(prey.type,percent.dry,fill=basin)) +
  geom_errorbar(aes(x=prey.type,ymin=0,ymax=percent.dry+dry.se),
                width=0.25,
                position = position_dodge(.75)) +
  scale_fill_grey(start=0.2,end=0.7) +
  scale_y_continuous(limits = c(0,100),expand=c(0,0)) +
  geom_bar(stat="identity",position="dodge",width = 0.75,colour="black") +
  labs(title="May") +
  theme(axis.text.y=element_text(size=15),axis.text.x=element_blank(),axis.line.x=element_line(),axis.line.y=element_line(),
        legend.position=c(0.9,0.9),legend.text=element_text(size=15),legend.title=element_blank(),axis.title=element_blank(),
        panel.background=element_blank(),plot.title=element_text(size=20),
        plot.margin=unit(c(4,2,8,4),"mm"),axis.ticks.length=unit(1.75,'mm'))

sept.diet <- ggplot(filter(ems.diet.perc,month=='September'),aes(prey.type,percent.dry,fill=basin)) +
  geom_errorbar(aes(x=prey.type,ymin=0,ymax=percent.dry+dry.se),
                width=0.25,
                position = position_dodge(.75)) +
  scale_fill_grey(start=0.2,end=0.7) +
  scale_y_continuous(limits = c(0,100),expand=c(0,0)) +
  geom_bar(stat="identity",position="dodge",width = 0.75,colour="black") +
  labs(title="September") +
  theme(axis.text=element_text(size=15),axis.line.x=element_line(),axis.line.y=element_line(),
        legend.position='top',legend.title=element_text(),axis.title=element_blank(),
        panel.background=element_blank(),plot.title=element_text(size=20),
        plot.margin=unit(c(4,2,0,4),"mm"),axis.ticks.length=unit(1.75,'mm'))

## -----------------------------------------------------------
## Save the plot as a figure (comment out line 273 and 289 until you are ready to save)
## -----------------------------------------------------------
png("figs/percent_dry_weight_bar.PNG",width=7,height=7,units="in",family="Times",res=300)

## -----------------------------------------------------------
## Put plots into a matrix
## -----------------------------------------------------------
grid.arrange(arrangeGrob(may.diet,
                         sept.diet + theme(legend.position="none"),
                         ncol=1,
                         nrow=2,
                         left=textGrob("Diet Composition (% Dry Weight)",y=unit(80,'mm'),rot=90,gp=gpar(fontsize=20)),
                         bottom=textGrob("Prey Type",y=unit(0,'mm'),x=unit(91.5,'mm'),gp=gpar(fontsize=20))),
             heights=c(8,1))

## -----------------------------------------------------------
## Close the device to make the actual PNG file
## -----------------------------------------------------------
dev.off()
