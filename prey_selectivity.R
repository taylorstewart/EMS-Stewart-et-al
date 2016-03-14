##############################################################
##############################################################
##  EMS (Stewart et al.) manuscript
##
##  PREY SELECTIVITY ANALYSIS SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get ems_cal data.frame
## ===========================================================
source('data_init.R')
str(ems.diet)
str(ems.zoop)
str(ems.benthos)

## -----------------------------------------------------------
## Remove stomachs with no diet contents
## -----------------------------------------------------------
ems.diet %<>% filter(food.item != 'Empty')

## -----------------------------------------------------------
## Combine benthos and zooplankton prey
## -----------------------------------------------------------
ems.prey <- bind_rows(ems.zoop,ems.benthos) %>% 
  group_by(month,region,basin,ems.taxa) %>% 
  summarize(biomass=sum(biomass))

## -----------------------------------------------------------
## Create unique lists of serials sampled, FID, diet prey, and available prey
## -----------------------------------------------------------
serial.list <- as.character(unique(ems.diet$serial))
region.list <- as.character(unique(ems.prey$region))
basin.list <- as.character(unique(ems.prey$basin))
month.list <-  c('May','September')
fid.list <- unique(ems.diet$fid)
diet.list <- unique(ems.diet$food.item)
prey.list <- unique(ems.prey$ems.taxa)

## -----------------------------------------------------------
## Filter prey taxa to only indentified taxa
## -----------------------------------------------------------
ems.prey %<>% filter(ems.taxa %in% diet.list, month %in% month.list)

## -----------------------------------------------------------
## Create a loop function to add zeros for all prey taxa missing from each region
## -----------------------------------------------------------
## Apply loop function
diet.missing <- data.frame(do.call(rbind,lapply(fid.list,function(i) {
  ## Filter catch by each serial
  fid2 <- ems.diet %>% filter(fid==i)
  serial <- unique(as.character(fid2$serial))
  region <- unique(as.character(fid2$region))
  ## True/false output if life stages does not exist (zero value life stages)
  pl <- diet.list[!diet.list %in% fid2$food.item]
  ## Determine the number of life stages to be added
  n <- length(pl)
  ## Create data frame with all zero value life stages, repeat by 'n'
  tmp <- data.frame(fid=rep(i,n),serial=serial,region=region,food.item=pl,biomass=rep(0,n))
})))

## -----------------------------------------------------------
## Join zero data by serial to fill in other variables
## -----------------------------------------------------------
ems.diet.effort <- ems.diet %>% distinct(serial,month,region,basin) %>% select(serial,month,region,basin)
ems.diet.zero <- left_join(diet.missing,ems.diet.effort)
ems.diet.all <- bind_rows(ems.diet,ems.diet.zero) %>% arrange(fid)

## -----------------------------------------------------------
## Create a loop function to add zeros for all prey taxa missing from each serial
## -----------------------------------------------------------
## Apply loop function
envir.missing <-  data.frame(do.call(rbind,lapply(month.list,function(g){
  month <- ems.prey %>% filter(month == g)
  tmp <- data.frame(do.call(rbind,lapply(region.list,function(i) {
    ## Filter catch by each region
    region2 <- month %>% filter(region==i)
      ## True/false output if life stages does not exist (zero value life stages)
      pl <- diet.list[!diet.list %in% region2$ems.taxa]
      ## Determine the number of life stages to be added
      n <- length(pl)
      ## Create data frame with all zero value life stages, repeat by 'n'
      data.frame(month=rep(g,n),region=rep(i,n),ems.taxa=pl,biomass=rep(0,n))
    })))
})))

## -----------------------------------------------------------
## Join zero data by serial to fill in other variables
## -----------------------------------------------------------
ems.prey.effort <- ems.prey %>% distinct(month,region,basin) %>% select(month,region,basin)
ems.prey.zero <- left_join(envir.missing,ems.prey.effort)
ems.prey.all <- bind_rows(ems.prey,ems.prey.zero) %>% arrange(month,region)

## -----------------------------------------------------------
## Calculate the proportion of prey available in the environment
## -----------------------------------------------------------
diet.prop <- as.data.frame(do.call(rbind,lapply(fid.list,function(i) {
  indiv <- ems.diet.all %>% filter(fid == i)
  prop <- as.data.frame(do.call(rbind,lapply(diet.list,function(j) {
    round((filter(indiv,food.item == j)$biomass)/sum(indiv$biomass),4)
  })))
  data.frame(fid=rep(i,9),food.item=diet.list,diet.prop=prop)
}))) %>% 
  mutate(diet.prop=V1) %>% select(-V1)

ems.diet.prop <- left_join(ems.diet,diet.prop)

## -----------------------------------------------------------
## Calculate the proportion of prey available in the environment
## -----------------------------------------------------------
envir.may.prop <- as.data.frame(do.call(rbind,lapply(region.list,function(i) {
  indiv <- ems.prey.all %>% filter(region == i,month == 'May')
  prop <- as.data.frame(do.call(rbind,lapply(diet.list,function(j) {
    round((filter(indiv,ems.taxa == j)$biomass)/sum(indiv$biomass),4)
  })))
  data.frame(month=rep('May',9),region=rep(i,9),food.item=diet.list,envir.prop=prop)
}))) %>% 
  mutate(envir.prop=V1) %>% select(-V1)

envir.sept.prop <- as.data.frame(do.call(rbind,lapply(region.list,function(i) {
  indiv <- ems.prey.all %>% filter(region == i,month == 'September')
  prop <- as.data.frame(do.call(rbind,lapply(diet.list,function(j) {
    round((filter(indiv,ems.taxa == j)$biomass)/sum(indiv$biomass),4)
  })))
  data.frame(month=rep('September',9),region=rep(i,9),food.item=diet.list,envir.prop=prop)
}))) %>% 
  mutate(envir.prop=V1) %>% select(-V1)

envir.prop.all <- bind_rows(envir.may.prop,envir.sept.prop)
ems.prey.select <- left_join(ems.diet.prop,envir.prop.all)

## -----------------------------------------------------------
## Divide proportion of prey available in the environment by proportion of prey in diet
## -----------------------------------------------------------
ems.prey.select %<>% mutate('diet/envir' = diet.prop/envir.prop)

## -----------------------------------------------------------
## Calculate alpha ((r/p)/sum(r/p))
## -----------------------------------------------------------
alpha <- data.frame(do.call(rbind,lapply(fid.list,function(k) {
  indiv <- ems.prey.select %>% filter(fid == k)
  serial <- unique(as.character(indiv$serial))
  month <- unique(as.character(indiv$month))
  basin <- unique(as.character(indiv$basin))
  taxa <- as.character(indiv$food.item)
  n <- nrow(indiv)
    tmp <- data.frame(do.call(rbind,lapply(1:n,function(p) {
      round(indiv[p,12]/sum(indiv[,12]),5)
    })))
  data.frame(fid=rep(k,n),serial,month,basin,taxa,alpha=tmp$envir.prop)
})))
## Remove NAs and summarize mean alpha by month, basin, and taxa
alpha %<>% filter(!is.na(alpha)) %>% 
  group_by(month,basin,taxa) %>% 
  summarize(alpha=mean(alpha)) %>% 
  arrange(desc(taxa))

## -----------------------------------------------------------
## Clean up environment
## -----------------------------------------------------------
rm(diet.missing,diet.prop,ems.diet.all,ems.diet.prop,ems.diet.zero,ems.diet.effort,
   ems.prey,ems.prey.all,ems.prey.select,ems.prey.zero,ems.prey.effort,envir.prop.all,
   envir.sept.prop,envir.may.prop,envir.missing,fid.list,prey.list,region.list,
   serial.list)

## -----------------------------------------------------------
## Rearrange data frame for paired t-test
## -----------------------------------------------------------
## Add zero values for consitent pairs among months and basins
## Apply first loop function
output1 <- data.frame(do.call(rbind,lapply(month.list,function(i) {
  ## Filter catch by each serial
  select.filt <- alpha %>% filter(month==i)
  ## Apply second loop function
  tmp <- lapply(basin.list,function(j) {
    ## Filter by each species and drop all other factor levels
    select.filt2 <- select.filt %>% filter(basin==j) %>% 
      droplevels()
    ## True/false output if life stages does not exist (zero value life stages)
    taxa <- diet.list[!diet.list %in% select.filt2$taxa]
    ## Determine the number of life stages to be added
    n <- length(taxa)
    ## Create data frame with all zero value life stages, repeat by 'n'
    tmp <- data.frame(month=rep(i,n),basin=rep(j,n),taxa=taxa,alpha=rep(0,n))
  })
  ## Bind list into data frame
  tmp2 <- data.frame(do.call(rbind,tmp))
  
  ## Bind all serials
  select.all <- if((exists('select.all'))==F) {
    tmp2 } else {
      rbind(select.all,tmp2)
    }
  select.all
})))
## End double loop

## Bind all zero values with non-zero values
final.alpha <- bind_rows(alpha,output1) %>% 
  arrange(month,basin,taxa) %>% 
  mutate(basin=factor(basin,levels = c('Western','Central','Eastern'),ordered = TRUE))
## remove extra objects
rm(alpha,output1,basin.list,diet.list,month.list)

## -----------------------------------------------------------
## abbreviate taxon
## -----------------------------------------------------------
final.alpha$taxa <- gsub('Calanoida','CA',final.alpha$taxa)
final.alpha$taxa <- gsub('Cyclopoida','CY',final.alpha$taxa)
final.alpha$taxa <- gsub('Chironomid Larvae','CL',final.alpha$taxa)
final.alpha$taxa <- gsub('Bosminidae','BO',final.alpha$taxa)
final.alpha$taxa <- gsub('Leptodoridae','LE',final.alpha$taxa)
final.alpha$taxa <- gsub('Nematoda','NE',final.alpha$taxa)
final.alpha$taxa <- gsub('Chironomid Pupae','CP',final.alpha$taxa)
final.alpha$taxa <- gsub('Daphnidae','DA',final.alpha$taxa)
final.alpha$taxa <- gsub('Oligochaeta','OL',final.alpha$taxa)

final.alpha %<>% arrange(month,basin,taxa) %>% 
  filter(taxa %in% c('BO','CA','CL','CY','DA','LE'))

## -----------------------------------------------------------
## season paired t-test
## -----------------------------------------------------------
t.test(filter(final.alpha,basin=='Western',month=='May')$alpha,filter(final.alpha,basin=='Western',month=='September')$alpha,paired=T)
t.test(filter(final.alpha,basin=='Central',month=='May')$alpha,filter(final.alpha,basin=='Central',month=='September')$alpha,paired=T)
t.test(filter(final.alpha,basin=='Eastern',month=='May')$alpha,filter(final.alpha,basin=='Eastern',month=='September')$alpha,paired=T)

## -----------------------------------------------------------
## basin post-hoc pairwise t-test
## -----------------------------------------------------------
pairwise.t.test(filter(final.alpha,month=='May')$alpha,filter(final.alpha,month=='May')$basin,paired=T,p.adjust.method='bonferroni')
pairwise.t.test(filter(final.alpha,month=='September')$alpha,filter(final.alpha,month=='September')$basin,paired=T,p.adjust.method='bonferroni')
pairwise.t.test(final.alpha$alpha,final.alpha$basin,paired=T,p.adjust.method='bonferroni')

## -----------------------------------------------------------
## visualization
## -----------------------------------------------------------
may.west <- ggplot(filter(final.alpha,month=='May',basin=='Western'),aes(taxa,alpha)) +
  geom_bar(stat='identity') +
  labs(x='',y='May',title='Western Basin\n') +
  scale_y_continuous(limit=c(0,1),expand=c(0,0)) +
  theme(axis.line.y=element_line(),axis.line.x=element_line(),axis.ticks.length=unit(1.5,'mm'),
        axis.text.y=element_text(size=15,vjust=0.5,hjust=0),axis.text.x=element_blank(),plot.title=element_text(size=16),
        axis.title=element_text(size=19),panel.background=element_blank(),plot.margin=unit(c(5,0,-6,3),'mm'))

may.cen <- ggplot(filter(final.alpha,month=='May',basin=='Central'),aes(taxa,alpha)) +
  geom_bar(stat='identity') +
  labs(x='',y='',title='Central Basin\n') +
  scale_y_continuous(limit=c(0,1),expand=c(0,0)) +
  theme(axis.line.y=element_line(),axis.line.x=element_line(),plot.title=element_text(size=16),
        axis.text.y=element_blank(),axis.text.x=element_blank(),panel.background=element_blank(),
        axis.ticks.length=unit(1.5,'mm'),plot.margin=unit(c(5,8,-4,8),'mm'))

may.east <- ggplot(filter(final.alpha,month=='May',basin=='Eastern'),aes(taxa,alpha)) +
  geom_bar(stat='identity') +
  labs(x='',y='',title='Eastern Basin\n') +
  scale_y_continuous(limit=c(0,1),expand=c(0,0)) +
  theme(axis.line.y=element_line(),axis.line.x=element_line(),plot.title=element_text(size=16),
        axis.text.y=element_blank(),axis.text.x=element_blank(),panel.background=element_blank(),
        axis.ticks.length=unit(1.5,'mm'),plot.margin=unit(c(5,16,-4,0),'mm'))

sept.west <- ggplot(filter(final.alpha,month=='September',basin=='Western'),aes(taxa,alpha)) +
  geom_bar(stat='identity') +
  labs(x='',y='September',title='') +
  scale_y_continuous(limit=c(0,1),expand=c(0,0)) +
  theme(axis.line.y=element_line(),axis.line.x=element_line(),
        axis.text.x=element_text(size=13),axis.text.y=element_text(size=15),axis.ticks.length=unit(1.5,'mm'),
        axis.title=element_text(size=19),panel.background=element_blank(),plot.margin=unit(c(11,0,-17,3),'mm'))

sept.cen <- ggplot(filter(final.alpha,month=='September',basin=='Central'),aes(taxa,alpha)) +
  geom_bar(stat='identity') +
  labs(x='',y='',title='') +
  scale_y_continuous(limit=c(0,1),expand=c(0,0)) +
  theme(axis.line.y=element_line(),axis.line.x=element_line(),
        axis.text.x=element_text(size=13),axis.ticks.length=unit(1.5,'mm'),
        axis.text.y=element_blank(),panel.background=element_blank(),plot.margin=unit(c(11,8,-15,8),'mm'))

sept.east <- ggplot(filter(final.alpha,month=='September',basin=='Eastern'),aes(taxa,alpha)) +
  geom_bar(stat='identity') +
  labs(x='',y='',title='') +
  scale_y_continuous(limit=c(0,1),expand=c(0,0)) +
  theme(axis.line.y=element_line(),axis.line.x=element_line(),
        axis.text.x=element_text(size=13),axis.ticks.length=unit(1.5,'mm'),
        axis.text.y=element_blank(),panel.background=element_blank(),plot.margin=unit(c(11,15,-15,1),'mm'))

## -----------------------------------------------------------
## Put plots into a matrix
## -----------------------------------------------------------
grid.arrange(arrangeGrob(may.west,
                         may.cen,
                         may.east,
                         sept.west,
                         sept.cen,
                         sept.east,
                         ncol=3,
                         nrow=2,
                         left=textGrob("Selectivity Index (W')",y=unit(90,'mm'),rot=90,gp=gpar(fontsize=22)),
                         bottom=textGrob('Prey Type',y=unit(-15,'mm'),x=unit(131.5,'mm'),gp=gpar(fontsize=22))),
             heights=c(8,1))
