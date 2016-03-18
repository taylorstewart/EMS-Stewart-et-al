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

## -----------------------------------------------------------
## Remove diets with no contents
## -----------------------------------------------------------
ems.diet %<>% filter(food.item != 'Empty')

## -----------------------------------------------------------
## Create unique lists of serials sampled, FID, diet prey, and available prey
## -----------------------------------------------------------
month.list <-  c('May','September')
basin.list <- as.character(unique(ems.diet$basin))
diet.list <- unique(ems.diet$food.item)

## -----------------------------------------------------------
## Summarize by month, basin, and taxa to calculate mean prey length
## -----------------------------------------------------------
ems.diet.mean <- ems.diet %>% group_by(month,basin,food.item) %>% 
  summarize(mean.prey.size = mean(mean.size),
            sd=sd(mean.size),
            n=n()) %>% 
  mutate(se=sd/(sqrt(n))) %>% 
  select(-sd,-n)

## -----------------------------------------------------------
## Rearrange data frame for paired t-test
## -----------------------------------------------------------
## Add zero values for consitent pairs among months and basins
## Apply first loop function
output1 <- data.frame(do.call(rbind,lapply(month.list,function(i) {
  ## Filter catch by each serial
  select.filt <- ems.diet %>% filter(month==i)
  ## Apply second loop function
  tmp <- lapply(basin.list,function(j) {
    ## Filter by each species and drop all other factor levels
    select.filt2 <- select.filt %>% filter(basin==j) %>% 
      droplevels()
    ## True/false output if life stages does not exist (zero value life stages)
    taxa <- diet.list[!diet.list %in% select.filt2$food.item]
    ## Determine the number of life stages to be added
    n <- length(taxa)
    ## Create data frame with all zero value life stages, repeat by "n"
    tmp <- data.frame(month=rep(i,n),basin=rep(j,n),food.item=taxa,mean.prey.size=rep(0,n),se=rep(0,n))
  })
  ## Bind list into data frame
  tmp2 <- data.frame(do.call(rbind,tmp))
  
  ## Bind all serials
  select.all <- if((exists("select.all"))==F) {
    tmp2 } else {
      rbind(select.all,tmp2)
    }
  select.all
})))
## End double loop

## Bind all zero values with non-zero values
ems.diet.mean.all <- bind_rows(ems.diet.mean,output1)

## -----------------------------------------------------------
## abbreviate taxon
## -----------------------------------------------------------
ems.diet.mean.all$food.item <- gsub('Calanoida','CA',ems.diet.mean.all$food.item)
ems.diet.mean.all$food.item <- gsub('Cyclopoida','CY',ems.diet.mean.all$food.item)
ems.diet.mean.all$food.item <- gsub('Chironomid Larvae','CL',ems.diet.mean.all$food.item)
ems.diet.mean.all$food.item <- gsub('Bosminidae','BO',ems.diet.mean.all$food.item)
ems.diet.mean.all$food.item <- gsub('Leptodoridae','LE',ems.diet.mean.all$food.item)
ems.diet.mean.all$food.item <- gsub('Nematoda','NE',ems.diet.mean.all$food.item)
ems.diet.mean.all$food.item <- gsub('Chironomid Pupae','CP',ems.diet.mean.all$food.item)
ems.diet.mean.all$food.item <- gsub('Daphnidae','DA',ems.diet.mean.all$food.item)
ems.diet.mean.all$food.item <- gsub('Oligochaeta','OL',ems.diet.mean.all$food.item)

## -----------------------------------------------------------
## Arrange in ascending order, filter taxa, and mutate variable structure
## -----------------------------------------------------------
ems.diet.mean.all %<>% arrange(month,basin,food.item) %>% 
  filter(food.item %in% c('BO','CA','CY','DA','LE')) %>% 
  arrange(month,basin,food.item) %>% 
  mutate(month=factor(month),
         basin=factor(basin,levels = c('Western','Central','Eastern'),ordered = TRUE),
         food.item=factor(food.item))

## -----------------------------------------------------------
## Visualization
## -----------------------------------------------------------
ggplot(ems.diet.mean.all,aes(food.item,mean.prey.size,fill=basin)) +
  geom_errorbar(aes(x=food.item,ymin=0,ymax=mean.prey.size+se),
                width=0.25,
                position = position_dodge(.9)) +
  geom_bar(stat='identity',position='dodge') +
  scale_y_continuous(limits = c(0,2.5),expand=c(0,0)) +
  scale_fill_grey(start=0.2,end=0.7) +
  theme_linedraw()

ggplot(ems.diet.mean.all,aes(food.item,mean.prey.size,fill=basin)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0,2.5),expand=c(0,0)) +
  scale_fill_grey(start=0.2,end=0.7) +
  theme_linedraw()

## -----------------------------------------------------------
## ANOVA
## -----------------------------------------------------------
lm.prey.may <- lm(mean.prey.size~food.item*basin,data=filter(ems.diet.mean.all,month=="May"))
lm.prey.sept <- lm(mean.prey.size~food.item*basin,data=filter(ems.diet.mean.all,month=="September"))
lm.prey <- lm(mean.prey.size~food.item*basin*month,data=ems.diet.mean.all)
anova(lm.prey.may)
anova(lm.prey.sept)
anova(lm.prey)

fitPlot(lm.prey.may)
fitPlot(lm.prey.sept)
