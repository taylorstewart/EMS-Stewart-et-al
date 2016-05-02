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
ems.diet
ems.zoop
ems.benthos

## -----------------------------------------------------------
## Remove stomachs with no diet contents
## -----------------------------------------------------------
ems.diet %<>% filter(food.item != 'Empty') %>% 
  droplevels()

## -----------------------------------------------------------
## Combine benthos and zooplankton prey
## -----------------------------------------------------------
ems.prey <- bind_rows(ems.zoop,ems.benthos) %>% 
  group_by(month,region,basin,ems.taxa) %>% 
  summarize(biomass=mean(biomass))

## -----------------------------------------------------------
## Create unique lists of serials sampled, FID, diet prey, and available prey
## -----------------------------------------------------------
region.list <- as.character(unique(ems.prey$region))
basin.list <- as.character(unique(ems.prey$basin))
month.list <-  c('May','September')
fid.list <- unique(ems.diet$fid)
diet.list <- unique(ems.diet$food.item)
prey.list <- unique(ems.prey$ems.taxa)

##############################################################
## Prey Available in Environment (Benthos and Zoops)
##############################################################
## -----------------------------------------------------------
## Filter prey taxa to only indentified taxa
## -----------------------------------------------------------
ems.prey %<>% filter(ems.taxa %in% diet.list, month %in% month.list)

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

##############################################################
## Prey Consumed in Diets (Benthos and Zoops)
##############################################################
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
  tmp <- data.frame(fid=rep(i,n),serial=serial,region=region,food.item=pl,mean.size=rep(0,n),biomass=rep(0,n))
})))

## -----------------------------------------------------------
## Join zero data by serial to fill in other variables
## -----------------------------------------------------------
ems.diet.effort <- ems.diet %>% distinct(serial,month,region,basin) %>% select(serial,month,region,basin)
ems.diet.zero <- left_join(diet.missing,ems.diet.effort)
ems.diet.all <- bind_rows(ems.diet,ems.diet.zero) %>% arrange(fid)

##############################################################
## Calculations
##############################################################
## -----------------------------------------------------------
## Calculate the proportion of prey in the diet of each individual fish
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
## May
envir.may.prop <- as.data.frame(do.call(rbind,lapply(region.list,function(i) {
  indiv <- ems.prey.all %>% filter(region == i,month == 'May')
  prop <- as.data.frame(do.call(rbind,lapply(diet.list,function(j) {
    round((filter(indiv,ems.taxa == j)$biomass)/sum(indiv$biomass),4)
  })))
  data.frame(month=rep('May',9),region=rep(i,9),food.item=diet.list,envir.prop=prop)
}))) %>% 
  mutate(envir.prop=V1) %>% select(-V1)

## September
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
ems.prey.select %<>% mutate('diet.envir' = diet.prop/envir.prop)

## -----------------------------------------------------------
## Calculate alpha ((r/p)/sum(r/p))
## -----------------------------------------------------------
alpha <- data.frame(do.call(rbind,lapply(fid.list,function(k) {
  indiv <- ems.prey.select %>% filter(fid == k)
  month <- unique(as.character(indiv$month))
  basin <- unique(as.character(indiv$basin))
  taxa <- as.character(indiv$food.item)
  n <- nrow(indiv)
    tmp <- data.frame(diet.envir=do.call(rbind,lapply(1:n,function(p) {
      round(indiv$diet.envir[p]/sum(indiv$diet.envir),5)
    })))
  data.frame(fid=rep(k,n),month,basin,taxa,alpha=tmp$diet.envir)
}))) %>% 
  mutate(fid=as.character(fid),
         month=as.character(month),
         basin=as.character(basin),
         taxa=as.character(taxa),
         alpha.arc=asin(sqrt(alpha))) %>% 
  arrange(month,basin,taxa) %>% 
  filter(alpha != 0) %>% 
  na.omit()

## -----------------------------------------------------------
## Clean up environment
## -----------------------------------------------------------
rm(diet.missing,diet.prop,ems.diet.all,ems.diet.prop,ems.diet.zero,ems.diet.effort,
   ems.prey,ems.prey.all,ems.prey.select,ems.prey.zero,ems.prey.effort,envir.prop.all,
   envir.sept.prop,envir.may.prop,envir.missing,prey.list,region.list)

## -----------------------------------------------------------
## Calculate the mean and arcsin transformed confidence intervals
## -----------------------------------------------------------
## May and Western
alpha.mw <- alpha %>% filter(month=="May",basin=="Western")
n.mw <- alpha.mw %>% group_by(taxa) %>% 
  summarize(n=n())
alpha.mw.greater <- left_join(alpha.mw,n.mw) %>% 
  filter(n > 1) %>% droplevels()
alpha.mw.less <- left_join(alpha.mw,n.mw) %>% 
  filter(n <= 1) %>% droplevels()
taxa.mw.greater <- unique(alpha.mw.greater$taxa)
taxa.mw.less <- unique(alpha.mw.less$taxa)
model.mw.greater <- glm(alpha.arc~0+taxa,family="gaussian",data=alpha.mw.greater)
b0 <- 0
while(b0!=100) {
  x <- try({
    boot.mw <- Boot(model.mw.greater,R=1000)
    ci.mw <- confint(boot.mw,level=0.95)
    df1.mw <- data.frame(
      month="May",
      basin="Western",
      taxa=taxa.mw.greater,
      n=distinct(alpha.mw.greater,taxa)$n,
      alpha=sin(coef(model.mw.greater))^2,
      lower.ci=sin(ci.mw[,1])^2,
      upper.ci=sin(ci.mw[,2])^2)
    df2.mw <- if(length(taxa.mw.less) > 0) {
      model.mw.less <- alpha.mw.less %>% group_by(taxa) %>% 
        summarize(mean=mean(alpha.arc))
      data.frame(
        month="May",
        basin="Western",
        taxa=taxa.mw.less,
        n=rep(1,nrow(alpha.mw.less)),
        alpha=sin(model.mw.less$mean)^2,
        lower.ci=rep(0,nrow(alpha.mw.less)),
        upper.ci=rep(0,nrow(alpha.mw.less)))
      }
    df.mw.final <- bind_rows(df1.mw,df2.mw)
  },silent=TRUE)
  if (class(x)=="try-error") {
    cat("ERROR1: ", x, "\n")
    Sys.sleep(1)
    print("reconntecting...")
    b0 <- b0+1
    print(b0)
  } else break 
}
rm(b0,alpha.mw,n.mw,alpha.mw.greater,alpha.mw.less,taxa.mw.greater,taxa.mw.less,model.mw.greater,boot.mw,ci.mw,df1.mw,df2.mw)

## May and Central
alpha.mc <- alpha %>% filter(month=="May",basin=="Central")
n.mc <- alpha.mc %>% group_by(taxa) %>% 
  summarize(n=n())
alpha.mc.greater <- left_join(alpha.mc,n.mc) %>% 
  filter(n > 1) %>% droplevels()
alpha.mc.less <- left_join(alpha.mc,n.mc) %>% 
  filter(n <= 1) %>% droplevels()
taxa.mc.greater <- unique(alpha.mc.greater$taxa)
taxa.mc.less <- unique(alpha.mc.less$taxa)
model.mc.greater <- glm(alpha.arc~0+taxa,family="gaussian",data=alpha.mc.greater)
b0 <- 0
while(b0!=100) {
  x <- try({
    boot.mc <- Boot(model.mc.greater,R=1000)
    ci.mc <- confint(boot.mc,level=0.95)
    df1.mc <- data.frame(
      month="May",
      basin="Central",
      taxa=taxa.mc.greater,
      n=distinct(alpha.mc.greater,taxa)$n,
      alpha=sin(coef(model.mc.greater))^2,
      lower.ci=sin(ci.mc[,1])^2,
      upper.ci=sin(ci.mc[,2])^2)
    df2.mc <- if(length(taxa.mc.less) > 0) {
      model.mc.less <- alpha.mc.less %>% group_by(taxa) %>% 
        summarize(mean=mean(alpha.arc))
      data.frame(
        month="May",
        basin="Central",
        taxa=taxa.mc.less,
        n=rep(1,nrow(alpha.mc.less)),
        alpha=sin(model.mc.less$mean)^2,
        lower.ci=rep(0,nrow(alpha.mc.less)),
        upper.ci=rep(0,nrow(alpha.mc.less)))
    }
    df.mc.final <- bind_rows(df1.mc,df2.mc)
  },silent=TRUE)
  if (class(x)=="try-error") {
    cat("ERROR1: ", x, "\n")
    Sys.sleep(1)
    print("reconntecting...")
    b0 <- b0+1
    print(b0)
  } else break 
}
rm(b0,alpha.mc,n.mc,alpha.mc.greater,alpha.mc.less,taxa.mc.greater,taxa.mc.less,model.mc.greater,model.mc.less,boot.mc,ci.mc,df1.mc,df2.mc)

## May and Eastern
alpha.me <- alpha %>% filter(month=="May",basin=="Eastern")
n.me <- alpha.me %>% group_by(taxa) %>% 
  summarize(n=n())
alpha.me.greater <- left_join(alpha.me,n.me) %>% 
  filter(n > 1) %>% droplevels()
alpha.me.less <- left_join(alpha.me,n.me) %>% 
  filter(n <= 1) %>% droplevels()
taxa.me.greater <- unique(alpha.me.greater$taxa)
taxa.me.less <- unique(alpha.me.less$taxa)
model.me.greater <- glm(alpha.arc~0+taxa,family="gaussian",data=alpha.me.greater)
b0 <- 0
while(b0!=100) {
  x <- try({
    boot.me <- Boot(model.me.greater,R=1000)
    ci.me <- confint(boot.me,level=0.95)
    df1.me <- data.frame(
      month="May",
      basin="Eastern",
      taxa=taxa.me.greater,
      n=distinct(alpha.me.greater,taxa)$n,
      alpha=sin(coef(model.me.greater))^2,
      lower.ci=sin(ci.me[,1])^2,
      upper.ci=sin(ci.me[,2])^2)
    df2.me <- if(length(taxa.me.less) > 0) {
      model.me.less <- alpha.me.less %>% group_by(taxa) %>% 
        summarize(mean=mean(alpha.arc))
      data.frame(
        month="May",
        basin="Eastern",
        taxa=taxa.me.less,
        n=rep(1,nrow(alpha.me.less)),
        alpha=sin(model.me.less$mean)^2,
        lower.ci=rep(0,nrow(alpha.me.less)),
        upper.ci=rep(0,nrow(alpha.me.less)))
    }
    df.me.final <- bind_rows(df1.me,df2.me)
  },silent=TRUE)
  if (class(x)=="try-error") {
    cat("ERROR1: ", x, "\n")
    Sys.sleep(1)
    print("reconntecting...")
    b0 <- b0+1
    print(b0)
  } else break 
}
rm(b0,alpha.me,n.me,alpha.me.greater,alpha.me.less,taxa.me.greater,taxa.me.less,model.me.greater,model.me.less,boot.me,ci.me,df1.me,df2.me)

## September and Western
alpha.sw <- alpha %>% filter(month=="September",basin=="Western")
n.sw <- alpha.sw %>% group_by(taxa) %>% 
  summarize(n=n())
alpha.sw.greater <- left_join(alpha.sw,n.sw) %>% 
  filter(n > 1) %>% droplevels()
alpha.sw.less <- left_join(alpha.sw,n.sw) %>% 
  filter(n <= 1) %>% droplevels()
taxa.sw.greater <- unique(alpha.sw.greater$taxa)
taxa.sw.less <- unique(alpha.sw.less$taxa)
model.sw.greater <- glm(alpha.arc~0+taxa,family="gaussian",data=alpha.sw.greater)
b0 <- 0
while(b0!=100) {
  x <- try({
    boot.sw <- Boot(model.sw.greater,R=1000)
    ci.sw <- confint(boot.sw,level=0.95)
    df1.sw <- data.frame(
      month="September",
      basin="Western",
      taxa=taxa.sw.greater,
      n=distinct(alpha.sw.greater,taxa)$n,
      alpha=sin(coef(model.sw.greater))^2,
      lower.ci=sin(ci.sw[,1])^2,
      upper.ci=sin(ci.sw[,2])^2)
    df2.sw <- if(length(taxa.sw.less) > 0) {
      model.sw.less <- alpha.sw.less %>% group_by(taxa) %>% 
        summarize(mean=mean(alpha.arc))
      data.frame(
        month="September",
        basin="Western",
        taxa=taxa.sw.less,
        n=rep(1,nrow(alpha.sw.less)),
        alpha=sin(model.sw.less$mean)^2,
        lower.ci=rep(0,nrow(alpha.sw.less)),
        upper.ci=rep(0,nrow(alpha.sw.less)))
    }
    df.sw.final <- bind_rows(df1.sw,df2.sw)
  },silent=TRUE)
  if (class(x)=="try-error") {
    cat("ERROR1: ", x, "\n")
    Sys.sleep(1)
    print("reconntecting...")
    b0 <- b0+1
    print(b0)
  } else break 
}
rm(b0,alpha.sw,n.sw,alpha.sw.greater,alpha.sw.less,taxa.sw.greater,taxa.sw.less,model.sw.greater,boot.sw,ci.sw,df1.sw,df2.sw)

## September and Central
alpha.sc <- alpha %>% filter(month=="September",basin=="Central")
n.sc <- alpha.sc %>% group_by(taxa) %>% 
  summarize(n=n())
alpha.sc.greater <- left_join(alpha.sc,n.sc) %>% 
  filter(n > 1) %>% droplevels()
alpha.sc.less <- left_join(alpha.sc,n.sc) %>% 
  filter(n <= 1) %>% droplevels()
taxa.sc.greater <- unique(alpha.sc.greater$taxa)
taxa.sc.less <- unique(alpha.sc.less$taxa)
model.sc.greater <- glm(alpha.arc~0+taxa,family="gaussian",data=alpha.sc.greater)
b0 <- 0
while(b0!=100) {
  x <- try({
    boot.sc <- Boot(model.sc.greater,R=1000)
    ci.sc <- confint(boot.sc,level=0.95)
    df1.sc <- data.frame(
      month="September",
      basin="Central",
      taxa=taxa.sc.greater,
      n=distinct(alpha.sc.greater,taxa)$n,
      alpha=sin(coef(model.sc.greater))^2,
      lower.ci=sin(ci.sc[,1])^2,
      upper.ci=sin(ci.sc[,2])^2)
    df2.sc <- if(length(taxa.sc.less) > 0) {
      model.sc.less <- alpha.sc.less %>% group_by(taxa) %>% 
        summarize(mean=mean(alpha.arc))
      data.frame(
        month="September",
        basin="Central",
        taxa=taxa.sc.less,
        n=rep(1,nrow(alpha.sc.less)),
        alpha=sin(model.sc.less$mean)^2,
        lower.ci=rep(0,nrow(alpha.sc.less)),
        upper.ci=rep(0,nrow(alpha.sc.less)))
    }
    df.sc.final <- bind_rows(df1.sc,df2.sc)
  },silent=TRUE)
  if (class(x)=="try-error") {
    cat("ERROR1: ", x, "\n")
    Sys.sleep(1)
    print("reconntecting...")
    b0 <- b0+1
    print(b0)
  } else break 
}
rm(b0,alpha.sc,n.sc,alpha.sc.greater,alpha.sc.less,taxa.sc.greater,taxa.sc.less,model.sc.greater,model.sc.less,boot.sc,ci.sc,df1.sc,df2.sc)

## September and Eastern
alpha.se <- alpha %>% filter(month=="September",basin=="Eastern")
n.se <- alpha.se %>% group_by(taxa) %>% 
  summarize(n=n())
alpha.se.greater <- left_join(alpha.se,n.se) %>% 
  filter(n > 1) %>% droplevels()
alpha.se.less <- left_join(alpha.se,n.se) %>% 
  filter(n <= 1) %>% droplevels()
taxa.se.greater <- unique(alpha.se.greater$taxa)
taxa.se.less <- unique(alpha.se.less$taxa)
model.se.greater <- glm(alpha.arc~0+taxa,family="gaussian",data=alpha.se.greater)
b0 <- 0
while(b0!=100) {
  x <- try({
    boot.se <- Boot(model.se.greater,R=1000)
    ci.se <- confint(boot.se,level=0.95)
    df1.se <- data.frame(
      month="September",
      basin="Eastern",
      taxa=taxa.se.greater,
      n=distinct(alpha.se.greater,taxa)$n,
      alpha=sin(coef(model.se.greater))^2,
      lower.ci=sin(ci.se[,1])^2,
      upper.ci=sin(ci.se[,2])^2)
    df2.se <- if(length(taxa.se.less) > 0) {
      model.se.less <- alpha.se.less %>% group_by(taxa) %>% 
        summarize(mean=mean(alpha.arc))
      data.frame(
        month="September",
        basin="Eastern",
        taxa=taxa.se.less,
        n=rep(1,nrow(alpha.se.less)),
        alpha=sin(model.se.less$mean)^2,
        lower.ci=rep(0,nrow(alpha.se.less)),
        upper.ci=rep(0,nrow(alpha.se.less)))
    }
    df.se.final <- bind_rows(df1.se,df2.se)
  },silent=TRUE)
  if (class(x)=="try-error") {
    cat("ERROR1: ", x, "\n")
    Sys.sleep(1)
    print("reconntecting...")
    b0 <- b0+1
    print(b0)
  } else break 
}
rm(b0,alpha.se,n.se,alpha.se.greater,alpha.se.less,taxa.se.greater,taxa.se.less,model.se.greater,boot.se,ci.se,df1.se,df2.se)

## -----------------------------------------------------------
## Bind all confidence intervals
## -----------------------------------------------------------
alpha.conf <- bind_rows(df.mw.final,df.mc.final,df.me.final,df.sw.final,df.sc.final,df.se.final)

## -----------------------------------------------------------
## Rearrange data frame for paired t-test
## -----------------------------------------------------------
## Add zero values for consitent pairs among months and basins
## Apply first loop function
output1 <- data.frame(do.call(rbind,lapply(month.list,function(i) {
  ## Filter catch by each serial
  select.filt <- alpha.conf %>% filter(month==i)
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
    tmp <- data.frame(month=rep(i,n),basin=rep(j,n),taxa=taxa,n=rep(0,n),alpha=rep(0,n),lower.ci=rep(0,n),upper.ci=rep(0,n))
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
final.alpha <- bind_rows(alpha.conf,output1) %>% 
  arrange(month,basin,taxa) %>% 
  mutate(basin=factor(basin,levels = c('Western','Central','Eastern'),ordered = TRUE))
## remove extra objects
rm(alpha,alpha.conf,output1,basin.list,diet.list,month.list)

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

##############################################################
## Visualization
##############################################################
may.west <- ggplot(filter(final.alpha,month=='May',basin=='Western'),aes(taxa,alpha)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=lower.ci,ymax=upper.ci)) +
  labs(x='',y='May',title='Western\n') +
  scale_y_continuous(limit=c(0,1.006),expand=c(0,0)) +
  theme(axis.line.y=element_line(),axis.line.x=element_line(),axis.ticks.length=unit(1.5,'mm'),
        axis.text.y=element_text(size=15,vjust=0.5,hjust=0),axis.text.x=element_blank(),plot.title=element_text(size=16),
        axis.title=element_text(size=19),panel.background=element_blank(),plot.margin=unit(c(5,0,-6,3),'mm'))

may.cen <- ggplot(filter(final.alpha,month=='May',basin=='Central'),aes(taxa,alpha)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=lower.ci,ymax=upper.ci)) +
  labs(x='',y='',title='Central\n') +
  scale_y_continuous(limit=c(0,1.006),expand=c(0,0)) +
  theme(axis.line.y=element_line(),axis.line.x=element_line(),plot.title=element_text(size=16),
        axis.text.y=element_blank(),axis.text.x=element_blank(),panel.background=element_blank(),
        axis.ticks.length=unit(1.5,'mm'),plot.margin=unit(c(5,8,-4,8),'mm'))

may.east <- ggplot(filter(final.alpha,month=='May',basin=='Eastern'),aes(taxa,alpha)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=lower.ci,ymax=upper.ci)) +
  labs(x='',y='',title='Eastern\n') +
  scale_y_continuous(limit=c(0,1.006),expand=c(0,0)) +
  theme(axis.line.y=element_line(),axis.line.x=element_line(),plot.title=element_text(size=16),
        axis.text.y=element_blank(),axis.text.x=element_blank(),panel.background=element_blank(),
        axis.ticks.length=unit(1.5,'mm'),plot.margin=unit(c(5,16,-4,0),'mm'))

sept.west <- ggplot(filter(final.alpha,month=='September',basin=='Western'),aes(taxa,alpha)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=lower.ci,ymax=upper.ci)) +
  labs(x='',y='September',title='') +
  scale_y_continuous(limit=c(0,1.006),expand=c(0,0)) +
  theme(axis.line.y=element_line(),axis.line.x=element_line(),
        axis.text.x=element_text(size=13),axis.text.y=element_text(size=15),axis.ticks.length=unit(1.5,'mm'),
        axis.title=element_text(size=19),panel.background=element_blank(),plot.margin=unit(c(11,0,-17,3),'mm'))

sept.cen <- ggplot(filter(final.alpha,month=='September',basin=='Central'),aes(taxa,alpha)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=lower.ci,ymax=upper.ci)) +
  labs(x='',y='',title='') +
  scale_y_continuous(limit=c(0,1.006),expand=c(0,0)) +
  theme(axis.line.y=element_line(),axis.line.x=element_line(),
        axis.text.x=element_text(size=13),axis.ticks.length=unit(1.5,'mm'),
        axis.text.y=element_blank(),panel.background=element_blank(),plot.margin=unit(c(11,8,-15,8),'mm'))

sept.east <- ggplot(filter(final.alpha,month=='September',basin=='Eastern'),aes(taxa,alpha)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=lower.ci,ymax=upper.ci)) +
  labs(x='',y='',title='') +
  scale_y_continuous(limit=c(0,1.006),expand=c(0,0)) +
  theme(axis.line.y=element_line(),axis.line.x=element_line(),
        axis.text.x=element_text(size=13),axis.ticks.length=unit(1.5,'mm'),
        axis.text.y=element_blank(),panel.background=element_blank(),plot.margin=unit(c(11,15,-15,1),'mm'))

## -----------------------------------------------------------
## Save the plot as a figure (comment out line 273 and 289 until you are ready to save)
## -----------------------------------------------------------
png("figs/prey_selectivity_wCI.PNG",width=10.75,height=8.5,units="in",family="Times",res=300)

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
                         left=textGrob("Selectivity Index (W')",y=unit(90,'mm'),rot=90,gp=gpar(fontsize=25)),
                         bottom=textGrob('Prey Type',y=unit(-15,'mm'),x=unit(136,'mm'),gp=gpar(fontsize=25))),
             heights=c(8,1))

## -----------------------------------------------------------
## Close the device to make the actual PNG file
## -----------------------------------------------------------
dev.off()
