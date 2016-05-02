##############################################################
##############################################################
##  EMS (Stewart et al.) manuscript work
##
##  ANALYSIS SETUP SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Clear the environment first
## ===========================================================
rm(list = ls(all.names=TRUE))

## ===========================================================
## Load Packages -- used here and in other scripts
##   other packages loaded as needed in the individual scripts
## ===========================================================
library(multcomp)   # for glht()
library(dplyr)      # manipulating data
library(magrittr)   # for %<>%
library(readxl)     # reading data
library(ggplot2)    # visualizations
library(FSA)        # ANCOVA functions
library(car)        # assumption tests
library(tidyr)      # tidy data
library(nlstools)   # non-linear modeling
library(grid)       # plot matrix text
library(gridExtra)  # plot matrix layout
library(gtable)     # plot matrix legend
library(NCStats)    # for addSigLetters()
library(lsmeans)    # calculating least-squares means
library(AICcmodavg) # for aictab()
library(car)        # for Boot()

## ===========================================================
## Set the random seed for reproducibility (i.e., randomization
##   is used in the application of the prey selectivity CI's).
## ===========================================================
set.seed(84621684)

## ===========================================================
## Load and Initial Manipulations of the Fish Sample Data
## ===========================================================
## -----------------------------------------------------------
## Load diet, calorimetry, and benthos data
## -----------------------------------------------------------
ems.diet <- read_excel(path="data/CSMI_2014_EmeraldShiner.xlsx",sheet="Diet Summary") %>% 
  mutate(fid=factor(fid),
         serial=factor(serial),
         basin=factor(basin,levels = c('Western','Central','Eastern'),ordered = TRUE),
         mean.size=as.numeric(mean.length.mm),
         biomass=as.numeric(mean.biomass.mg)) %>%
  select(-comments,-c(9:21)) %>% 
  filter(food.item != "Cercopagididae",food.item != "Sididae",food.item != "Hydrachnidiae",food.item != "Harpacticoida")
ems.cal <- read_excel(path="data/CSMI_2014_EmeraldShiner.xlsx",sheet="Calorimetry") %>% 
  mutate(fid=factor(fid),
         serial=factor(serial),
         month=factor(month),
         basin=factor(basin,levels = c('Western','Central','Eastern'),ordered = TRUE),
         log.tl=log(tl),
         log.wt=log(wet.wt),
         log.hoc=log(wet.HOC.JG)) %>%
  filter(include=="Yes") %>% 
  select(-c(8:15),-timestamp,-include,-comments)
ems.benthos <- read_excel(path="data/benthos/CSMI_2014_Benthos_Biomass.xlsx",sheet="Biomass") %>% 
  select(serial=sample.id,month,region,basin,ems.taxa=item,mean.size=mean.length.mm,biomass=mean.biomass.mg) %>%
  filter(ems.taxa!="Empty Sample") %>% 
  group_by(serial,month,region,basin,ems.taxa) %>% 
  summarize(mean.size=mean(mean.size),
            biomass=sum(biomass)) %>% ungroup() %>% 
  mutate(serial=factor(serial))
ems.sia <- read_excel(path="data/stable_isotope/CSMI_2014_SIA_EMS.xlsx",sheet="C&N") %>% 
  select(-region)
ems.pc <- read_excel(path="data/water_quality/CSMI_WQ.xlsx",sheet='WQ') %>% 
  select(serial,month,basin,depth.mean,temp.mean,do.percent.mean,do.ppm.mean)

## -----------------------------------------------------------
## rename some diet species and summarise multiple taxa within the same fish
## -----------------------------------------------------------
ems.diet$food.item <- gsub("nauplii","Cyclopoida",ems.diet$food.item)
ems.diet %<>% group_by(fid,serial,month,region,basin,tl,w.wt,food.item) %>% 
  summarise(mean.size=mean(mean.size),
            biomass=sum(biomass)) %>% ungroup()

## -----------------------------------------------------------
## rename some benthos species
## -----------------------------------------------------------
ems.benthos$ems.taxa <- gsub("Quagga mussel","Dreissenidae",ems.benthos$ems.taxa)
ems.benthos$ems.taxa <- gsub("Zebra mussel","Dreissenidae",ems.benthos$ems.taxa)

## ===========================================================
## Zooplankton Data Manipulation
## ===========================================================
## -----------------------------------------------------------
## Load rep1 zooplankton data
## -----------------------------------------------------------
zoop.pred.biom <- read_excel("data/zooplankton/2014_CSMI_Zp_Pred_Clad_Rep1.xlsx","Biom",skip=3) %>% 
  slice(2:155) %>% 
  select(-c(63:77,132:145,149:167))  #drop columns of egg counts and other noise
zoop.sub.biom <- read_excel("data/zooplankton/2014_CSMI_Zp_Sub_Rep1.xlsx","Biom",skip=3) %>%
  slice(-1) %>% 
  select(-c(63:77,132:145,149:167))  #drop columns of egg counts and other noise
zoop.pred.dens <- read_excel("data/zooplankton/2014_CSMI_Zp_Pred_Clad_Rep1.xlsx","Dens",skip=3) %>% 
  slice(2:155) %>% 
  select(-c(63:77,132:145,149:167))  #drop columns of egg counts and other noise
zoop.sub.dens <- read_excel("data/zooplankton/2014_CSMI_Zp_Sub_Rep1.xlsx","Dens",skip=3) %>%
  slice(-1) %>% 
  select(-c(63:77,132:145,149:167))  #drop columns of egg counts and other noise

## -----------------------------------------------------------
## reshape data into key(species)-value pairs
## -----------------------------------------------------------
tidy.zoop.pred.biom <- zoop.pred.biom %>%
  ## remove samples with issues (i.e. flowmeter, net under boat, etc.)
  filter(!(SampleLocation %in% c("5096Fair","5103MB","5124Fair","5131Erie","5130Erie","5240Lor","5248Fair"))) %>% 
  gather(species,biomass,-SampleDate,-SampleLocation)
tidy.zoop.sub.biom <- zoop.sub.biom %>%
  ## remove samples with issues (i.e. flowmeter, net under boat, etc.)
  filter(!(SampleLocation %in% c("5096Fair","5103MB","5124Fair","5131Erie","5130Erie","5240Lor","5248Fair"))) %>% 
  gather(species,biomass,-SampleDate,-SampleLocation)
tidy.zoop.pred.dens <- zoop.pred.dens %>%
  ## remove samples with issues (i.e. flowmeter, net under boat, etc.)
  filter(!(SampleLocation %in% c("5096Fair","5103MB","5124Fair","5131Erie","5130Erie","5240Lor","5248Fair"))) %>% 
  gather(species,density,-SampleDate,-SampleLocation)
tidy.zoop.sub.dens <- zoop.sub.dens %>%
  ## remove samples with issues (i.e. flowmeter, net under boat, etc.)
  filter(!(SampleLocation %in% c("5096Fair","5103MB","5124Fair","5131Erie","5130Erie","5240Lor","5248Fair"))) %>% 
  gather(species,density,-SampleDate,-SampleLocation)

## -----------------------------------------------------------
## remove "w/ eggs" and "w/o eggs" from column names
## -----------------------------------------------------------
tidy.zoop.pred.biom$species <- gsub(" w/ eggs","",tidy.zoop.pred.biom$species)
tidy.zoop.pred.biom$species <- gsub("w/ eggs","",tidy.zoop.pred.biom$species)
tidy.zoop.pred.biom$species <- gsub(" w/o eggs","",tidy.zoop.pred.biom$species)
tidy.zoop.pred.biom$species <- gsub("w/o eggs","",tidy.zoop.pred.biom$species)
tidy.zoop.sub.biom$species <- gsub(" w/ eggs","",tidy.zoop.sub.biom$species)
tidy.zoop.sub.biom$species <- gsub("w/ eggs","",tidy.zoop.sub.biom$species)
tidy.zoop.sub.biom$species <- gsub(" w/o eggs","",tidy.zoop.sub.biom$species)
tidy.zoop.sub.biom$species <- gsub("w/o eggs","",tidy.zoop.sub.biom$species)
tidy.zoop.pred.dens$species <- gsub(" w/ eggs","",tidy.zoop.pred.dens$species)
tidy.zoop.pred.dens$species <- gsub("w/ eggs","",tidy.zoop.pred.dens$species)
tidy.zoop.pred.dens$species <- gsub(" w/o eggs","",tidy.zoop.pred.dens$species)
tidy.zoop.pred.dens$species <- gsub("w/o eggs","",tidy.zoop.pred.dens$species)
tidy.zoop.sub.dens$species <- gsub(" w/ eggs","",tidy.zoop.sub.dens$species)
tidy.zoop.sub.dens$species <- gsub("w/ eggs","",tidy.zoop.sub.dens$species)
tidy.zoop.sub.dens$species <- gsub(" w/o eggs","",tidy.zoop.sub.dens$species)
tidy.zoop.sub.dens$species <- gsub("w/o eggs","",tidy.zoop.sub.dens$species)

## -----------------------------------------------------------
## combine values from subsample data and whole count predatory data
## -----------------------------------------------------------
zoop.pred <- left_join(tidy.zoop.pred.biom,tidy.zoop.pred.dens,by=c("SampleDate", "SampleLocation", "species"))
zoop.sub <- left_join(tidy.zoop.sub.biom,tidy.zoop.sub.dens,by=c("SampleDate", "SampleLocation", "species"))
all.zoop <- bind_rows(zoop.pred,zoop.sub) %>%
  mutate(SampleDate=factor(SampleDate),
         SampleLocation=factor(SampleLocation),
         value.den=as.numeric(density),
         value.bio=as.numeric(biomass))

## -----------------------------------------------------------
## summarize values and filter to serials where diets were sampled from
## -----------------------------------------------------------
all.zoop %<>% group_by(SampleDate,SampleLocation,species) %>% 
  summarise(density=sum(value.den),
            biomass=sum(value.bio)) %>% 
  filter(biomass > 0, density > 0) %>% ungroup() %>% 
  mutate(serial=as.numeric(substr(SampleLocation,1,4))) %>% 
  select(serial,species,density,biomass) %>% 
  arrange(serial,species)

## -----------------------------------------------------------
## Now read in effort data so can merge in region, depth, lat, long, etc.
## -----------------------------------------------------------
ef <- read_excel("data/zooplankton/CSMI_Zp_Effort.xlsx",sheet="Effort") %>% 
  select(-depth.strata,-depth.ft,-depth.m,-date,-longitude,-latitude)

## -----------------------------------------------------------
## merge zoop data and effort data
## -----------------------------------------------------------
zoop.effort <- inner_join(ef,all.zoop,by="serial")

## -----------------------------------------------------------
## read in taxamonic data and join with zoop data
## -----------------------------------------------------------
taxa <- read_excel("data/zooplankton/CSMI_Zp_Taxonomy.xlsx",sheet="Taxa")
ems.zoop <- left_join(zoop.effort,taxa,by="species") %>% 
  select(serial,month,region,basin,ems.taxa,biomass) %>% 
  group_by(serial,month,region,basin,ems.taxa) %>% 
  summarize(biomass=sum(biomass))

## ===========================================================
## clean up environment
## ===========================================================
rm(all.zoop,ef,taxa,tidy.zoop.pred.biom,tidy.zoop.sub.biom,tidy.zoop.pred.dens,tidy.zoop.sub.dens,
   zoop.effort,zoop.pred.biom,zoop.sub.biom,zoop.pred.dens,zoop.sub.dens,zoop.sub,zoop.pred)
