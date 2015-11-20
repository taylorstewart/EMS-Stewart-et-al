##############################################################
##############################################################
##  EMS (Taylor Stewart et al.) manuscript work
##
##  ANALYSIS SETUP SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Clear the environment first
## ===========================================================
rm(list = ls(all.names=TRUE))
gc()

## ===========================================================
## Load Packages -- used here and in other scripts
##   other packages loaded as needed in the individual scripts
## ===========================================================
library(dplyr)     # manipulating data
library(magrittr)  # for %<>%
library(readxl)    # reading data
library(ggplot2)   # visualizations
library(FSA)       # ANCOVA functions
library(car)       # assumption tests
library(tidyr)     # tidy data

## ===========================================================
## Set the random seed for reproducibility
## ===========================================================
set.seed(84621684)

## ===========================================================
## Load and Initial Manipulations of the Fish Sample Data
## ===========================================================
## -----------------------------------------------------------
## Load diet and calorimetry data
## -----------------------------------------------------------
ems_diet <- read_excel(path="data/CSMI_2014_EmeraldShiner.xlsx",sheet="Diet Summary") %>% 
  mutate(fid=factor(fid),
         serial=factor(serial),
         month=factor(month),
         basin=factor(basin),
         food_item=factor(food_item),
         mean_biomass_mg=as.numeric(mean_biomass_mg)) %>% 
  select(-comments,-c(9:18))
ems_cal <- read_excel(path="data/CSMI_2014_EmeraldShiner.xlsx",sheet="Calorimetry") %>% 
  mutate(fid=factor(fid),
         serial=factor(serial),
         month=factor(month),
         basin=factor(basin)) %>%
  filter(include=="Yes")

## ===========================================================
## Zooplankton Data Manipulation
## ===========================================================
## -----------------------------------------------------------
## Load rep1 zooplankton data
## -----------------------------------------------------------
zoop_pred <- read_excel("data/zooplankton/2014_CSMI_Zp_Pred_Clad_Rep1.xlsx","Biom",skip=3) %>% 
  slice(2:155) %>% 
  select(-c(63:77,132:145,149:167))  #drop columns of egg counts and other noise
zoop_sub <- read_excel("data/zooplankton/2014_CSMI_Zp_Sub_Rep1.xlsx","Biom",skip=3) %>%
  slice(-1) %>% 
  select(-c(63:77,132:145,149:167))  #drop columns of egg counts and other noise

## -----------------------------------------------------------
## reshape data into key(species)-value pairs
## -----------------------------------------------------------
tidy_zoop_pred <- zoop_pred %>%
  ## remove samples with issues (i.e. flowmeter, net under boat, etc.)
  filter(!(SampleLocation %in% c("5096Fair","5103MB","5124Fair","5131Erie","5130Erie","5240Lor","5248Fair"))) %>% 
  gather(species,value,-SampleDate,-SampleLocation)
tidy_zoop_sub <- zoop_sub %>%
  ## remove samples with issues (i.e. flowmeter, net under boat, etc.)
  filter(!(SampleLocation %in% c("5096Fair","5103MB","5124Fair","5131Erie","5130Erie","5240Lor","5248Fair"))) %>% 
  gather(species,value,-SampleDate,-SampleLocation)

## -----------------------------------------------------------
## remove "w/ eggs" and "w/o eggs" from column names
## -----------------------------------------------------------
tidy_zoop_pred$species <- gsub(" w/ eggs","",tidy_zoop_pred$species)
tidy_zoop_pred$species <- gsub("w/ eggs","",tidy_zoop_pred$species)
tidy_zoop_pred$species <- gsub(" w/o eggs","",tidy_zoop_pred$species)
tidy_zoop_pred$species <- gsub("w/o eggs","",tidy_zoop_pred$species)
tidy_zoop_sub$species <- gsub(" w/ eggs","",tidy_zoop_sub$species)
tidy_zoop_sub$species <- gsub("w/ eggs","",tidy_zoop_sub$species)
tidy_zoop_sub$species <- gsub(" w/o eggs","",tidy_zoop_sub$species)
tidy_zoop_sub$species <- gsub("w/o eggs","",tidy_zoop_sub$species)

## -----------------------------------------------------------
## summarize values
## -----------------------------------------------------------
tidy_zoop_pred %<>%
  mutate(SampleDate=factor(SampleDate),
         SampleLocation=factor(SampleLocation),
         species=factor(species),
         value=as.numeric(value))
tidy_zoop_sub %<>%
  mutate(SampleDate=factor(SampleDate),
         SampleLocation=factor(SampleLocation),
         species=factor(species),
         value=as.numeric(value))
tidy_zoop_pred %<>% group_by(SampleDate,SampleLocation,species) %>% 
  summarise(value2=sum(value)) %>% 
  filter(value2 > 0)
tidy_zoop_sub %<>% group_by(SampleDate,SampleLocation,species) %>% 
  summarise(value2=sum(value)) %>% 
  filter(value2 > 0)
tidy_zoop_pred$serial<-substr(tidy_zoop_pred$SampleLocation,1,4)
tidy_zoop_sub$serial<-substr(tidy_zoop_sub$SampleLocation,1,4)

## -----------------------------------------------------------
## combine values from subsample data and whole count predatory data
## -----------------------------------------------------------
all_zoop <- bind_rows(tidy_zoop_sub,tidy_zoop_pred) %>% 
  mutate(serial=as.numeric(serial)) %>% 
  select(-SampleDate,-SampleLocation)

## -----------------------------------------------------------
## Now read in effort data so can merge in region, depth, lat, long, etc.
## -----------------------------------------------------------
ef <- read_excel("data/zooplankton/CSMI_Zp_Effort.xlsx",sheet="Effort") %>% 
  select(-depth_ft,-depth_m,-date,-longitude,-latitude)

## -----------------------------------------------------------
## merge zoop data and effort data
## -----------------------------------------------------------
zoop_effort <- inner_join(ef,all_zoop)

## -----------------------------------------------------------
## read in taxamonic data and join with zoop data
## -----------------------------------------------------------
taxa <- read_excel("data/zooplankton/CSMI_Zp_Taxonomy.xlsx",sheet="Taxa")
zoop_taxa <- left_join(zoop_effort,taxa)

## -----------------------------------------------------------
##  average and sum zooplankton biomass by month, region, depth strata, and family
## -----------------------------------------------------------
zoop_biomass <- zoop_taxa %>% group_by(month,region,depth_strata,family) %>% 
  summarize(mean_biom=mean(value2),
            total_biom=sum(value2))

## ===========================================================
## clean up environment
## ===========================================================
rm(all_zoop,ef,taxa,tidy_zoop_pred,tidy_zoop_sub,zoop_effort,zoop_pred,zoop_sub,zoop_taxa)
