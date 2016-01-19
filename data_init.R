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
library(nlstools)  # non-linear modeling

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
         basin=factor(basin,levels = c('Western','Central','Eastern'),ordered = TRUE),
         food_item=factor(food_item),
         mean_biomass_mg=as.numeric(mean_biomass_mg)) %>% 
  select(-comments,-c(9:18))
ems_cal <- read_excel(path="data/CSMI_2014_EmeraldShiner.xlsx",sheet="Calorimetry") %>% 
  mutate(fid=factor(fid),
         serial=factor(serial),
         month=factor(month),
         basin=factor(basin,levels = c('Western','Central','Eastern'),ordered = TRUE),
         log_tl=log(tl),
         log_wt=log(wet_wt),
         log_hoc=log(wet_HOC_JG)) %>%
  filter(include=="Yes") %>% 
  select(-c(8:15),-timestamp,-include,-comments)

## ===========================================================
## Zooplankton Data Manipulation
## ===========================================================
## -----------------------------------------------------------
## Load rep1 zooplankton data
## -----------------------------------------------------------
zoop_pred_biom <- read_excel("data/zooplankton/2014_CSMI_Zp_Pred_Clad_Rep1.xlsx","Biom",skip=3) %>% 
  slice(2:155) %>% 
  select(-c(63:77,132:145,149:167))  #drop columns of egg counts and other noise
zoop_sub_biom <- read_excel("data/zooplankton/2014_CSMI_Zp_Sub_Rep1.xlsx","Biom",skip=3) %>%
  slice(-1) %>% 
  select(-c(63:77,132:145,149:167))  #drop columns of egg counts and other noise
zoop_pred_dens <- read_excel("data/zooplankton/2014_CSMI_Zp_Pred_Clad_Rep1.xlsx","Dens",skip=3) %>% 
  slice(2:155) %>% 
  select(-c(63:77,132:145,149:167))  #drop columns of egg counts and other noise
zoop_sub_dens <- read_excel("data/zooplankton/2014_CSMI_Zp_Sub_Rep1.xlsx","Dens",skip=3) %>%
  slice(-1) %>% 
  select(-c(63:77,132:145,149:167))  #drop columns of egg counts and other noise

## -----------------------------------------------------------
## reshape data into key(species)-value pairs
## -----------------------------------------------------------
tidy_zoop_pred_biom <- zoop_pred_biom %>%
  ## remove samples with issues (i.e. flowmeter, net under boat, etc.)
  filter(!(SampleLocation %in% c("5096Fair","5103MB","5124Fair","5131Erie","5130Erie","5240Lor","5248Fair"))) %>% 
  gather(species,biomass,-SampleDate,-SampleLocation)
tidy_zoop_sub_biom <- zoop_sub_biom %>%
  ## remove samples with issues (i.e. flowmeter, net under boat, etc.)
  filter(!(SampleLocation %in% c("5096Fair","5103MB","5124Fair","5131Erie","5130Erie","5240Lor","5248Fair"))) %>% 
  gather(species,biomass,-SampleDate,-SampleLocation)
tidy_zoop_pred_dens <- zoop_pred_dens %>%
  ## remove samples with issues (i.e. flowmeter, net under boat, etc.)
  filter(!(SampleLocation %in% c("5096Fair","5103MB","5124Fair","5131Erie","5130Erie","5240Lor","5248Fair"))) %>% 
  gather(species,density,-SampleDate,-SampleLocation)
tidy_zoop_sub_dens <- zoop_sub_dens %>%
  ## remove samples with issues (i.e. flowmeter, net under boat, etc.)
  filter(!(SampleLocation %in% c("5096Fair","5103MB","5124Fair","5131Erie","5130Erie","5240Lor","5248Fair"))) %>% 
  gather(species,density,-SampleDate,-SampleLocation)

## -----------------------------------------------------------
## remove "w/ eggs" and "w/o eggs" from column names
## -----------------------------------------------------------
tidy_zoop_pred_biom$species <- gsub(" w/ eggs","",tidy_zoop_pred_biom$species)
tidy_zoop_pred_biom$species <- gsub("w/ eggs","",tidy_zoop_pred_biom$species)
tidy_zoop_pred_biom$species <- gsub(" w/o eggs","",tidy_zoop_pred_biom$species)
tidy_zoop_pred_biom$species <- gsub("w/o eggs","",tidy_zoop_pred_biom$species)
tidy_zoop_sub_biom$species <- gsub(" w/ eggs","",tidy_zoop_sub_biom$species)
tidy_zoop_sub_biom$species <- gsub("w/ eggs","",tidy_zoop_sub_biom$species)
tidy_zoop_sub_biom$species <- gsub(" w/o eggs","",tidy_zoop_sub_biom$species)
tidy_zoop_sub_biom$species <- gsub("w/o eggs","",tidy_zoop_sub_biom$species)
tidy_zoop_pred_dens$species <- gsub(" w/ eggs","",tidy_zoop_pred_dens$species)
tidy_zoop_pred_dens$species <- gsub("w/ eggs","",tidy_zoop_pred_dens$species)
tidy_zoop_pred_dens$species <- gsub(" w/o eggs","",tidy_zoop_pred_dens$species)
tidy_zoop_pred_dens$species <- gsub("w/o eggs","",tidy_zoop_pred_dens$species)
tidy_zoop_sub_dens$species <- gsub(" w/ eggs","",tidy_zoop_sub_dens$species)
tidy_zoop_sub_dens$species <- gsub("w/ eggs","",tidy_zoop_sub_dens$species)
tidy_zoop_sub_dens$species <- gsub(" w/o eggs","",tidy_zoop_sub_dens$species)
tidy_zoop_sub_dens$species <- gsub("w/o eggs","",tidy_zoop_sub_dens$species)

## -----------------------------------------------------------
## combine values from subsample data and whole count predatory data
## -----------------------------------------------------------
zoop_pred <- left_join(tidy_zoop_pred_biom,tidy_zoop_pred_dens,by=c("SampleDate", "SampleLocation", "species"))
zoop_sub <- left_join(tidy_zoop_sub_biom,tidy_zoop_sub_dens,by=c("SampleDate", "SampleLocation", "species"))
all_zoop <- bind_rows(zoop_pred,zoop_sub) %>%
  mutate(SampleDate=factor(SampleDate),
         SampleLocation=factor(SampleLocation),
         value_den=as.numeric(density),
         value_bio=as.numeric(biomass))

## -----------------------------------------------------------
## summarize values and filter to serials where diets were sampled from
## -----------------------------------------------------------
serial_list <- as.character(unique(ems_diet$serial))
all_zoop %<>% group_by(SampleDate,SampleLocation,species) %>% 
  summarise(density=sum(value_den),
            biomass=sum(value_bio)) %>% 
  filter(biomass > 0, density > 0) %>% ungroup() %>% 
  mutate(serial=as.numeric(substr(SampleLocation,1,4))) %>% 
  filter(serial %in% c(serial_list)) %>% 
  select(serial,species,density,biomass) %>% 
  arrange(serial,species)

## -----------------------------------------------------------
## Now read in effort data so can merge in region, depth, lat, long, etc.
## -----------------------------------------------------------
ef <- read_excel("data/zooplankton/CSMI_Zp_Effort.xlsx",sheet="Effort") %>% 
  select(-depth_strata,-depth_ft,-depth_m,-date,-longitude,-latitude)

## -----------------------------------------------------------
## merge zoop data and effort data
## -----------------------------------------------------------
zoop_effort <- inner_join(ef,all_zoop,by="serial")

## -----------------------------------------------------------
## read in taxamonic data and join with zoop data
## -----------------------------------------------------------
taxa <- read_excel("data/zooplankton/CSMI_Zp_Taxonomy.xlsx",sheet="Taxa")
ems_zoop <- left_join(zoop_effort,taxa,by="species") %>% 
  select(serial,month,region,basin,species,family,suborder,order,density,biomass)

## ===========================================================
## clean up environment
## ===========================================================
rm(all_zoop,ef,taxa,tidy_zoop_pred_biom,tidy_zoop_sub_biom,tidy_zoop_pred_dens,tidy_zoop_sub_dens,
   zoop_effort,zoop_pred_biom,zoop_sub_biom,zoop_pred_dens,zoop_sub_dens,zoop_sub,zoop_pred,serial_list)
