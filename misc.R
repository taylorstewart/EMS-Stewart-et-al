##############################################################
##############################################################
##  EMS (Stewart et al.) manuscript work
##
##  MISC. ANALYSIS SCRIPT
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
library(multcomp)  # for glht()
library(dplyr)     # manipulating data
library(magrittr)  # for %<>%
library(readxl)    # reading data
library(ggplot2)   # visualizations
library(FSA)       # ANCOVA functions
library(car)       # assumption tests
library(tidyr)     # tidy data
library(nlstools)  # non-linear modeling
library(grid)      # plot matrix text
library(gridExtra) # plot matrix layout
library(gtable)    # plot matrix legend

## ===========================================================
## Load and Initial Manipulations of the Fish Sample Data
## ===========================================================
## -----------------------------------------------------------
## Load data
## -----------------------------------------------------------
ems.cal <- read_excel(path="data/CSMI_2014_EmeraldShiner.xlsx",sheet="Calorimetry") %>% 
  mutate(fid=factor(fid),
         serial=factor(serial),
         month=factor(month),
         basin=factor(basin,levels = c('Western','Central','Eastern'),ordered = TRUE),
         log.tl=log(tl),
         log.wt=log(wet.wt),
         log.hoc=log(wet.HOC.JG)) %>%
  filter(include=="Yes")

ems.pellet <- ems.cal %>% mutate(dry.pellet = sample.wt-spike.wt) %>% 
  summarize(min=min(dry.pellet),
            max=max(dry.pellet),
            mean=mean(dry.pellet))

ems.spike <- ems.cal %>% summarize(min=min(spike.wt),
                                    max=max(spike.wt),
                                    mean=mean(spike.wt))
