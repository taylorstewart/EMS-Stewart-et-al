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
source("data_init.R")
str(ems_diet)
str(ems_zoop)
str(ems_benthos)

## -----------------------------------------------------------
## Remove stomachs with no diet contents
## -----------------------------------------------------------
ems_diet %<>% filter(food_item != "Empty")

## -----------------------------------------------------------
## Combine benthos and zooplankton prey
## -----------------------------------------------------------
ems_prey <- bind_rows(ems_zoop,ems_benthos) %>% 
  group_by(month,region,basin,ems_taxa) %>% 
  summarize(biomass=sum(biomass))

## -----------------------------------------------------------
## Create unique lists of serials sampled, FID, diet prey, and available prey
## -----------------------------------------------------------
serial_list <- as.character(unique(ems_diet$serial))
region_list <- as.character(unique(ems_prey$region))
basin_list <- as.character(unique(ems_prey$basin))
month_list <-  c('May','September')
fid_list <- unique(ems_diet$fid)
diet_list <- unique(ems_diet$food_item)
prey_list <- unique(ems_prey$ems_taxa)

## -----------------------------------------------------------
## Filter prey taxa to only indentified taxa
## -----------------------------------------------------------
ems_prey %<>% filter(ems_taxa %in% diet_list, month %in% month_list)

## -----------------------------------------------------------
## Create a loop function to add zeros for all prey taxa missing from each region
## -----------------------------------------------------------
## Apply loop function
diet_missing <- data.frame(do.call(rbind,lapply(fid_list,function(i) {
  ## Filter catch by each serial
  fid2 <- ems_diet %>% filter(fid==i)
  serial <- unique(as.character(fid2$serial))
  region <- unique(as.character(fid2$region))
  ## True/false output if life stages does not exist (zero value life stages)
  pl <- diet_list[!diet_list %in% fid2$food_item]
  ## Determine the number of life stages to be added
  n <- length(pl)
  ## Create data frame with all zero value life stages, repeat by "n"
  tmp <- data.frame(fid=rep(i,n),serial=serial,region=region,food_item=pl,biomass=rep(0,n))
})))

## -----------------------------------------------------------
## Join zero data by serial to fill in other variables
## -----------------------------------------------------------
ems_diet_effort <- ems_diet %>% distinct(serial,month,region,basin) %>% select(serial,month,region,basin)
ems_diet_zero <- left_join(diet_missing,ems_diet_effort)
ems_diet_all <- bind_rows(ems_diet,ems_diet_zero) %>% arrange(fid)

## -----------------------------------------------------------
## Create a loop function to add zeros for all prey taxa missing from each serial
## -----------------------------------------------------------
## Apply loop function
envir_missing <-  data.frame(do.call(rbind,lapply(month_list,function(g){
  month <- ems_prey %>% filter(month == g)
  tmp <- data.frame(do.call(rbind,lapply(region_list,function(i) {
    ## Filter catch by each region
    region2 <- month %>% filter(region==i)
      ## True/false output if life stages does not exist (zero value life stages)
      pl <- diet_list[!diet_list %in% region2$ems_taxa]
      ## Determine the number of life stages to be added
      n <- length(pl)
      ## Create data frame with all zero value life stages, repeat by "n"
      data.frame(month=rep(g,n),region=rep(i,n),ems_taxa=pl,biomass=rep(0,n))
    })))
})))

## -----------------------------------------------------------
## Join zero data by serial to fill in other variables
## -----------------------------------------------------------
ems_prey_effort <- ems_prey %>% distinct(month,region,basin) %>% select(month,region,basin)
ems_prey_zero <- left_join(envir_missing,ems_prey_effort)
ems_prey_all <- bind_rows(ems_prey,ems_prey_zero) %>% arrange(month,region)

## -----------------------------------------------------------
## Calculate the proportion of prey available in the environment
## -----------------------------------------------------------
diet_prop <- as.data.frame(do.call(rbind,lapply(fid_list,function(i) {
  indiv <- ems_diet_all %>% filter(fid == i)
  prop <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round((filter(indiv,food_item == j)$biomass)/sum(indiv$biomass),4)
  })))
  data.frame(fid=rep(i,13),food_item=diet_list,diet_prop=prop)
}))) %>% 
  mutate(diet_prop=V1) %>% select(-V1)

ems_diet_prop <- left_join(ems_diet,diet_prop)

## -----------------------------------------------------------
## Calculate the proportion of prey available in the environment
## -----------------------------------------------------------
envir_may_prop <- as.data.frame(do.call(rbind,lapply(region_list,function(i) {
  indiv <- ems_prey_all %>% filter(region == i,month == "May")
  prop <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round((filter(indiv,ems_taxa == j)$biomass)/sum(indiv$biomass),4)
  })))
  data.frame(month=rep("May",13),region=rep(i,13),food_item=diet_list,envir_prop=prop)
}))) %>% 
  mutate(envir_prop=V1) %>% select(-V1)

envir_sept_prop <- as.data.frame(do.call(rbind,lapply(region_list,function(i) {
  indiv <- ems_prey_all %>% filter(region == i,month == "September")
  prop <- as.data.frame(do.call(rbind,lapply(diet_list,function(j) {
    round((filter(indiv,ems_taxa == j)$biomass)/sum(indiv$biomass),4)
  })))
  data.frame(month=rep("September",13),region=rep(i,13),food_item=diet_list,envir_prop=prop)
}))) %>% 
  mutate(envir_prop=V1) %>% select(-V1)

envir_prop_all <- bind_rows(envir_may_prop,envir_sept_prop)
ems_prey_select <- left_join(ems_diet_prop,envir_prop_all)

## -----------------------------------------------------------
## Divide proportion of prey available in the environment by proportion of prey in diet
## -----------------------------------------------------------
ems_prey_select %<>% mutate("diet/envir" = diet_prop/envir_prop)

## -----------------------------------------------------------
## Calculate alpha ((r/p)/sum(r/p))
## -----------------------------------------------------------
alpha <- data.frame(do.call(rbind,lapply(fid_list,function(k) {
  indiv <- ems_prey_select %>% filter(fid == k)
  serial <- unique(as.character(indiv$serial))
  month <- unique(as.character(indiv$month))
  basin <- unique(as.character(indiv$basin))
  taxa <- as.character(indiv$food_item)
  n <- nrow(indiv)
    tmp <- data.frame(do.call(rbind,lapply(1:n,function(p) {
      round(indiv[p,12]/sum(indiv[,12]),5)
    })))
  data.frame(fid=rep(k,n),serial,month,basin,taxa,alpha=tmp$diet.envir)
})))
## Remove NAs and summarize mean alpha by month, basin, and taxa
alpha %<>% filter(!is.na(alpha)) %>% 
  group_by(month,basin,taxa) %>% 
  summarize(alpha=mean(alpha))

## -----------------------------------------------------------
## Clean up environment
## -----------------------------------------------------------
rm(diet_missing,diet_prop,ems_diet_all,ems_diet_prop,ems_diet_zero,ems_diet_effort,
   ems_prey,ems_prey_all,ems_prey_select,ems_prey_zero,ems_prey_effort,envir_prop_all,
   envir_sept_prop,envir_may_prop,envir_missing,fid_list,prey_list,region_list,
   serial_list)

## -----------------------------------------------------------
## Rearrange data frame for paired t-test
## -----------------------------------------------------------
## Add zero values for consitent pairs among months and basins
## Apply first loop function
output1 <- data.frame(do.call(rbind,lapply(month_list,function(i) {
  ## Filter catch by each serial
  select_filt <- alpha %>% filter(month==i)
  ## Apply second loop function
  tmp <- lapply(basin_list,function(j) {
    ## Filter by each species and drop all other factor levels
    select_filt2 <- select_filt %>% filter(basin==j) %>% 
      droplevels()
    ## True/false output if life stages does not exist (zero value life stages)
    taxa <- diet_list[!diet_list %in% select_filt2$taxa]
    ## Determine the number of life stages to be added
    n <- length(taxa)
    ## Create data frame with all zero value life stages, repeat by "n"
    tmp <- data.frame(month=rep(i,n),basin=rep(j,n),taxa=taxa,alpha=rep(0,n))
  })
  ## Bind list into data frame
  tmp2 <- data.frame(do.call(rbind,tmp))
  
  ## Bind all serials
  select_all <- if((exists("select_all"))==F) {
    tmp2 } else {
      rbind(select_all,tmp2)
    }
  select_all
})))
## End double loop

## Bind all zero values with non-zero values
final_alpha <- bind_rows(alpha,output1) %>% 
  arrange(month,basin,taxa) %>% 
  mutate(basin=factor(basin,levels = c('Western','Central','Eastern'),ordered = TRUE))

## -----------------------------------------------------------
## paired t-test
## -----------------------------------------------------------
t.test(filter(final_alpha,basin=="Western",month=="May")$alpha,filter(final_alpha,basin=="Western",month=="September")$alpha,paired=T)
t.test(filter(final_alpha,basin=="Central",month=="May")$alpha,filter(final_alpha,basin=="Central",month=="September")$alpha,paired=T)
t.test(filter(final_alpha,basin=="Eastern",month=="May")$alpha,filter(final_alpha,basin=="Eastern",month=="September")$alpha,paired=T)

## -----------------------------------------------------------
## ANOVA
## -----------------------------------------------------------
alpha_taxa_lm <- lm(alpha~taxa,data=final_alpha)
anova(alpha_taxa_lm)
alpha_month_lm <- lm(alpha~taxa*month,data=final_alpha)
anova(alpha_month_lm)
alpha_basin_lm <- lm(alpha~taxa*basin,data=final_alpha)
anova(alpha_basin_lm)

final_alpha$grps <- final_alpha$month:final_alpha$taxa  # create single-factor interaction variable
alpha_lm2 <- lm(alpha~grps,data=final_alpha) # fit new one-way ANOVA model
alpha_mc2 <- glht(alpha_lm2,mcp(grps="Tukey"))
summary(alpha_mc2)
