##############################################################
##############################################################
##  EMS (Stewart et al.) manuscript
##
##  ENERGY DENSITY ANALYSIS SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get ems.cal data.frame
## ===========================================================
source("data_init.R")
ems.cal

## -----------------------------------------------------------
## Exploratory linear-models
## -----------------------------------------------------------
## All basin/month combinations 
ggplot(filter(ems.cal,month=="May"),aes(log.wt,log.hoc,group=basin,linetype=month)) +
  geom_point(aes(colour=basin)) +
  geom_smooth(method="glm",aes(colour=basin)) +
  geom_point(data=filter(ems.cal,month=="September"),aes(colour=basin),pch=1) +
  geom_smooth(data=filter(ems.cal,month=="September"),method="glm",aes(colour=basin),lty=2) +
  ylab("Log Energy Density") +
  xlab("Log Weight") +
  theme_bw()

## months seperated by plots
ggplot(ems.cal,aes(log.wt,log.hoc,group=basin)) +
  geom_point(aes(colour=basin)) +
  geom_smooth(method="glm",aes(colour=basin)) +
  ylab("Log Energy Density") +
  xlab("Log Weight") +
  theme_bw() +
  facet_wrap(~month)

## months combined
ggplot(ems.cal,aes(log.wt,log.hoc,group=basin)) +
  geom_point(aes(colour=basin)) +
  geom_smooth(method="glm",aes(colour=basin)) +
  ylab("Log Energy Density") +
  xlab("Log Weight") +
  theme_bw()

## basins combined
ggplot(ems.cal,aes(log.wt,log.hoc,group=month)) +
  geom_point(aes(colour=month)) +
  geom_smooth(method="glm",aes(colour=month)) +
  ylab("Log Energy Density") +
  xlab("Log Weight") +
  theme_bw()

## -----------------------------------------------------------
## linear models (weight, basin, month)
## -----------------------------------------------------------
## hoc~weight by basin and month
lm.wt.basinmonth <- lm(wet.HOC.JG~log.wt*basin+month,data=ems.cal)
summary(lm.wt.basinmonth)
anova(lm.wt.basinmonth)
fitPlot(lm.wt.basinmonth)

## hoc~weight by month
lm.wt.month <- lm(wet.HOC.JG~log.wt*month,data=ems.cal)
summary(lm.wt.month)
anova(lm.wt.month)
fitPlot(lm.wt.month)

## hoc~weight by basin
lm.wt.basin <- lm(wet.HOC.JG~log.wt*basin,data=ems.cal)
summary(lm.wt.basin)
anova(lm.wt.basin)
fitPlot(lm.wt.basin)

## basin as independent categorical variable, month as covariate
lm.basin <- lm(wet.HOC.JG~basin*month,data=ems.cal)
summary(lm.basin)
anova(lm.basin)
fitPlot(lm.basin,ylim=c(5500,9500),xlab="",ylab="Energy Density (J/g Wet Basis)")

## month as independent categorical variable, basin as covariate
lm.month <- lm(wet.HOC.JG~month*basin,data=ems.cal)
summary(lm.month)
anova(lm.month)
fitPlot(lm.month,ylim=c(5500,9500),xlab="",ylab="Energy Density (J/g Wet Basis)",legend="bottomright")

## -----------------------------------------------------------
## multiple comparisons
## -----------------------------------------------------------
## Spatial comparison
## May
may.basin.multc <- glht(lm(wet.HOC.JG~basin,data=filter(ems.cal,month=='May')),mcp(basin="Tukey"))
summary(may.basin.multc)
## September
sept.basin.multc <- glht(lm(wet.HOC.JG~basin,data=filter(ems.cal,month=='September')),mcp(basin="Tukey"))
summary(sept.basin.multc)

## Seasonal comparison 
## Western
western.may.multc <- glht(lm(wet.HOC.JG~month,data=filter(ems.cal,basin=='Western')),mcp(month="Tukey"))
summary(western.may.multc)
## Central
western.may.multc <- glht(lm(wet.HOC.JG~month,data=filter(ems.cal,basin=='Central')),mcp(month="Tukey"))
summary(western.may.multc)
## Eastern
western.may.multc <- glht(lm(wet.HOC.JG~month,data=filter(ems.cal,basin=='Eastern')),mcp(month="Tukey"))
summary(western.may.multc)

## -----------------------------------------------------------
## Exploratory boxplots
## -----------------------------------------------------------
## by month
ggplot(ems.cal,aes(basin,wet.HOC.JG)) +
  geom_boxplot() +
  xlab("") +
  ylab("Energy Density (J/g Wet Basis)") +
  theme_bw() +
  facet_wrap(~month)

## by basin
ggplot(ems.cal,aes(month,wet.HOC.JG)) +
  geom_boxplot() +
  xlab("") +
  ylab("Energy Density (J/g Wet Basis)") +
  theme_bw() +
  facet_wrap(~basin)

