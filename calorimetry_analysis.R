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
## All basin/month combinations (log transformed)
ggplot(filter(ems.cal,month=="May"),aes(log.wt,log.hoc,group=basin,linetype=month)) +
  geom_point(aes(colour=basin)) +
  geom_smooth(method="glm",aes(colour=basin)) +
  geom_point(data=filter(ems.cal,month=="September"),aes(colour=basin),pch=1) +
  geom_smooth(data=filter(ems.cal,month=="September"),method="glm",aes(colour=basin),lty=2) +
  ylab("Log Energy Density") +
  xlab("Log Weight") +
  theme_linedraw()
## All basin/month combinations (non-transformed)
ggplot(filter(ems.cal,month=="May"),aes(wet.wt,wet.HOC.JG,group=basin,linetype=month)) +
  geom_point(aes(colour=basin)) +
  geom_smooth(method="glm",aes(colour=basin)) +
  geom_point(data=filter(ems.cal,month=="September"),aes(colour=basin),pch=1) +
  geom_smooth(data=filter(ems.cal,month=="September"),method="glm",aes(colour=basin),lty=2) +
  ylab("Wet-weight Energy Density (J/g)") +
  xlab("Wet-weight (g)") +
  theme_linedraw()

## months seperated by plots (log transformed)
ggplot(ems.cal,aes(log.wt,log.hoc,group=basin)) +
  geom_point(aes(colour=basin)) +
  geom_smooth(method="glm",aes(colour=basin)) +
  ylab("Log Energy Density") +
  xlab("Log Weight") +
  theme_linedraw() +
  facet_wrap(~month)
## months seperated by plots (non-transformed)
ggplot(ems.cal,aes(wet.wt,wet.HOC.JG,group=basin)) +
  geom_point(aes(colour=basin)) +
  geom_smooth(method="glm",aes(colour=basin)) +
  ylab("Wet Energy Density (J/g)") +
  xlab("Wet-weight (g)") +
  theme_linedraw() +
  facet_wrap(~month)

## months combined (log transformed)
ggplot(ems.cal,aes(log.wt,log.hoc,group=basin)) +
  geom_point(aes(colour=basin)) +
  geom_smooth(method="glm",aes(colour=basin)) +
  ylab("Log Energy Density") +
  xlab("Log Weight") +
  theme_linedraw()
## months combined (non-transformed)
ggplot(ems.cal,aes(wet.wt,wet.HOC.JG,group=basin)) +
  geom_point(aes(colour=basin)) +
  geom_smooth(method="glm",aes(colour=basin)) +
  ylab("Log Energy Density") +
  xlab("Log Weight") +
  theme_linedraw()

## basins combined (log-transformed)
ggplot(ems.cal,aes(log.wt,log.hoc,group=month)) +
  geom_point(aes(colour=month)) +
  geom_smooth(method="glm",aes(colour=month)) +
  ylab("Log Energy Density") +
  xlab("Log Weight") +
  theme_linedraw()
## basins combined (non-transformed)
ggplot(ems.cal,aes(wet.wt,wet.HOC.JG,group=month)) +
  geom_point(aes(colour=month)) +
  geom_smooth(method="glm",aes(colour=month)) +
  ylab("Log Energy Density") +
  xlab("Log Weight") +
  theme_linedraw()

## -----------------------------------------------------------
## Interaction plots
## -----------------------------------------------------------
## basin as independent categorical variable (non-transformed)
lm.basin <- lm(wet.HOC.JG~basin*month,data=ems.cal)
summary(lm.basin)
anova(lm.basin)
fitPlot(lm.basin,ylim=c(5500,9500),xlab="",ylab="Energy Density (J/g Wet Basis)")

## month as independent categorical variable (non transformed)
lm.month <- lm(wet.HOC.JG~month*basin,data=ems.cal)
summary(lm.month)
anova(lm.month)
fitPlot(lm.month,ylim=c(5500,9500),xlab="",ylab="Wet-Weight Energy Density (J/g)",legend="bottomright")

## -----------------------------------------------------------
## linear models (weight, basin, month)
## -----------------------------------------------------------
## hoc~weight by basin and month (log transformed)
lm.log.wt.basinmonth <- lm(log.hoc~log.wt*basin+month,data=ems.cal)
summary(lm.log.wt.basinmonth)
anova(lm.log.wt.basinmonth)
fitPlot(lm.log.wt.basinmonth)
## hoc~weight by basin and month (non-transformed)
lm.wt.basinmonth <- lm(wet.HOC.JG~wet.wt*basin+month,data=ems.cal)
summary(lm.wt.basinmonth)
anova(lm.wt.basinmonth)
fitPlot(lm.wt.basinmonth)

## hoc~weight by month (log transformed)
lm.log.wt.month <- lm(log.hoc~log.wt*month,data=ems.cal)
summary(lm.log.wt.month)
anova(lm.log.wt.month)
fitPlot(lm.log.wt.month)
## hoc~weight by month (non-transformed)
lm.wt.month <- lm(wet.HOC.JG~wet.wt*month,data=ems.cal)
summary(lm.wt.month)
anova(lm.wt.month)
fitPlot(lm.wt.month)

## hoc~weight by basin (log transformed)
lm.log.wt.basin <- lm(log.hoc~log.wt*basin,data=ems.cal)
summary(lm.log.wt.basin)
anova(lm.log.wt.basin)
fitPlot(lm.log.wt.basin)
## hoc~weight by basin (non-transformed)
lm.wt.basin <- lm(wet.HOC.JG~wet.wt*basin,data=ems.cal)
summary(lm.wt.basin)
anova(lm.wt.basin)
fitPlot(lm.wt.basin)

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
central.may.multc <- glht(lm(wet.HOC.JG~month,data=filter(ems.cal,basin=='Central')),mcp(month="Tukey"))
summary(central.may.multc)
## Eastern
eastern.may.multc <- glht(lm(wet.HOC.JG~month,data=filter(ems.cal,basin=='Eastern')),mcp(month="Tukey"))
summary(eastern.may.multc)

## -----------------------------------------------------------
## Exploratory boxplots
## -----------------------------------------------------------
## by month
ggplot(ems.cal,aes(basin,wet.HOC.JG)) +
  geom_boxplot() +
  xlab("") +
  ylab("Energy Density (J/g Wet Basis)") +
  theme_linedraw() +
  facet_wrap(~month)

## by basin
ggplot(ems.cal,aes(month,wet.HOC.JG)) +
  geom_boxplot() +
  xlab("") +
  ylab("Energy Density (J/g Wet Basis)") +
  theme_linedraw() +
  facet_wrap(~basin)

## -----------------------------------------------------------
## Linear Regression Coefficients
## -----------------------------------------------------------
summary(lm(log.hoc~log.wt,data=filter(ems.cal,month=="May",basin=="Western")))
summary(lm(wet.HOC.JG~wet.wt,data=filter(ems.cal,month=="May",basin=="Western")))

summary(lm(log.hoc~log.wt,data=filter(ems.cal,month=="May",basin=="Central")))
summary(lm(wet.HOC.JG~wet.wt,data=filter(ems.cal,month=="May",basin=="Central")))

summary(lm(log.hoc~log.wt,data=filter(ems.cal,month=="May",basin=="Eastern")))
summary(lm(wet.HOC.JG~wet.wt,data=filter(ems.cal,month=="May",basin=="Eastern")))

summary(lm(log.hoc~log.wt,data=filter(ems.cal,month=="September",basin=="Western")))
summary(lm(wet.HOC.JG~wet.wt,data=filter(ems.cal,month=="September",basin=="Western")))

summary(lm(log.hoc~log.wt,data=filter(ems.cal,month=="September",basin=="Central")))
summary(lm(wet.HOC.JG~wet.wt,data=filter(ems.cal,month=="September",basin=="Central")))

summary(lm(log.hoc~log.wt,data=filter(ems.cal,month=="September",basin=="Eastern")))
summary(lm(wet.HOC.JG~wet.wt,data=filter(ems.cal,month=="September",basin=="Eastern")))

