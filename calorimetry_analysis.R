##############################################################
##############################################################
##  EMS (Taylor Stewart et al.) manuscript
##
##  ENERGY DENSITY ANALYSIS SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get ems_cal data.frame
## ===========================================================
source("data_init.R")
ems_cal

fit1 <- lm(wet_HOC_JG~wet_wt*basin,data=ems_cal)
coef(fit1)
anova(fit1)
fitPlot(fit1,legend="bottomright")

fit2 <- lm(wet_HOC_JG~wet_wt*month,data=ems_cal)
coef(fit2)
anova(fit2)
fitPlot(fit2,legend="bottomright")

fit3 <- lm(wet_HOC_JG~wet_wt*basin+month,data=ems_cal)
coef(fit3)
anova(fit3)
fitPlot(fit3,legend="bottomright")

fit4 <- lm(wet_HOC_JG~basin*month,data=ems_cal)
coef(fit4)
anova(fit4)
fitPlot(fit4,which="basin",ylim=c(8000,11000),xlab="",ylab="Energy Density (J/g Wet Basis)")
fitPlot(fit4,which="month",ylim=c(8000,11000),xlab="",ylab="Energy Density (J/g Wet Basis)")
fitPlot(fit4,ylim=c(7000,12000),xlab="",ylab="Energy Density (J/g Wet Basis)")
fitPlot(fit4,change.order=TRUE,ylim=c(7000,12000),legend="topleft",xlab="",ylab="Energy Density (J/g Wet Basis)")

ggplot(ems_cal,aes(basin,wet_HOC_JG)) +
  geom_boxplot() +
  xlab("") +
  ylab("Energy Density (J/g Wet Basis)") +
  theme_bw() +
  facet_wrap(~month)

ggplot(ems_cal,aes(month,wet_HOC_JG)) +
  geom_boxplot() +
  xlab("") +
  ylab("Energy Density (J/g Wet Basis") +
  theme_bw() +
  facet_wrap(~basin)
