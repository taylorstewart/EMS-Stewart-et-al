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
## Interaction plots (two-way anova)
## -----------------------------------------------------------
## basin and month as independent categorical variables (non-transformed)
lm.interaction <- lm(wet.HOC.JG~basin*month,data=ems.cal)
summary(lm.interaction)
anova(lm.interaction)
lm.main <- lm(wet.HOC.JG~basin+month,data=ems.cal)
summary(lm.main)
fitPlot(lm.interaction,ylim=c(5500,9500),xlab="",ylab="Energy Density (J/g Wet Basis)")

  ## There is no evidence for an interaction effect between basin and month (p = 0.382210); 
  ## thus, the main effects can be interpreted directly. There appears to be a significant 
  ## difference in mean energy density among the different months (p < 0.001). 
  ## There also appears to be a significant difference between the three basins (p = 0.001).

summary(glht(lm.interaction,mcp(basin="Tukey")))
summary(glht(lm.interaction,mcp(month="Tukey")))
fitPlot(lm.interaction,which="basin")
addSigLetters(lm.interaction,which="basin",lets=c("a","b","b"),pos=c(2,2.8,4))
fitPlot(lm.interaction,which="month")
addSigLetters(lm.interaction,which="month",lets=c("a","b"),pos=c(2,4))

  ## The mean energy density in western basin was significantly lower than the mean energy 
  ## density in central and eastern basins (p = 0.0269, 0.0044; respectively). Mean energy density 
  ## was not significantly different in central and eastern basin (p = 0.9202). Finally, the mean 
  ## energy density was also significant higher in September as compared to May (p < 0.001).

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

## -----------------------------------------------------------
## linear models as a function of weight
## -----------------------------------------------------------
## AIC
aictab(cand.set=list(glm(wet.HOC.JG~wet.wt+month+basin,data=ems.cal),
                     glm(wet.HOC.JG~wet.wt*basin,data=ems.cal),
                     glm(wet.HOC.JG~wet.wt*month,data=ems.cal),
                     glm(wet.HOC.JG~wet.wt*basin*month,data=ems.cal)),
       modnames=c('No Interaction','Basin Only','Month Only','Basin and Month'))

## Best fit according to AIC
## hoc~weight by month
lm.wt <- lm(wet.HOC.JG~0+wet.wt+month+basin,data=ems.cal)
summary(lm.wt)
anova(lm.wt)
residPlot(lm.wt)
fitPlot(lm.wt)

## -----------------------------------------------------------
## Calculate least-squares means
## -----------------------------------------------------------
## mean weight
mean.wt <- ems.cal %>% summarize(mean.wt=mean(wet.wt))
## create a reference grid from the fitted model
lsm.grid <- ref.grid(lm.wt,at=list(wet.wt=as.numeric(mean.wt)))
## least-squares means
lsm <- data.frame(print(lsmeans(lsm.grid,list(~month|basin))))
colnames(lsm) <- c('month','basin','lsmean','SE','df','lower.CL','upper.CL')

  ## Can we define CL in lsmeans()? Is it 95%, 99%, etc.?

##############################################################
## Visualization
##############################################################
## -----------------------------------------------------------
## Save the plot as a figure (comment out line 273 and 289 until you are ready to save)
## -----------------------------------------------------------
png("figs/calorimetry_lsm.PNG",width=8,height=7,units="in",family="Times",res=300)

## -----------------------------------------------------------
## Make plot
## -----------------------------------------------------------
ggplot(lsm,aes(basin,lsmean,group=month)) +
  geom_point(aes(group=month,shape=month),position=position_dodge(0.3),size=3) +
  geom_line(aes(group=month,linetype=month),position=position_dodge(0.3),size=0.65) +
  geom_errorbar(aes(x=basin,ymin=lower.CL,ymax=upper.CL),width=0.25,position=position_dodge(0.3),size=0.65) +
  scale_y_continuous(limits=c(6000,9000),breaks=seq(6000,9000,500),expand=c(0,0)) +
  labs(y='Wet-weight Energy Density (J/g)\n',x='') +
  scale_fill_grey(start=0.2,end=0.7) +
  scale_linetype_manual(values=c("solid","dotdash")) +
  theme(axis.text=element_text(size=18),axis.line.x=element_line(),axis.line.y=element_line(),
        legend.position='top',legend.text=element_text(size=15),legend.title=element_blank(),
        legend.key=element_rect(size=10,color='white'),legend.key.width=unit(2.5,'lines'),
        axis.title=element_text(size=22),axis.ticks.length=unit(1.75,'mm'),
        panel.background=element_blank(),plot.margin=unit(c(1,1,1,1),"mm"))

## -----------------------------------------------------------
## Close the device to make the actual PNG file
## -----------------------------------------------------------
dev.off()
