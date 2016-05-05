##############################################################
##############################################################
##  EMS (Stewart et al.) manuscript
##
##  STABLE ISOTOPE SCRIPT
##
##############################################################
##############################################################
## ===========================================================
## Source Data_Init Script ... get ems.sia data.frame
## ===========================================================
source("data_init.R")
ems.sia

## -----------------------------------------------------------
## create an ordered factor for basin and month
## -----------------------------------------------------------
ems.sia %<>% mutate(basin=factor(basin,levels = c('Western','Central','Eastern'),ordered = TRUE),
                    month=factor(month))

##############################################################
## Energy density by weight regressions
##############################################################
## -----------------------------------------------------------
## linear models as a function of weight
## -----------------------------------------------------------
## Nitrogen AIC
aictab(cand.set=list(glm(d15N~wet.wt.g+month+basin,data=ems.sia),
                     glm(d15N~wet.wt.g*basin,data=ems.sia),
                     glm(d15N~wet.wt.g*month,data=ems.sia),
                     glm(d15N~wet.wt.g*basin*month,data=ems.sia),
                     glm(d15N~wet.wt.g,data=ems.sia)),
       modnames=c('No Interaction','Basin Only','Month Only','Basin and Month','Null'))

## Best fit according to AIC
## nitrogen~weight by month and basin
lm.n <- glm(d15N~wet.wt.g*basin*month,data=ems.sia)
summary(lm.n)
anova(lm.n)

## Carbon AIC
aictab(cand.set=list(glm(d13C~wet.wt.g+month+basin,data=ems.sia),
                     glm(d13C~wet.wt.g*basin,data=ems.sia),
                     glm(d13C~wet.wt.g*month,data=ems.sia),
                     glm(d13C~wet.wt.g*basin*month,data=ems.sia),
                     glm(d13C~wet.wt.g,data=ems.sia)),
       modnames=c('No Interaction','Basin Only','Month Only','Basin and Month','Null'))

## Best fit according to AIC
## carbon~weight by month and basin
lm.c <- glm(d13C~wet.wt.g*basin*month,data=ems.sia)
summary(lm.c)
anova(lm.c)

## -----------------------------------------------------------
## Tukey HSD
## -----------------------------------------------------------
## Nitrogen
summary(glht(lm.n,mcp(basin="Tukey")))
summary(glht(lm.n,mcp(month="Tukey")))

  ## The mean nitrogen in western basin was significantly lower than the mean nitrogen in eastern
  ## basin (p = 0.00111). Mean nitrogen was not significantly different in central and eastern 
  ## or western basins (p = 0.11512, 0.65045; respectively). Finally, the mean nitrogen was 
  ## not significant higher in September as compared to May (p < 0.503).

## Carbon
summary(glht(lm.c,mcp(basin="Tukey")))
summary(glht(lm.c,mcp(month="Tukey")))

  ## The mean nitrogen in eastern basin was significantly higher than the mean nitrogen in 
  ## central and western basins (p = 0.0282, <0.001; respectively). Mean nitrogen was not 
  ## significantly different in central and western basin (p = 0.8942). Finally, the mean nitrogen was 
  ## not significant higher in September as compared to May (p < 0.0513).

## -----------------------------------------------------------
## Calculate least-squares means
## -----------------------------------------------------------
## mean weight
mean.wt <- ems.sia %>% summarize(mean.wt=mean(wet.wt.g))

## NITROGEN
## create a reference grid from the fitted model (nitrogen)
lsm.n.grid <- ref.grid(lm.n,at=list(wet.wt.g=as.numeric(mean.wt)))
## least-squares means
lsm.n <- data.frame(print(lsmeans(lsm.n.grid,list(~month|basin)))) %>% 
  mutate(type="Nitrogen")
colnames(lsm.n) <- c('month','basin','lsmean','SE','df','lower.CL','upper.CL','type')

## CARBON
## create a reference grid from the fitted model (carbon)
lsm.c.grid <- ref.grid(lm.c,at=list(wet.wt.g=as.numeric(mean.wt)))
## least-squares means
lsm.c <- data.frame(print(lsmeans(lsm.c.grid,list(~month|basin)))) %>% 
  mutate(type="Carbon")
colnames(lsm.c) <- c('month','basin','lsmean','SE','df','lower.CL','upper.CL','type')

## Combine LSM data frames
lsm <- bind_rows(lsm.n,lsm.c) %>% 
  mutate(type=factor(type))

##############################################################
## Visualization
##############################################################
## -----------------------------------------------------------
## Save the plot as a figure (comment out line 273 and 289 until you are ready to save)
## -----------------------------------------------------------
png("figs/stable_isotope_lsm.PNG",width=9,height=7,units="in",family="Times",res=300)

## -----------------------------------------------------------
## Make plot
## -----------------------------------------------------------
ggplot(lsm,aes(basin,lsmean,group=month)) +
  geom_point(aes(group=month,shape=month),position=position_dodge(0.3),size=3) +
  geom_line(aes(group=month,linetype=month),position=position_dodge(0.3),size=0.65) +
  geom_errorbar(aes(x=basin,ymin=lower.CL,ymax=upper.CL),width=0.25,position=position_dodge(0.3),size=0.65) +
  #scale_y_continuous(limits=c(100,700),expand=c(0,0)) +
  labs(y='Wet-weight Energy Density (J/g)\n',x='') +
  scale_fill_grey(start=0.2,end=0.7) +
  scale_linetype_manual(values=c("solid","dotdash")) +
  theme(axis.text=element_text(size=18),axis.line.x=element_line(),axis.line.y=element_line(),
        legend.position='top',legend.text=element_text(size=15),legend.title=element_blank(),
        legend.key=element_rect(size=10,color='white'),legend.key.width=unit(2.5,'lines'),
        axis.title=element_text(size=22),axis.ticks.length=unit(1.75,'mm'),
        strip.background=element_blank(),strip.text=element_text(size=18),
        panel.background=element_blank(),plot.margin=unit(c(1,1,1,1),"mm")) +
  facet_wrap(~type,scales = "free")

## -----------------------------------------------------------
## Close the device to make the actual PNG file
## -----------------------------------------------------------
dev.off()
