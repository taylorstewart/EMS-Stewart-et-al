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

## create an ordered factor for basin
ems.sia %<>% mutate(basin=factor(basin,levels = c('Western','Central','Eastern'),ordered = TRUE))

## summarize mean N and C
ems.sia.mean <- ems.sia %>% group_by(month,basin) %>% 
  summarize(mean.n = mean(n.amount.ug),
            mean.c = mean(c.amount.ug))

## fit model and conduct ANOVA
## Nitrogen
lm.n <- lm(n.amount.ug~basin*month,data=ems.sia)
anova(lm.n)
## Multiple comparisons
## May
lm.n.may.multc <- glht(lm(n.amount.ug~basin,data=filter(ems.sia,month=='May')),mcp(basin="Tukey"))
summary(lm.n.may.multc)
## Sept
lm.n.sept.multc <- glht(lm(n.amount.ug~basin,data=filter(ems.sia,month=='Sept')),mcp(basin="Tukey"))
summary(lm.n.sept.multc)

## Carbon
lm.c <- lm(c.amount.ug~basin*month,data=ems.sia)
anova(lm.c)
## Multiple comparisons
## May
lm.c.multc <- glht(lm(c.amount.ug~basin,data=filter(ems.sia,month=='May')),mcp(basin="Tukey"))
summary(lm.c.multc)
## Sept
lm.c.multc <- glht(lm(c.amount.ug~basin,data=filter(ems.sia,month=='Sept')),mcp(basin="Tukey"))
summary(lm.c.multc)

## boxplot
ggplot(ems.sia,aes(basin,n.amount.ug)) +
  geom_boxplot() +
  labs(y="Nitrogen",x="") +
  theme_linedraw() +
  facet_wrap(~month)

ggplot(ems.sia,aes(basin,c.amount.ug)) +
  geom_boxplot() +
  labs(y="Carbon",x="") +
  theme_linedraw() +
  facet_wrap(~month)
