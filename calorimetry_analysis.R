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

## ===========================================================
## Non-linear S-R Model HOC~WT*Basin (Deriso-Schnute Only)
## ===========================================================
ems_cal_west <- filter(ems_cal,basin=="Western")
ems_cal_cen <- filter(ems_cal,basin=="Central")
ems_cal_east <- filter(ems_cal,basin=="Eastern")

## define S-R function
schnute <- function(S,a,b,g) {
if (length(a) > 1) {
    g <- a[[3]]
    b <- a[[2]]
    a <- a[[1]]
  }
    a*S*(1-b*g*S)^(1/g)
}

## calculate non-linear model estimates
srR_west <- nls(log(wet_HOC_JG)~log(schnute(wet_wt,a,b,g)),data=ems_cal,start=list(a=13497.28,b=0.4493762,g=-1))
srR_cen <- nls(log(wet_HOC_JG)~log(schnute(wet_wt,a,b,g)),data=ems_cal_west,start=list(a=9102.114,b=0.295015,g=-1))
srR_east <- nls(log(wet_HOC_JG)~log(schnute(wet_wt,a,b,g)),data=ems_cal_cen,start=list(a=13344.53,b=0.4542794,g=-1))
srR <- nls(log(wet_HOC_JG)~log(schnute(wet_wt,a,b,g)),data=ems_cal_east,start=list(a=11460.3,b=0.3748594,g=-1))

## Bootstrap confidence intervals
bootR_west <- nlsBoot(srR_west,niter = 1000)
bootR_cen <- nlsBoot(srR_cen,niter = 1000)
bootR_east <- nlsBoot(srR_east,niter = 1000)

ind <- srFuns("independence")
svI_west <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_west,type="independence")
svI_cen <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_cen,type="independence")
svI_east <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_east,type="independence")

srI_west <- nls(log(wet_HOC_JG)~log(ind(wet_wt,a)),data=ems_cal_west,start=svI_west)
srI_cen <- nls(log(wet_HOC_JG)~log(ind(wet_wt,a)),data=ems_cal_cen,start=svI_cen)
srI_east <- nls(log(wet_HOC_JG)~log(ind(wet_wt,a)),data=ems_cal_east,start=svI_east)

## Gather the ANOVA results for a simple output
extraSS(srI_west,com=srR_west)
extraSS(srI_cen,com=srR_cen)
extraSS(srI_east,com=srR_east)

cor(schnute(ems_cal_west$wet_wt,a=coef(srR_west)),ems_cal_west$wet_HOC_JG)^2
cor(schnute(ems_cal_cen$wet_wt,a=coef(srR_cen)),ems_cal_cen$wet_HOC_JG)^2
cor(schnute(ems_cal_east$wet_wt,a=coef(srR_east)),ems_cal_east$wet_HOC_JG)^2

x <- seq(0,8,length.out=199)            # many S for prediction
pR <- schnute(x,a=coef(srR))               # predicted mean R
pR_west <- schnute(x,a=coef(srR_west))     # predicted mean R
pR_cen <- schnute(x,a=coef(srR_cen))       # predicted mean R
pR_east <- schnute(x,a=coef(srR_east))     # predicted mean R
LCI <- UCI <- numeric(length(x))

for(i in 1:length(x)) {             # CIs for mean R @ each S
  tmp <- apply(bootR$coefboot,MARGIN=1,FUN=rckr,S=x[i])
  LCI[i] <- quantile(tmp,0.025)
  UCI[i] <- quantile(tmp,0.975)
}
ylmts <- range(ems_cal$wet_HOC_JG)
xlmts <- range(x)

plot(wet_HOC_JG~wet_wt,data=ems_cal,xlim=xlmts,ylim=ylmts,cex=1.5,col="white",
     ylab="Energy Density (J/g Wet Basis)",
     xlab="Wet Weight (g)",
     main="Deriso-Schnute")
#polygon(c(x,rev(x)),c(LCI,rev(UCI)),col="gray80",border=NA)
#polygon(c(x,rev(x)),c(LCI,rev(UCI)),col="gray80",border=NA)
#polygon(c(x,rev(x)),c(LCI,rev(UCI)),col="gray80",border=NA)
points(wet_HOC_JG~wet_wt,data=ems_cal_west,pch=19,cex=1.1,col=rgb(0,0,0,3/4))
points(wet_HOC_JG~wet_wt,data=ems_cal_cen,pch=19,cex=1.1,col=rgb(1,0,0,3/4))
points(wet_HOC_JG~wet_wt,data=ems_cal_east,pch=19,cex=1.1,col=rgb(0,0,1,3/4))
lines(pR_west~x,lwd=2,col=rgb(0,0,0))
lines(pR_cen~x,lwd=2,col=rgb(1,0,0))
lines(pR_east~x,lwd=2,col=rgb(0,0,1))
legend(x=7,y=13350,c("Western","Central","Eastern"),col=c(rgb(0,0,0),rgb(1,0,0),rgb(0,0,1)),cex=1.25,pch=16,pt.lwd=2,bty="n")

## ===========================================================
## Non-linear S-R Model HOC~WT*Basin
## ===========================================================
ems_cal_west <- filter(ems_cal,basin=="Western")
ems_cal_cen <- filter(ems_cal,basin=="Central")
ems_cal_east <- filter(ems_cal,basin=="Eastern")

## define S-R function to use ("BevertonHolt","Ricker","Shepherd","SailaLorda","independence")
model <- "BevertonHolt"

## Establish starting values for nls()
rckr <- srFuns(model)
( svR_west <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_west,type=model) )
( svR_cen <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_cen,type=model) )
( svR_east <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_east,type=model) )
( svR <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal,type=model) ) ## for comparison

## calculate non-linear model estimates
srR_west <- nls(log(wet_HOC_JG)~log(rckr(wet_wt,a,b)),data=ems_cal_west,start=svR_cen)
residPlot(srR_west)
srR_cen <- nls(log(wet_HOC_JG)~log(rckr(wet_wt,a,b)),data=ems_cal_cen,start=svR_cen)
residPlot(srR_cen)
srR_east <- nls(log(wet_HOC_JG)~log(rckr(wet_wt,a,b)),data=ems_cal_east,start=svR_east)
residPlot(srR_east)
srR <- nls(log(wet_HOC_JG)~log(rckr(wet_wt,a,b)),data=ems_cal,start=svR) ## for comparison
extraSS(srR_west,srR_cen,srR_east,com=srR)

## Bootstrap confidence intervals
#bootR_west <- nlsBoot(srR_west,niter = 1000)
#bootR_cen <- nlsBoot(srR_cen,niter = 1000)
#bootR_east <- nlsBoot(srR_east,niter = 1000)

ind <- srFuns("independence")
svI_west <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_west,type="independence")
svI_cen <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_cen,type="independence")
svI_east <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_east,type="independence")

srI_west <- nls(log(wet_HOC_JG)~log(ind(wet_wt,a)),data=ems_cal_west,start=svI_west)
srI_cen <- nls(log(wet_HOC_JG)~log(ind(wet_wt,a)),data=ems_cal_cen,start=svI_cen)
srI_east <- nls(log(wet_HOC_JG)~log(ind(wet_wt,a)),data=ems_cal_east,start=svI_east)

## Gather the ANOVA results for a simple output
extraSS(srI_west,com=srR_west)
extraSS(srI_cen,com=srR_cen)
extraSS(srI_east,com=srR_east)

cor(rckr(ems_cal_west$wet_wt,a=coef(srR_west)),ems_cal_west$wet_HOC_JG)^2
cor(rckr(ems_cal_cen$wet_wt,a=coef(srR_cen)),ems_cal_cen$wet_HOC_JG)^2
cor(rckr(ems_cal_east$wet_wt,a=coef(srR_east)),ems_cal_east$wet_HOC_JG)^2

x <- seq(0,8,length.out=199)            # many S for prediction
pR_west <- rckr(x,a=coef(srR_west))     # predicted mean R
pR_cen <- rckr(x,a=coef(srR_cen))       # predicted mean R
pR_east <- rckr(x,a=coef(srR_east))     # predicted mean R
LCI <- UCI <- numeric(length(x))

for(i in 1:length(x)) {             # CIs for mean R @ each S
  tmp <- apply(bootR$coefboot,MARGIN=1,FUN=rckr,S=x[i])
  LCI[i] <- quantile(tmp,0.025)
  UCI[i] <- quantile(tmp,0.975)
}
ylmts <- range(ems_cal$wet_HOC_JG)
xlmts <- range(x)

plot(wet_HOC_JG~wet_wt,data=ems_cal,xlim=xlmts,ylim=ylmts,cex=1.5,col="white",
     ylab="Energy Density (J/g Wet Basis)",
     xlab="Wet Weight (g)",
     main=model)
#polygon(c(x,rev(x)),c(LCI,rev(UCI)),col="gray80",border=NA)
#polygon(c(x,rev(x)),c(LCI,rev(UCI)),col="gray80",border=NA)
#polygon(c(x,rev(x)),c(LCI,rev(UCI)),col="gray80",border=NA)
points(wet_HOC_JG~wet_wt,data=ems_cal_west,pch=19,cex=1.1,col=rgb(0,0,0,1/2))
points(wet_HOC_JG~wet_wt,data=ems_cal_cen,pch=19,cex=1.1,col=rgb(1,0,0,1/2))
points(wet_HOC_JG~wet_wt,data=ems_cal_east,pch=19,cex=1.1,col=rgb(0,0,1,1/2))
lines(pR_west~x,lwd=2,col=rgb(0,0,0))
lines(pR_cen~x,lwd=2,col=rgb(1,0,0))
lines(pR_east~x,lwd=2,col=rgb(0,0,1))
legend(x=7,y=13350,c("Western","Central","Eastern"),col=c(rgb(0,0,0),rgb(1,0,0),rgb(0,0,1)),cex=1.25,pch=16,pt.lwd=2,bty="n")

## ===========================================================
## Non-linear S-R Model HOC~WT*Month
## ===========================================================
ems_cal_may <- filter(ems_cal,month=="May")
ems_cal_sept <- filter(ems_cal,month=="September")

## define S-R function to use ("BevertonHolt","Ricker","Shepherd","SailaLorda","independence")
model <- "Ricker"

## Establish starting values for nls()
rckr <- srFuns(model,msg=T)
( svR_may <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_may,type=model) )
( svR_sept <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_sept,type=model) )

## calculate non-linear model estimates
srR_may <- nls(log(wet_HOC_JG)~log(rckr(wet_wt,a,b)),data=ems_cal_may,start=svR_may)
residPlot(srR_may)
srR_sept <- nls(log(wet_HOC_JG)~log(rckr(wet_wt,a,b)),data=ems_cal_sept,start=svR_sept)
residPlot(srR_sept)

cbind(estimates=coef(srR_may),confint(srR_may))
cbind(estimates=coef(srR_sept),confint(srR_sept))

#bootR <- nlsBoot(srR,niter = 5)
#cbind(estimates=coef(srR),confint(bootR))
#pR <- apply(bootR$coefboot,MARGIN=1,FUN=rckr,S=2)
#quantile(pR,c(0.025,0.975))

ind <- srFuns("independence")
svI_may <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_may,type="independence")
svI_sept <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_sept,type="independence")

srI_may <- nls(log(wet_HOC_JG)~log(ind(wet_wt,a)),data=ems_cal_may,start=svI_may)
srI_sept <- nls(log(wet_HOC_JG)~log(ind(wet_wt,a)),data=ems_cal_sept,start=svI_sept)

extraSS(srI_may,com=srR_may)
extraSS(srI_sept,com=srR_sept)

cor(rckr(ems_cal_may$wet_wt,a=coef(srR_may)),ems_cal_may$wet_HOC_JG)^2
cor(rckr(ems_cal_sept$wet_wt,a=coef(srR_sept)),ems_cal_sept$wet_HOC_JG)^2

x <- seq(0,8,length.out=199)             # many S for prediction
pR_may <- rckr(x,a=coef(srR_may))        # predicted mean R
pR_sept <- rckr(x,a=coef(srR_sept))      # predicted mean R
LCI <- UCI <- numeric(length(x))

#for(i in 1:length(x)) {             # CIs for mean R @ each S
#  tmp <- apply(bootR$coefboot,MARGIN=1,FUN=rckr,S=x[i])
#  LCI[i] <- quantile(tmp,0.025)
#  UCI[i] <- quantile(tmp,0.975)
#}
ylmts <- range(ems_cal$wet_HOC_JG)
xlmts <- range(x)

plot(wet_HOC_JG~wet_wt,data=ems_cal,xlim=xlmts,ylim=ylmts,cex=1.5,col="white",
     ylab="Energy Density (J/g Wet Basis)",
     xlab="Wet Weight (g)",
     main=model)
#polygon(c(x,rev(x)),c(LCI,rev(UCI)),col="gray80",border=NA)
#polygon(c(x,rev(x)),c(LCI,rev(UCI)),col="gray80",border=NA)
#polygon(c(x,rev(x)),c(LCI,rev(UCI)),col="gray80",border=NA)
points(wet_HOC_JG~wet_wt,data=ems_cal_may,pch=19,cex=1.1,col=rgb(0,0,0,1/2))
points(wet_HOC_JG~wet_wt,data=ems_cal_sept,pch=19,cex=1.1,col=rgb(1,0,0,1/2))
lines(pR_may~x,lwd=2,col=rgb(0,0,0))
lines(pR_sept~x,lwd=2,col=rgb(1,0,0))
legend(x=6.85,y=13350,c("May","September"),col=c(rgb(0,0,0),rgb(1,0,0)),cex=1.25,pch=16,pt.lwd=2,bty="n")

## -----------------------------------------------------------
## Exploratory linear-models
## -----------------------------------------------------------
## log(weight)*basin
fit1 <- lm(log_hoc~log_wt*basin,data=ems_cal)
coef(fit1)
anova(fit1)
residPlot(fit1)
fitPlot(fit1,legend="bottomright")

## log(weight)*month
fit2 <- lm(log_hoc~log_wt*month,data=ems_cal)
coef(fit2)
anova(fit2)
residPlot(fit2)
fitPlot(fit2,legend="bottomright")

## log(tl)*basin
fit3 <- lm(log_hoc~log_tl*basin,data=ems_cal)
coef(fit3)
anova(fit3)
residPlot(fit3)
fitPlot(fit3,legend="bottomright")

## log(tl)*month
fit4 <- lm(log_hoc~log_tl*month,data=ems_cal)
coef(fit4)
anova(fit4)
residPlot(fit4)
fitPlot(fit4,legend="bottomright")

## log(tl)*basin*month
fit5 <- lm(log_hoc~log_tl*basin*month,data=ems_cal)
coef(fit5)
anova(fit5)
residPlot(fit5)
fitPlot(fit5,legend="bottomright")

## basin*month
fit6 <- lm(wet_HOC_JG~basin*month,data=ems_cal)
coef(fit6)
anova(fit6)
residPlot(fit6)
fitPlot(fit6,which="basin",ylim=c(8000,11000),xlab="",ylab="Energy Density (J/g Wet Basis)")
fitPlot(fit6,which="month",ylim=c(8000,11000),xlab="",ylab="Energy Density (J/g Wet Basis)")
fitPlot(fit6,ylim=c(7000,12000),xlab="",ylab="Energy Density (J/g Wet Basis)")
fitPlot(fit6,change.order=TRUE,ylim=c(7000,12000),legend="topleft",xlab="",ylab="Energy Density (J/g Wet Basis)")

## -----------------------------------------------------------
## Exploratory boxplots
## -----------------------------------------------------------
ggplot(ems_cal,aes(basin,wet_HOC_JG)) +
  geom_boxplot() +
  xlab("") +
  ylab("Energy Density (J/g Wet Basis)") +
  theme_bw() +
  facet_wrap(~month)

ggplot(ems_cal,aes(month,wet_HOC_JG)) +
  geom_boxplot() +
  xlab("") +
  ylab("Energy Density (J/g Wet Basis)") +
  theme_bw() +
  facet_wrap(~basin)
