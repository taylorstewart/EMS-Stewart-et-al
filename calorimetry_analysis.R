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
ems_cal_may_west <- filter(ems_cal,basin=="Western",month=="May")
ems_cal_may_cen <- filter(ems_cal,basin=="Central",month=="May")
ems_cal_may_east <- filter(ems_cal,basin=="Eastern",month=="May")
ems_cal_may <- filter(ems_cal,month=="May")
ems_cal_sept_west <- filter(ems_cal,basin=="Western",month=="September")
ems_cal_sept_cen <- filter(ems_cal,basin=="Central",month=="September")
ems_cal_sept_east <- filter(ems_cal,basin=="Eastern",month=="September")
ems_cal_sept <- filter(ems_cal,month=="September")

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
srR_ds_may_west <- nls(log(wet_HOC_JG)~log(schnute(wet_wt,a,b,g)),data=ems_cal_may_west,start=list(a=7731.955125,b=1.160141,g=-1))
srR_ds_may_cen <- nls(log(wet_HOC_JG)~log(schnute(wet_wt,a,b,g)),data=ems_cal_may_cen,start=list(a=8868.94765,b=1.05957,g=-1))
srR_ds_may_east <- nls(log(wet_HOC_JG)~log(schnute(wet_wt,a,b,g)),data=ems_cal_may_east,start=list(a=8894.864979,b=1.086484,g=-1))
srR_ds_may <- nls(log(wet_HOC_JG)~log(schnute(wet_wt,a,b,g)),data=ems_cal_may,start=list(a=8299.652500,b=1.141984,g=-1))
srR_ds_sept_west <- nls(log(wet_HOC_JG)~log(schnute(wet_wt,a,b,g)),data=ems_cal_sept_west,start=list(a=10886.43,b=0.9063312,g=-1))
srR_ds_sept_cen <- nls(log(wet_HOC_JG)~log(schnute(wet_wt,a,b,g)),data=ems_cal_sept_cen,start=list(a=5737.511366,b=1.456952,g=-1))
srR_ds_sept_east <- nls(log(wet_HOC_JG)~log(schnute(wet_wt,a,b,g)),data=ems_cal_sept_east,start=list(a=10015.442104,b=1.007434,g=-1))
srR_ds_sept <- nls(log(wet_HOC_JG)~log(schnute(wet_wt,a,b,g)),data=ems_cal_sept,start=list(a=9594.835215,b=1.036805,g=-1))

## Bootstrap confidence intervals
bootR_may_west <- nlsBoot(srR_ds_may_west,niter = 1000)
bootR_may_cen <- nlsBoot(srR_ds_may_cen,niter = 1000)
bootR_may_east <- nlsBoot(srR_ds_may_east,niter = 1000)
bootR_sept_west <- nlsBoot(srR_ds_sept_west,niter = 1000)
bootR_sept_cen <- nlsBoot(srR_ds_sept_cen,niter = 1000)
bootR_sept_east <- nlsBoot(srR_ds_sept_east,niter = 1000)

ind <- srFuns("independence")
svI_may_west <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_may_west,type="independence")
svI_may_cen <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_may_cen,type="independence")
svI_may_east <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_may_east,type="independence")
svI_sept_west <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_sept_west,type="independence")
svI_sept_cen <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_sept_cen,type="independence")
svI_sept_east <- srStarts(wet_HOC_JG~wet_wt,data=ems_cal_sept_east,type="independence")

srI_may_west <- nls(log(wet_HOC_JG)~log(ind(wet_wt,a)),data=ems_cal_may_west,start=svI_may_west)
srI_may_cen <- nls(log(wet_HOC_JG)~log(ind(wet_wt,a)),data=ems_cal_may_cen,start=svI_may_cen)
srI_may_east <- nls(log(wet_HOC_JG)~log(ind(wet_wt,a)),data=ems_cal_may_east,start=svI_may_east)
srI_sept_west <- nls(log(wet_HOC_JG)~log(ind(wet_wt,a)),data=ems_cal_sept_west,start=svI_sept_west)
srI_sept_cen <- nls(log(wet_HOC_JG)~log(ind(wet_wt,a)),data=ems_cal_sept_cen,start=svI_sept_cen)
srI_sept_east <- nls(log(wet_HOC_JG)~log(ind(wet_wt,a)),data=ems_cal_sept_east,start=svI_sept_east)

## Gather the ANOVA results for a simple output
extraSS(srI_may_west,com=srR_may_west)
extraSS(srI_may_cen,com=srR_may_cen)
extraSS(srI_may_east,com=srR_may_east)
extraSS(srI_sept_west,com=srR_sept_west)
extraSS(srI_sept_cen,com=srR_sept_cen)
extraSS(srI_sept_east,com=srR_sept_east)

cor(schnute(ems_cal_may_west$wet_wt,a=coef(srR_ds_may_west)),ems_cal_may_west$wet_HOC_JG)^2
cor(schnute(ems_cal_may_cen$wet_wt,a=coef(srR_ds_may_cen)),ems_cal_may_cen$wet_HOC_JG)^2
cor(schnute(ems_cal_may_east$wet_wt,a=coef(srR_ds_may_east)),ems_cal_may_east$wet_HOC_JG)^2
cor(schnute(ems_cal_sept_west$wet_wt,a=coef(srR_ds_sept_west)),ems_cal_sept_west$wet_HOC_JG)^2
cor(schnute(ems_cal_sept_cen$wet_wt,a=coef(srR_ds_sept_cen)),ems_cal_sept_cen$wet_HOC_JG)^2
cor(schnute(ems_cal_sept_east$wet_wt,a=coef(srR_ds_sept_east)),ems_cal_sept_east$wet_HOC_JG)^2

x <- seq(0,8,length.out=199)            # many S for prediction
pR_may <- schnute(x,a=coef(srR_ds_may))               # predicted mean R
pR_may_west <- schnute(x,a=coef(srR_ds_may_west))     # predicted mean R
pR_may_cen <- schnute(x,a=coef(srR_ds_may_cen))       # predicted mean R
pR_may_east <- schnute(x,a=coef(srR_ds_may_east))     # predicted mean R
pR_sept <- schnute(x,a=coef(srR_ds_sept))             # predicted mean R
pR_sept_west <- schnute(x,a=coef(srR_ds_sept_west))   # predicted mean R
pR_sept_cen <- schnute(x,a=coef(srR_ds_sept_cen))     # predicted mean R
pR_sept_east <- schnute(x,a=coef(srR_ds_sept_east))   # predicted mean R
LCI <- UCI <- numeric(length(x))

for(i in 1:length(x)) {             # CIs for mean R @ each S
  tmp <- apply(bootR$coefboot,MARGIN=1,FUN=rckr,S=x[i])
  LCI[i] <- quantile(tmp,0.025)
  UCI[i] <- quantile(tmp,0.975)
}
ylmts_may <- range(c(0,ems_cal_may$wet_HOC_JG))
ylmts_sept <- range(c(0,ems_cal_sept$wet_HOC_JG))
xlmts <- range(x)

## May
plot(wet_HOC_JG~wet_wt,data=ems_cal_may,xlim=xlmts,ylim=ylmts_may,cex=1.5,col="white",
     ylab="Energy Density (J/g Wet Basis)",
     xlab="Wet Weight (g)",
     main="Deriso-Schnute")
#polygon(c(x,rev(x)),c(LCI,rev(UCI)),col="gray80",border=NA)
#polygon(c(x,rev(x)),c(LCI,rev(UCI)),col="gray80",border=NA)
#polygon(c(x,rev(x)),c(LCI,rev(UCI)),col="gray80",border=NA)
points(wet_HOC_JG~wet_wt,data=ems_cal_may_west,pch=19,cex=1.1,col=rgb(0,0,0,3/4))
points(wet_HOC_JG~wet_wt,data=ems_cal_may_cen,pch=19,cex=1.1,col=rgb(1,0,0,3/4))
points(wet_HOC_JG~wet_wt,data=ems_cal_may_east,pch=19,cex=1.1,col=rgb(0,0,1,3/4))
lines(pR_may_west~x,lwd=2,col=rgb(0,0,0))
lines(pR_may_cen~x,lwd=2,col=rgb(1,0,0))
lines(pR_may_east~x,lwd=2,col=rgb(0,0,1))
legend(x=7,y=13350,c("Western","Central","Eastern"),col=c(rgb(0,0,0),rgb(1,0,0),rgb(0,0,1)),cex=1.25,pch=16,pt.lwd=2,bty="n")

## September
plot(wet_HOC_JG~wet_wt,data=ems_cal_sept,xlim=xlmts,ylim=ylmts_sept,cex=1.5,col="white",
     ylab="Energy Density (J/g Wet Basis)",
     xlab="Wet Weight (g)",
     main="Deriso-Schnute")
#polygon(c(x,rev(x)),c(LCI,rev(UCI)),col="gray80",border=NA)
#polygon(c(x,rev(x)),c(LCI,rev(UCI)),col="gray80",border=NA)
#polygon(c(x,rev(x)),c(LCI,rev(UCI)),col="gray80",border=NA)
points(wet_HOC_JG~wet_wt,data=ems_cal_sept_west,pch=19,cex=1.1,col=rgb(0,0,0,3/4))
points(wet_HOC_JG~wet_wt,data=ems_cal_sept_cen,pch=19,cex=1.1,col=rgb(1,0,0,3/4))
points(wet_HOC_JG~wet_wt,data=ems_cal_sept_east,pch=19,cex=1.1,col=rgb(0,0,1,3/4))
lines(pR_sept_west~x,lwd=2,col=rgb(0,0,0))
lines(pR_sept_cen~x,lwd=2,col=rgb(1,0,0))
lines(pR_sept_east~x,lwd=2,col=rgb(0,0,1))
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
srR_cen <- nls(log(wet_HOC_JG)~log(rckr(wet_wt,a,b)),data=ems_cal_cen,start=svR_cen)
srR_east <- nls(log(wet_HOC_JG)~log(rckr(wet_wt,a,b)),data=ems_cal_east,start=svR_east)
srR <- nls(log(wet_HOC_JG)~log(rckr(wet_wt,a,b)),data=ems_cal,start=svR) ## for comparison

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
ggplot(ems_cal,aes(log_wt,log_hoc,group=basin)) +
  geom_point(aes(colour=basin)) +
  geom_smooth(method="glm",aes(colour=basin)) +
  ylab("Log Energy Density") +
  xlab("Log Weight") +
  theme_bw() +
  facet_wrap(~month)

ggplot(ems_cal,aes(log_tl,log_hoc,group=basin)) +
  geom_point(aes(colour=basin)) +
  geom_smooth(method="glm",aes(colour=basin)) +
  ylab("Log Energy Density") +
  xlab("Log Total Length") +
  theme_bw() +
  facet_wrap(~month)

## -----------------------------------------------------------
## interaction plots (basin+month)
## -----------------------------------------------------------
fit3 <- lm(wet_HOC_JG~basin+month,data=ems_cal)
coef(fit3)
anova(fit3)
residPlot(fit3)
fitPlot(fit3,which="basin",ylim=c(8000,11000),xlab="",ylab="Energy Density (J/g Wet Basis)")
fitPlot(fit3,which="month",ylim=c(8000,11000),xlab="",ylab="Energy Density (J/g Wet Basis)")
fitPlot(fit3,ylim=c(7000,12000),xlab="",ylab="Energy Density (J/g Wet Basis)")
fitPlot(fit3,change.order=TRUE,ylim=c(7000,12000),legend="topleft",xlab="",ylab="Energy Density (J/g Wet Basis)")

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
