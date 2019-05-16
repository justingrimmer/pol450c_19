rm(list=ls())

library(foreign)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(mgcv)

# load immigration data
load("immigration.RData")
summary(d)

# fit gam model
gam.out <- gam(imgpro5mod1~s(ppeduc,k=4)+s(ppage),data=d)
summary(gam.out)
plot(gam.out,residuals=F,shade=T)

# prediction
nd <- data.frame(ppeduc=mean(nd$ppeduc),ppage=seq(18,90,1))
pout <- predict.gam(gam.out,newdata=nd,se=TRUE)

nd$fit <- pout$fit
nd$se  <- pout$se
nd$ub  <- pout$fit + 2*pout$se
nd$lb  <- pout$fit - 2*pout$se

p <- ggplot(nd, aes(y=fit,x=ppage)) + geom_rug(position='jitter',col="chocolate1",sides="b") +
     geom_ribbon(aes(ymin=lb, ymax=ub,alpha=1)) + geom_line(aes(y=fit),col="red") +
     ylab("predicted level of pro-immigration (1-5)") + xlab("age") + 
     theme_economist(base_size=16) + theme(legend.position="none") 
p


# 3D plots
vis.gam(gam.out, view=c("ppeduc", "ppage"), type="response",
        ticktype="detailed", theta=-45, phi=20,plot.type="persp")

vis.gam(gam.out, view=c("ppeduc", "ppage"), type="response",
        ticktype="detailed", theta=-45, phi=20,plot.type="persp",se=2)

vis.gam(gam.out, view=c("ppeduc", "ppage"), type="response",
        ticktype="detailed", theta=100, phi=20,plot.type="persp")

vis.gam(gam.out, view=c("ppeduc", "ppage"), type="response",
    plot.type="contour")

# change smoothness
gam.out <- gam(imgpro5mod1~s(ppeduc,k=1)+s(ppage,1),data=d)
summary(gam.out)
plot(gam.out,residuals=F,shade=T)

# hybrid model
gam.out <- gam(imgpro5mod1~ppeduc+s(ppage),data=d)
summary(gam.out)
plot(gam.out,residuals=F,shade=T)

# interaction
gam.out <- gam(imgpro5mod1~s(ppeduc,ppage),data=d)
summary(gam.out)
plot(gam.out,residuals=F,shade=T)
plot(gam.out,residuals=F,shade=T,pers=T)

vis.gam(gam.out, view=c("ppeduc", "ppage"), type="response",
        ticktype="detailed", theta=-45, phi=20,plot.type="persp")

vis.gam(gam.out, view=c("ppeduc", "ppage"), type="response",
        ticktype="detailed", theta=-45, phi=20,plot.type="persp",se=2)

# interactions



