##########################################################
## LapseRateFigure2.R
## Project: HI Temp
## Author: Aurora Kagawa-Viviani
## Date: 5/30/2019
## Notes: Script for looking at lapse rate trends:
#      - breakpoint analysis
#      - are trends driven by certain periods?
#   Bring in sounding index data, ceilometer, look at trends
#   Bring in sounding profile data, look at trends


setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping")

library(xts)
library(Kendall)
library(zyp)

# Fitted temperature indices
Tdat<-read.csv("annual_lm_2017.csv", stringsAsFactors = F)
Tdat$Year<-Tdat$coefs_Year
Tdat<-Tdat[,-c(1,2)]
Tdat[,grep(".m", names(Tdat), fixed=T)]<-Tdat[,grep(".m", names(Tdat), fixed=T)]*(-1000)  # C/km

# Soundings
sHilo<-read.csv("NOT_USED/AKV_Sonde/UWyo/Hilo_monthly_medians.csv", stringsAsFactors = F)
sLih<-read.csv("NOT_USED/AKV_Sonde/UWyo/Lihue_monthly_medians.csv", stringsAsFactors = F)
sHilo.xts<-xts(sHilo[,-1], order.by=as.Date(sHilo$X, format="%m/%d/%Y"))
sLih.xts<-xts(sLih[,-1], order.by=as.Date(sLih$X))

for (i in 1:48){
  L<-apply.yearly(sLih.xts[,i], mean, na.rm=T)
  H<-apply.yearly(sHilo.xts[,i], mean, na.rm=T)
  
  if(i==1){
    L.yearly.all<-L
    H.yearly.all<-H
    
  }else{
    L.yearly.all<-merge(L.yearly.all, L)
    H.yearly.all<-merge(H.yearly.all, H)
  }}


sLih<-as.data.frame(L.yearly.all)
sHilo<-as.data.frame(H.yearly.all)

sLih$Year<-as.numeric(substr(index(L.yearly.all), 1, 4))
sHilo$Year<-as.numeric(substr(index(H.yearly.all), 1, 4))


# Ceilometer
ceil<-read.csv("NOT_USED/ASOS_Cloud/CeilometerOB_all_yearly.csv", stringsAsFactors = F)
ceil$Year<-as.numeric(substr(ceil$X, 1, 4))

cphto<-ceil[,grep("phto", names(ceil))]
cphto$Year<-ceil$Year 

cphli<-ceil[,grep("phli", names(ceil))]
cphli$Year<-ceil$Year

# Sounding layers information 
RH_Hilo.00<-read.csv("NOT_USED/AKV_Sonde/UWyo/RH_layers_Hilo.csv", stringsAsFactors = F)
RH_Lih.00<-read.csv("NOT_USED/AKV_Sonde/UWyo/RH_layers_Lihue.csv", stringsAsFactors = F)

RH_Hilo.12<-read.csv("NOT_USED/AKV_Sonde/UWyo/RH_layers_Hilo_12Z.csv", stringsAsFactors = F)
RH_Lih.12<-read.csv("NOT_USED/AKV_Sonde/UWyo/RH_layers_Lihue_12Z.csv", stringsAsFactors = F)

RH_Hilo.00$Year<-as.numeric(substr(RH_Hilo.00$X, 1, 4))
RH_Lih.00$Year<-as.numeric(substr(RH_Lih.00$X, 1, 4))

RH_Hilo.12$Year<-as.numeric(substr(RH_Hilo.12$X, 1, 4))
RH_Lih.12$Year<-as.numeric(substr(RH_Lih.12$X, 1, 4))

RH_Hilo<-merge(RH_Hilo.00, RH_Hilo.12, by="Year", suffixes = c("H00", "H12"))
RH_Lih<-merge(RH_Lih.00, RH_Lih.12, by="Year", suffixes = c("L00", "L12"))

# Sounding layers information 400m slices
rh<-read.csv("NOT_USED/AKV_Sonde/UWyo/relh.profileTS.csv", stringsAsFactors = F)
temp<-read.csv("NOT_USED/AKV_Sonde/UWyo/temp.profileTS.csv", stringsAsFactors = F)
mixr<-read.csv("NOT_USED/AKV_Sonde/UWyo/mixr.profileTS.csv", stringsAsFactors = F)
sknt<-read.csv("NOT_USED/AKV_Sonde/UWyo/sknt.profileTS.csv", stringsAsFactors = F)

# Sounding Profile trends
relh.tr<-read.csv("NOT_USED/AKV_Sonde/UWyo/relh.trends2.csv", stringsAsFactors = F)
temp.tr<-read.csv("NOT_USED/AKV_Sonde/UWyo/temp.trends2.csv", stringsAsFactors = F)
mixr.tr<-read.csv("NOT_USED/AKV_Sonde/UWyo/mixr.trends2.csv", stringsAsFactors = F)
sknt.tr<-read.csv("NOT_USED/AKV_Sonde/UWyo/sknt.trends2.csv", stringsAsFactors = F)


####### Breakpoints:
library(segmented)
Tdat.58<-Tdat[Tdat$Year>=1958,]
lm.minm.58<-lm(coefs_Tmin.m~Year, Tdat.58)
seg.minm.58<-segmented(lm.minm.58, seg.Z=~Year)
seg.minm.58  # breakpoint 2016

lm.maxm.58<-lm(coefs_Tmax.m~Year, Tdat.58)
seg.maxm.58<-segmented(lm.maxm.58, seg.Z=~Year)
seg.maxm.58  # breakpoint 1993

lm.minb.58<-lm(coefs_Tmin.b~Year, Tdat.58)
seg.minb.58<-segmented(lm.minb.58, seg.Z=~Year)
seg.minb.58  # breakpoint 2013

lm.maxb.58<-lm(coefs_Tmax.b~Year, Tdat.58)
seg.maxb.58<-segmented(lm.maxb.58, seg.Z=~Year)
seg.maxb.58  # breakpoint 1984

####### Focus on overlapping period 1978-2017
Tdat.78<-Tdat[Tdat$Year>=1978,]

sHilo.78<-sHilo[sHilo$Year>=1978 & sHilo$Year<=2017,]
sLih.78<-sLih[sLih$Year>=1978 & sHilo$Year<=2017,]

cphto.78<-cphto[cphto$Year>=1978 & cphto$Year<=2017,]
cphli.78<-cphli[cphli$Year>=1978& cphli$Year<=2017,]

RH_Hilo.78<-RH_Hilo[RH_Hilo$Year>=1978 & RH_Hilo$Year<=2017,]
RH_Lih.78<-RH_Lih[RH_Lih$Year>=1978 & RH_Lih$Year<=2017,]

##### Compare PHTO and PHLI annual series
par(mfrow=c(3,4))
for (i in 1:10){
  metr<-unlist(strsplit(names(cphto.78)[i], "_"))[2]
  plot(cphto.78[,i]~cphto.78$Year, 
       main=metr,ylab=metr, 
       ylim=range(cphto.78[,i], cphli.78[,i], na.rm=T),
       type="l", col="red")
  lines(cphli.78[,i]~cphli.78$Year, col="green")
}


# ##### Compare Tmax
# plot(Tdat.78$coefs_Tmax.b~Tdat.78$Year,
#      type="l", col="black", lwd=2)
# plot(Tdat.78$coefs_Tmax.m~Tdat.78$Year,
#      type="l", col="black", lwd=2)
# 
# plot(cphto.78$phto_OBday.z~cphto.78$Year, type="l", col="red", 
#      ylim=range(cphto.78$phto_OBday.z, cphto.78$phto_OBSFday.z, na.rm=T))
# lines(cphto.78$phto_OBSFday.z~cphto.78$Year, col="purple")
# 
# plot(cphto.78$phto_OBday.fr~cphto.78$Year, type="l", col="red",
#      ylim=range(cphto.78$phto_OBday.fr, cphto.78$phto_OBSFday.fr, na.rm=T))
# lines(cphto.78$phto_OBSFday.fr~cphto.78$Year, col="purple")
# 
# plot(sHilo.78$Mean.mixed.layer.mixing.ratio~sHilo.78$Year, 
#      type="l", col="red")
# plot(sHilo.78$Precipitable.water..mm..for.entire.sounding~sHilo.78$Year, 
#      type="l", col="red")
# plot(sHilo.78$Pres..hPa..of.the.Lifted.Condensation.Level~sHilo.78$Year, 
#      type="l", col="red")
# plot(sHilo.78$LFCT.using.virtual.temperature~sHilo.78$Year, 
#      type="l", col="red")
# 
# ## Compare daytime LCL to that from ceilometer for Hilo (left) and Lihue (right)
# par(mfcol=c(2,2))
# plot(cphto.78$phto_OBday.z~cphto.78$Year, col="red",type="l",
#      ylim=range(cphto.78$phto_OBday.z, cphto.78$phto_OBSFday.z, na.rm=T))
# lines(cphto.78$phto_OBSFday.z~cphto.78$Year, col="purple")
# plot(sHilo.78$Pres..hPa..of.the.Lifted.Condensation.Level~sHilo.78$Year, col="red", type="l")
# 
# summary(lm(cphto.78$phto_OBSFday.z~sHilo.78$Pres..hPa..of.the.Lifted.Condensation.Level))
# plot(cphto.78$phto_OBSFday.z~sHilo.78$Pres..hPa..of.the.Lifted.Condensation.Level)
# 
# plot(cphli.78$phli_OBday.z~cphli.78$Year, col="red",type="l",
#      ylim=range(cphli.78$phli_OBday.z, cphli.78$phli_OBSFday.z, na.rm=T))
# lines(cphli.78$phli_OBSFday.z~cphli.78$Year, col="purple")
# plot(sLih.78$Pres..hPa..of.the.Lifted.Condensation.Level~sLih.78$Year, col="red", type="l")
# 
# summary(lm(cphli.78$phli_OBSFday.z~sLih.78$Pres..hPa..of.the.Lifted.Condensation.Level))
# plot(cphli.78$phli_OBSFday.z~sLih.78$Pres..hPa..of.the.Lifted.Condensation.Level)
# 

## Figure of 
#   Surface: -dT/dz, temperature
#   Sounding: RH 0-100, 800-1600m 
#   Ceilometer: OBSF ceiling height, Clear day frequency
#   Sounding trend profile: Sound_UWyo_profile

pdf("LapseFig5_Sfc_Sound_Ceil.pdf", width=6, height=7)
mat<-matrix(c(1:5, 1, 6:9), byrow=F, ncol=2, nrow=5)
layout(mat)
par(mar=c(0.1, 0.1, 0.1, 0.1), oma=c(3,6,3,6), 
    tcl=-0.3, mgp=c(2,1,0))

################# DAY/NIGHT Surface #################################
# Surface, Tmax, daytime
plot(Tdat.58$coefs_Tmax.m ~Tdat.58$Year, ylim=c(5.5, 9.5),
     col="red",type="l", las=1)
# Shaded area for 1978-2017
coord.x<-c(1978, 1978, 2017, 2017)
coord.y<-c(5.5, 9.5, 9.5, 5.5)
polygon (coord.x, coord.y, col="lightgray", border=NA)

# Surface, Tmin, nighttime
lines((Tdat.58$coefs_Tmin.m) ~Tdat.58$Year, col="blue", lwd=2)
lines((Tdat.58$coefs_Tmax.m) ~Tdat.58$Year, col="red", lwd=2)

# Add in trends IF significant
sen.58max<-zyp.sen(coefs_Tmax.m~Year, data=Tdat.58)
confint(sen.58max) # signif
abline(coef(sen.58max), col="red", lty=2, lwd=2)

sen.78max<-zyp.sen(coefs_Tmax.m~Year, data=Tdat.78)
confint(sen.78max) # signif
usr <- par("usr")  # recent trend
clip(x1=1978, usr[2], usr[3], usr[4])
abline(coef(sen.78max), col="black", lty=3, lwd=2)

sen.58min<-zyp.sen(coefs_Tmin.m~Year, data=Tdat.58)
confint(sen.58min) # NS
# abline(coef(sen.58min), col="blue", lty=2)

sen.78min<-zyp.sen(coefs_Tmin.m~Year, data=Tdat.78)
confint(sen.78min) # NS

mtext(expression(Surface~-dT/dz~(degree*C~km^-1)), 
      side=2, line=3.5, cex=0.8)
#axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))


# par(new=T)
# plot(Tdat.78$coefs_Tmax.b ~ Tdat.78$Year, ylim=c(19, 30),
#      col="red", type="l", lty=2, las=1,
#      yaxt="n", xaxt="n")
# axis(4, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
# #mtext(expression('T'[max,z0]~(degree*C)), side=2, line=3, cex=0.8, 
# #      outer=T, adj=1)
# 
# # Surface, Tmin, nighttime
# plot((Tdat.78$coefs_Tmin.m)*(-1000) ~Tdat.78$Year, ylim=c(5.5, 9.5),
#      col="black",type="l", xaxt="n", yaxt="n", las=1)
# axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
# axis(side=2, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
# mtext(expression(T[min]*","~Midight), side=3, line=0.5, cex=0.9)
# par(new=T)
# plot(Tdat.78$coefs_Tmin.b ~ Tdat.78$Year, ylim=c(19, 30),
#      col="blue", type="l", lty=2, las=1,
#      yaxt="n", ylab="", xaxt="n", xlab="",)
# axis(4, las=1)
# mtext(expression('T'[z0]~(degree*C)), side=4, line=3.5, cex=0.8)


################# DAY #################################
# Sounding RH
plot(rh$z1200.1600.H00 ~rh$year, 
     col="dark gray", type="l", lwd=2, las=1, xaxt="n", ylim=c(60,95))
mtext("Layer RH (%)", side=2, line=3.5, cex=0.8)
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
lines(rh$z0.400.H00 ~rh$year, col="red", lwd=2)
lines(rh$z1200.1600.L00 ~rh$year,col="dark gray", lty=2, lwd=2)
lines(rh$z0.400.L00 ~rh$year, col="red", lty=2, lwd=2)

# Add in trends IF significant
sen.78rhhi<-zyp.sen(z1200.1600.H00~year, data=rh)
confint(sen.78rhhi) # signif
abline(coef(sen.78rhhi), col="black", lty=1, lwd=1.5)

sen.78rhhiL<-zyp.sen(z1200.1600.L00~year, data=rh)
confint(sen.78rhhiL) # signif
abline(coef(sen.78rhhiL), col="black", lty=2, lwd=1)

sen.78rhlo<-zyp.sen(z0.400.H00~year, data=rh)
confint(sen.78rhlo) # signif
abline(coef(sen.78rhlo), col="black", lty=1, lwd=1)

sen.78rhloL<-zyp.sen(z0.400.L00~year, data=rh)
confint(sen.78rhloL) # NS


# Sounding LCL #####################################
plot(sHilo.78$Pres..hPa..of.the.Lifted.Condensation.Level~sHilo.78$Year, 
     col="black", type="l", ylim=c(930, 890), xaxt="n", las=1)
mtext("LCL pressure (hPa)", side=2, line=3.5,cex=0.8)
lines(sLih.78$Pres..hPa..of.the.Lifted.Condensation.Level~sLih.78$Year, 
      col="black", lty=2)
# axis(side=4, labels=F, tick=T )
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))

# Add in trends IF significant
sen.78lclH<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level~Year, data=sHilo.78)
confint(sen.78lclH) # signif
abline(coef(sen.78lclH), col="black", lty=1, lwd=2)

sen.78lclL<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level~Year, data=sLih.78)
confint(sen.78lclL) # NS
# abline(coef(sen.78lclL), col="black", lty=2, lwd=2)


# Ceilometer Cloud Base Height #####################################
plot(cphto.78$phto_OBday.z~cphto.78$Year, 
     col="dark gray",type="l", las=1, xaxt="n", ylim=c(600,1600))
mtext("Ceiling height (m)", side=2, line=3.5, cex=0.8)
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))

lines(cphto.78$phto_OBSFday.z~cphto.78$Year, col="black")
lines(cphli.78$phli_OBday.z~cphli.78$Year, col="dark gray", lty=2)
lines(cphli.78$phli_OBSFday.z~cphli.78$Year, col="black", lty=2)

# Add in trends IF significant
sen.78OBH<-zyp.sen(phto_OBday.z~Year, data=cphto.78)
confint(sen.78OBH) # signif
abline(coef(sen.78OBH), col="black", lty=1, lwd=2)

sen.78OBL<-zyp.sen(phli_OBday.z~Year, data=cphli.78)
confint(sen.78OBL) # NS
# abline(coef(sen.78OBL), col="black", lty=2, lwd=2)

sen.78OBSFH<-zyp.sen(phto_OBSFday.z~Year, data=cphto.78)
confint(sen.78OBSFH) # signif
abline(coef(sen.78OBSFH), col="black", lty=1, lwd=2)

sen.78OBSFL<-zyp.sen(phli_OBSFday.z~Year, data=cphli.78)
confint(sen.78OBSFL) # signif
abline(coef(sen.78OBSFL), col="black", lty=2, lwd=2)

# Ceilometer Frequency #####################################
plot(cphto.78$phto_CLRday.fr~cphto.78$Year, col="blue",type="l",
     ylab="", las=1, xlab="Year", ylim=c(0,0.85))
lines(cphli.78$phli_CLRday.fr~cphli.78$Year, col="blue", lty=2)

lines(cphto.78$phto_OBday.fr~cphto.78$Year, col="dark gray")
lines(cphli.78$phli_OBday.fr~cphli.78$Year, col="dark gray", lty=2)
mtext("Frequency", side=2, line=3.5,cex=0.8)

# Add in trends IF significant
sen.78CLRH<-zyp.sen(phto_CLRday.fr~Year, data=cphto.78)
confint(sen.78CLRH) # NS
# abline(coef(sen.78CLRH), col="black", lty=1, lwd=2)

sen.78CLRL<-zyp.sen(phli_CLRday.fr~Year, data=cphli.78)
confint(sen.78CLRL) # signif
abline(coef(sen.78CLRL), col="black", lty=2, lwd=2)

sen.78OBfrH<-zyp.sen(phto_OBday.fr~Year, data=cphto.78)
confint(sen.78OBfrH) # signif
abline(coef(sen.78OBfrH), col="black", lty=1, lwd=2)

sen.78OBfrL<-zyp.sen(phli_OBday.fr~Year, data=cphli.78)
confint(sen.78OBfrL) # NS
# abline(coef(sen.78OBfrL), col="black", lty=2, lwd=2)

################# NIGHT #################################
# Sounding RH
plot(rh$z1200.1600.H12 ~rh$year, 
     col="dark gray",type="l",lwd=2,
     yaxt="n", xaxt="n",ylim=c(70,95))
axis(side=4, las=1)
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
lines(rh$z0.400.H12 ~rh$year, col="red",lwd=2)

lines(rh$z1200.1600.L12 ~rh$year,col="dark gray", lty=2,lwd=2)
lines(rh$z0.400.L12 ~rh$year, col="red", lty=2,lwd=2)
mtext("Layer RH (%)", side=4, line=3.5,cex=0.8)

# Add in trends IF significant
sen.78rhhi<-zyp.sen(z1200.1600.H12~year, data=rh)
confint(sen.78rhhi) # NS
# abline(coef(sen.78rhhi), col="black", lty=1, lwd=2)

sen.78rhhiL<-zyp.sen(z1200.1600.L12~year, data=rh)
confint(sen.78rhhiL) # signif
abline(coef(sen.78rhhiL), col="black", lty=2, lwd=1)

sen.78rhlo<-zyp.sen(z0.400.H12~year, data=rh)
confint(sen.78rhlo) # signif
abline(coef(sen.78rhlo), col="black", lty=1, lwd=1)

sen.78rhloL<-zyp.sen(z0.400.L12~year, data=rh)
confint(sen.78rhloL) # NS

# Sounding LCL #####################################
plot(sHilo.78$Pres..hPa..of.the.Lifted.Condensation.Level.1~sHilo.78$Year, 
     col="black", type="l", ylim=c(960,920), yaxt="n", xaxt="n")
lines(sLih.78$Pres..hPa..of.the.Lifted.Condensation.Level.1~sLih.78$Year, 
      col="black", lty=2)
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
axis(4, las=1)
mtext("LCL pressure (hPa)", side=4, line=3.5,cex=0.8)

# Add in trends IF significant
sen.78lclH<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level.1~Year, data=sHilo.78)
confint(sen.78lclH) # NS
# abline(coef(sen.78lclH), col="black", lty=2, lwd=2)

sen.78lclL<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level.1~Year, data=sLih.78)
confint(sen.78lclL) # NS
# abline(coef(sen.78lclL), col="black", lty=2, lwd=2)

# Ceilometer Cloud Base Height #####################################
plot(cphto.78$phto_OBnt.z~cphto.78$Year, 
     col="dark gray",type="l", las=1, xaxt="n", 
     yaxt="n", ylim=c(600,1600))
lines(cphto.78$phto_OBSFnt.z~cphto.78$Year, col="black")
lines(cphli.78$phli_OBnt.z~cphli.78$Year, col="dark gray", lty=2)
lines(cphli.78$phli_OBSFnt.z~cphli.78$Year, col="black", lty=2)
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
axis(4, las=1)
mtext("Ceiling height (m)", side=4, line=3.5,cex=0.8)

# Add in trends IF significant
sen.78OBH<-zyp.sen(phto_OBnt.z~Year, data=cphto.78)
confint(sen.78OBH) # signif
abline(coef(sen.78OBH), col="black", lty=1, lwd=2)

sen.78OBL<-zyp.sen(phli_OBnt.z~Year, data=cphli.78)
confint(sen.78OBL) # NS
# abline(coef(sen.78OBL), col="black", lty=2, lwd=2)

sen.78OBSFH<-zyp.sen(phto_OBSFnt.z~Year, data=cphto.78)
confint(sen.78OBSFH) # signif
abline(coef(sen.78OBSFH), col="black", lty=1, lwd=2)

sen.78OBSFL<-zyp.sen(phli_OBSFnt.z~Year, data=cphli.78)
confint(sen.78OBSFL) # signif
abline(coef(sen.78OBSFL), col="black", lty=2, lwd=2)

# Ceilometer Cloud frequency #####################################
plot(cphto.78$phto_CLRnt.fr~cphto.78$Year, col="blue",type="l",
     ylab="", yaxt="n", ylim=c(0,0.85))
lines(cphli.78$phli_CLRnt.fr~cphli.78$Year, col="blue", lty=2)

lines(cphto.78$phto_OBnt.fr~cphto.78$Year, col="dark gray")
lines(cphli.78$phli_OBnt.fr~cphli.78$Year, col="dark gray", lty=2)

axis(4, las=1)
mtext("Frequency", side=4, line=3.5,cex=0.8)

# Add in trends IF significant
sen.78CLRH<-zyp.sen(phto_CLRnt.fr~Year, data=cphto.78)
confint(sen.78CLRH) # NS
# abline(coef(sen.78CLRH), col="black", lty=2, lwd=2)

sen.78CLRL<-zyp.sen(phli_CLRnt.fr~Year, data=cphli.78)
confint(sen.78CLRL) # signif
abline(coef(sen.78CLRL), col="black", lty=2, lwd=2)

sen.78OBfrH<-zyp.sen(phto_OBnt.fr~Year, data=cphto.78)
confint(sen.78OBfrH) # signif
abline(coef(sen.78OBfrH), col="black", lty=1, lwd=2)

sen.78OBfrL<-zyp.sen(phli_OBnt.fr~Year, data=cphli.78)
confint(sen.78OBfrL) # NS
abline(coef(sen.78OBfrL), col="black", lty=2, lwd=2)

dev.off()


#############################################################################
################# Figure 6: profiles

pdf("LapseFig6_T_RH_WSprofile.pdf", width=6, height=7)
mat<-matrix(c(1:6), byrow=F, ncol=3, nrow=2)
layout(mat)
par(mar=c(0.1, 0.1, 0.1, 0.1), oma=c(8,6,3,6), 
    tcl=-0.3, mgp=c(2,1,0))

### Profile trends

slc<-matrix(as.numeric(unlist(strsplit(substring(relh.tr$X, 2), "[.]"))),byrow=T,ncol=2)
mid<-apply(slc, 1, mean)

H00t<-temp.tr[,grep("H00", names(temp.tr))]
H12t<-temp.tr[,grep("H12", names(temp.tr))]
L00t<-temp.tr[,grep("L00", names(temp.tr))]
L12t<-temp.tr[,grep("L12", names(temp.tr))]

H00r<-relh.tr[,grep("H00", names(relh.tr))]
H12r<-relh.tr[,grep("H12", names(relh.tr))]
L00r<-relh.tr[,grep("L00", names(relh.tr))]
L12r<-relh.tr[,grep("L12", names(relh.tr))]

H00m<-mixr.tr[,grep("H00", names(mixr.tr))]
H12m<-mixr.tr[,grep("H12", names(mixr.tr))]
L00m<-mixr.tr[,grep("L00", names(mixr.tr))]
L12m<-mixr.tr[,grep("L12", names(mixr.tr))]

H00s<-sknt.tr[,grep("H00", names(sknt.tr))]
H12s<-sknt.tr[,grep("H12", names(sknt.tr))]
L00s<-sknt.tr[,grep("L00", names(sknt.tr))]
L12s<-sknt.tr[,grep("L12", names(sknt.tr))]

### Daytime RH
# Hilo
plot(mid~H00r[,2], xlim=c(-3, 5), col="red",pch=1, xlab="", xaxt="n", las=1)
arrows(H00r[,1], mid, H00r[,3], mid, col="red", length=0, lwd=2)
abline(v=0)
abline(h=1600)
# abline(h=2150)

points(L00r[,2], mid+20, col="purple", pch=8)
arrows(L00r[,1], mid+20, L00r[,3], mid+20, col="purple", length=0, lwd=2)
mtext("Altitude (m)", side=2, line=3.5, cex=0.8)
# mtext("Midday", side=3, line=0.5, cex=0.9)
axis(1, labels=F, tcl=0.3)

par(new=T)
plot(H00r$H00r.mean, mid, type="l", col="red", lwd=2, xlim=c(0,100), yaxt="n", xaxt="n")
lines(L00r$L00r.mean, mid, type="l", col="purple", lwd=2, lty=2)
# axis(1, line=4)
mtext("Relative Humidity", side=3, line=1)

### Nighttime RH
plot(mid~H12r[,2], xlim=c(-3, 5), col="blue",pch=1, xlab="", las=1)
arrows(H12r[,1], mid, H12r[,3], mid, col="blue", length=0, lwd=2)
abline(v=0)
abline(h=1600)
# abline(h=2150)

points(L12r[,2], mid+20, col="purple", pch=8)
arrows(L12r[,1], mid+20, L12r[,3], mid+20, col="purple", length=0, lwd=2)
# axis(side=2, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
mtext("Altitude (m)", side=2, line=3.5, cex=0.8)

# mtext("Midnight", side=3, line=0.5, cex=0.9)
# mtext(expression('RH trend'~('%'*dec^-1)), side=1, line=2.5, cex=0.9)

par(new=T)
plot(H12r$H12r.mean, mid, type="l", col="blue", lwd=2,  xlim=c(0,100),
     yaxt="n", xaxt="n")
lines(L12r$L12r.mean, mid, type="l", col="purple", lwd=2,  lty=2)
axis(1, line=4)
#mtext("RH", side=1, line=6, cex=0.9)

mtext(expression('RH trend'~('%'~dec^-1)), side=1, line=2.5, cex=0.8)
mtext(expression('RH'~('%')), side=1, line=6.5, cex=0.8)

#######################
### Daytime Temp
range(H00t[,1:3], H12t[,1:3], L00t[,1:3], L12t[,1:3])

plot(mid~H00t[,2], xlim=c(-0.25, 0.45), col="red",pch=1, xlab="", xaxt="n", yaxt="n")
arrows(H00t[,1], mid, H00t[,3], mid, col="red", length=0, lwd=2)
abline(v=0)
abline(h=1600)
# abline(h=2150)

points(L00t[,2], mid+20, col="purple", pch=8)
arrows(L00t[,1], mid+20, L00t[,3], mid+20, col="purple", length=0, lwd=2)
axis(1, labels=F, tcl=0.3)
# mtext("Midday", side=3, line=0.5, cex=0.9)

par(new=T)
plot(H00t$H00t.mean, mid, type="l", col="red", lwd=2,  xlim=c(0,25),
     yaxt="n", ylab="", xlab="", xaxt="n")
lines(L00t$L00t.mean, mid, type="l", col="purple", lwd=2,  lty=2)
# axis(1, line=4)
mtext("Temperature", side=3, line=1)

# Nighttime temp
plot(mid~H12t[,2], xlim=c(-0.25, 0.45), col="blue",pch=1, xlab="", yaxt="n")
arrows(H12t[,1], mid, H12t[,3], mid, col="blue", length=0, lwd=2)
abline(v=0)
abline(h=1600)
# abline(h=2150)

points(L12t[,2], mid+20, col="purple", pch=8)
arrows(L12t[,1], mid+20, L12t[,3], mid+20, col="purple", length=0, lwd=2)

# mtext("Altitude (m)", side=2, line=3.5, cex=0.8)
# mtext("Midnight", side=3, line=0.5, cex=0.9)
mtext(expression('Temp trend'~(degree*C~dec^-1)), side=1, line=2.5, cex=0.8)
mtext(expression("Temp"~(degree*C)), side=1, line=6.5, cex=0.8)

par(new=T)
plot(H12t$H12t.mean, mid, type="l", col="blue", lwd=2,  xlim=c(0,25), yaxt="n", xaxt="n")
lines(L12t$L12t.mean, mid, type="l", col="purple", lwd=2,  lty=2)
axis(1, line=4)


#######################
### Daytime Wind speed
range(H00s[,1:3], H12s[,1:3], L00s[,1:3], L12s[,1:3])
range(H00s[,4], L00s[,4], H12s[,4], L12s[,4])

plot(mid~H00s[,2], xlim=c(-0.31, 0.2), col="red",pch=1, xlab="", yaxt="n", xaxt="n")
arrows(H00s[,1], mid, H00s[,3], mid, col="red", length=0, lwd=2)
abline(v=0)
abline(h=1600)
# abline(h=2150)

points(L00s[,2], mid+20, col="purple", pch=8)
arrows(L00s[,1], mid+20, L00s[,3], mid+20, col="purple", length=0, lwd=2)
axis(1, labels=F, tcl=0.3)
axis(4, las=1)
# mtext("Midday", side=3, line=0.5, cex=0.9)

par(new=T)
plot(H00s$H00s.mean, mid, type="l", col="red", lwd=2,  xlim=c(2.5,8.0),
     yaxt="n", ylab="", xlab="", xaxt="n")
lines(L00s$L00s.mean, mid, type="l", col="purple", lwd=2,  lty=2)
mtext("Wind Speed", side=3, line=1)

# Nighttime temp
plot(mid~H12s[,2], xlim=c(-0.31, 0.2), col="blue",pch=1, xlab="", yaxt="n")
arrows(H12s[,1], mid, H12s[,3], mid, col="blue", length=0, lwd=2)
abline(v=0)
abline(h=1600)
# abline(h=2150)

points(L12s[,2], mid+20, col="purple", pch=8)
arrows(L12s[,1], mid+20, L12s[,3], mid+20, col="purple", length=0, lwd=2)
axis(4, las=1)

# mtext("Altitude (m)", side=2, line=3.5, cex=0.8)
# mtext("Midnight", side=3, line=0.5, cex=0.9)
mtext(expression('WS trend'~(m~s^-1~dec^-1)), side=1, line=2.5, cex=0.8)
mtext(expression("WS"~(m~s^-1)), side=1, line=6.5, cex=0.8)

par(new=T)
plot(H12s$H12s.mean, mid, type="l", col="blue", lwd=2,  xlim=c(2.5,8.0), yaxt="n", xaxt="n")
lines(L12s$L12s.mean, mid, type="l", col="purple", lwd=2,  lty=2)
axis(1, line=4)


dev.off()


# ## Relationship of LCL and Ceilometer
# par(mfrow=c(2,2))
# summary(lm(cphto.80$phto_OBSFnt.z~sHilo.80$Pres..hPa..of.the.Lifted.Condensation.Level))
# plot(cphto.80$phto_OBSFnt.z~sHilo.80$Pres..hPa..of.the.Lifted.Condensation.Level)
# lm.phto_ceil.lcl<-lm(cphto.80$phto_OBSFnt.z~sHilo.80$Pres..hPa..of.the.Lifted.Condensation.Level)
# abline(lm.phto_ceil.lcl)
# 
# summary(lm(cphli.80$phli_OBSFnt.z~sLih.80$Pres..hPa..of.the.Lifted.Condensation.Level))
# plot(cphli.80$phli_OBSFnt.z~sLih.80$Pres..hPa..of.the.Lifted.Condensation.Level)
# lm.phli_ceil.lcl<-lm(cphli.80$phli_OBSFnt.z~sLih.80$Pres..hPa..of.the.Lifted.Condensation.Level)
# abline(lm.phli_ceil.lcl)
# 
# ## Relationship of LCL and RH
# rh.lcl<-merge(RH_Hilo.80, sHilo.80, by="Year", all=T)
# 
# summary(lm(Pres..hPa..of.the.Lifted.Condensation.Level.1~RH_800.1600m, data=rh.lcl))
# plot(Pres..hPa..of.the.Lifted.Condensation.Level.1~RH_800.1600m, data=rh.lcl)
# lm.phto_rh.lcl<-lm(Pres..hPa..of.the.Lifted.Condensation.Level.1~RH_800.1600m, data=rh.lcl)
# abline(lm.phto_rh.lcl)
# 
# rh.lcl.lih<-merge(RH_Lih.80, sLih.80, by="Year", all=T)
# summary(lm(Pres..hPa..of.the.Lifted.Condensation.Level.1~RH_800.1600m, data=rh.lcl.lih))
# plot(Pres..hPa..of.the.Lifted.Condensation.Level.1~RH_800.1600m, data=rh.lcl.lih)
# lm.phli_rh.lcl<-lm(Pres..hPa..of.the.Lifted.Condensation.Level.1~RH_800.1600m, data=rh.lcl.lih)
# abline(lm.phli_rh.lcl)
# 
# #dev.off()


xx<-c(cphto.78$Year, rev(cphto.78$Year))
clr.y<-c(cphto.78$phto_CLRday.fr, rep(0, length(cphto.78$phto_CLRday.fr)))
bkn.ovc<-c(rep(1, length(cphto.78$phto_CLRday.fr)), 1-rev(cphto.78$phto_OBday.fr )) 
fs<-1-cphto.78$phto_CLRday.fr-cphto.78$phto_OBday.fr
few.sct<-c(cphto.78$phto_CLRday.fr, rev(cphto.78$phto_CLRday.fr+fs))

plot(x=cphto.78$Year, y=cphto.78$phto_CLRday.fr, col='blue', type='l', ylim=c(0,1))
polygon(xx, clr.y, col="blue")
polygon(xx, few.sct, col="skyblue")
polygon(xx, bkn.ovc, col="lightgray")

clr.y<-c(cphli.78$phli_CLRday.fr, rep(0, length(cphli.78$phli_CLRday.fr)))
bkn.ovc<-c(rep(1, length(cphli.78$phli_CLRday.fr)), 1-rev(cphli.78$phli_OBday.fr )) 
fs<-1-cphli.78$phli_CLRday.fr-cphli.78$phli_OBday.fr
few.sct<-c(cphli.78$phli_CLRday.fr, rev(cphli.78$phli_CLRday.fr+fs))

plot(x=cphli.78$Year, y=cphli.78$phli_CLRday.fr, col='blue', type='l', ylim=c(0,1))
polygon(xx, clr.y, col="blue")
polygon(xx, few.sct, col="skyblue")
polygon(xx, bkn.ovc, col="lightgray")

#####
clr.y<-c(cphto.78$phto_CLRnt.fr, rep(0, length(cphto.78$phto_CLRnt.fr)))
bkn.ovc<-c(rep(1, length(cphto.78$phto_CLRnt.fr)), 1-rev(cphto.78$phto_OBnt.fr )) 
fs<-1-cphto.78$phto_CLRnt.fr-cphto.78$phto_OBnt.fr
few.sct<-c(cphto.78$phto_CLRnt.fr, rev(cphto.78$phto_CLRnt.fr+fs))

plot(x=cphto.78$Year, y=cphto.78$phto_CLRnt.fr, col='blue', type='l', ylim=c(0,1))
polygon(xx, clr.y, col="blue")
polygon(xx, few.sct, col="skyblue")
polygon(xx, bkn.ovc, col="lightgray")

clr.y<-c(cphli.78$phli_CLRnt.fr, rep(0, length(cphli.78$phli_CLRnt.fr)))
bkn.ovc<-c(rep(1, length(cphli.78$phli_CLRnt.fr)), 1-rev(cphli.78$phli_OBnt.fr )) 
fs<-1-cphli.78$phli_CLRnt.fr-cphli.78$phli_OBnt.fr
few.sct<-c(cphli.78$phli_CLRnt.fr, rev(cphli.78$phli_CLRnt.fr+fs))

plot(x=cphli.78$Year, y=cphli.78$phli_CLRnt.fr, col='blue', type='l', ylim=c(0,1))
polygon(xx, clr.y, col="blue")
polygon(xx, few.sct, col="skyblue")
polygon(xx, bkn.ovc, col="lightgray")

