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

# Sounding surface information
H.00.sfc<-read.csv("NOT_USED/AKV_Sonde/UWyo/H.00.sfc.yr78.csv", stringsAsFactors = F)
H.12.sfc<-read.csv("NOT_USED/AKV_Sonde/UWyo/H.12.sfc.yr78.csv", stringsAsFactors = F)
L.00.sfc<-read.csv("NOT_USED/AKV_Sonde/UWyo/L.00.sfc.yr78.csv", stringsAsFactors = F)
L.12.sfc<-read.csv("NOT_USED/AKV_Sonde/UWyo/L.12.sfc.yr78.csv", stringsAsFactors = F)

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

####### Focus on period of homogeneous ceilometer data 1998-2017
Tdat.98<-Tdat[Tdat$Year>=1998,]

sHilo.98<-sHilo[sHilo$Year>=1998 & sHilo$Year<=2017,]
sLih.98<-sLih[sLih$Year>=1998 & sHilo$Year<=2017,]

cphto.98<-cphto[cphto$Year>=1998 & cphto$Year<=2017,]
cphli.98<-cphli[cphli$Year>=1998& cphli$Year<=2017,]

RH_Hilo.98<-RH_Hilo[RH_Hilo$Year>=1998 & RH_Hilo$Year<=2017,]
RH_Lih.98<-RH_Lih[RH_Lih$Year>=1998 & RH_Lih$Year<=2017,]


##### Compare PHTO and PHLI annual series
par(mfrow=c(3,4))
for (i in 1:10){
  metr<-unlist(strsplit(names(cphto.98)[i], "_"))[2]
  plot(cphto.98[,i]~cphto.98$Year, 
       main=metr,ylab=metr, 
       ylim=range(cphto.98[,i], cphli.98[,i], na.rm=T),
       type="l", col="red")
  lines(cphli.98[,i]~cphli.98$Year, col="green")
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

## Figure for paper
#   Surface: -dT/dz, temperature
#   Sounding: RH 11m (surface) 
#   Ceilometer: OBSF ceiling height, Clear day frequency

## Figure in supplement (overcast)
#   Surface: -dT/dz, temperature
#   Sounding: Mixing ratio 11m (surface) 
#   Ceilometer: OB ceiling height, OB frequency

pdf("LapseFig5_Sfc_Sound_Ceil_1998.pdf", width=6, height=8.5)
mat<-matrix(c(1,1,0,2,3,3,4,4,5,5,6,6,1,1,0,2,7,7,8,8,9,9,10,10), 
            byrow=F, ncol=2, nrow=12)
layout(mat)
par(mar=c(0.1, 0.1, 0.1, 0.1), oma=c(3,6,3,6), 
    tcl=-0.3, mgp=c(2,1,0))

################# DAY/NIGHT Surface #################################
# Surface, Tmax, daytime
plot(Tdat.58$coefs_Tmax.m ~Tdat.58$Year, ylim=c(5.5, 10), xlim=c(1958, 2017),
     col="red",type="l", las=1)
# Shaded area for 1978-2017
coord.x<-c(1978, 1978, 2017, 2017)
coord.y<-c(5.5, 10, 10, 5.5)
polygon (coord.x, coord.y, col="lightgray", border=NA)

# Surface, Tmin, nighttime
lines((Tdat.58$coefs_Tmin.m) ~Tdat.58$Year, col="blue", lwd=1)
lines((Tdat.58$coefs_Tmax.m) ~Tdat.58$Year, col="red", lwd=3)
abline(9.8,0)  # dry adiabatic lapse rate
text(2014, 9.5, "DALR")
abline(6.49,0) # environmental lapse rate
text(2014, 6.2, "ELR")

# Add in trends IF significant
sen.58max<-zyp.sen(coefs_Tmax.m~Year, data=Tdat.58)
confint(sen.58max) # signif
usr <- par("usr")  # recent trend
clip(x1=1958, usr[2], usr[3], usr[4])
abline(coef(sen.58max), col="red", lty=2, lwd=2)

sen.78max<-zyp.sen(coefs_Tmax.m~Year, data=Tdat.78)
confint(sen.78max) # signif
usr <- par("usr")  # recent trend
clip(x1=1978, usr[2], usr[3], usr[4])
abline(coef(sen.78max), col="black", lty=3, lwd=2)

sen.98max<-zyp.sen(coefs_Tmax.m~Year, data=Tdat.98)
confint(sen.98max) # NOT signif

sen.58min<-zyp.sen(coefs_Tmin.m~Year, data=Tdat.58)
confint(sen.58min) # NS
# abline(coef(sen.58min), col="blue", lty=2)

sen.78min<-zyp.sen(coefs_Tmin.m~Year, data=Tdat.78)
confint(sen.78min) # NS

sen.98min<-zyp.sen(coefs_Tmin.m~Year, data=Tdat.98)
confint(sen.98min) # NS

mtext(expression(Surface~-dT/dz~(degree*C~km^-1)), 
      side=2, line=3.5, cex=0.7)
#axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
legend(1958, 10, legend=c(expression(-dT[min]/dz),expression(-dT[max]/dz)),
       lwd=c(1,3), lty=1, col=c("blue", "red"), bty='n')
legend(1975, 10, legend=c(as.expression(bquote(
  1958-2017~trend:~+.(round(coef(sen.58max)[2]*10,2))~dec^-1)),
  as.expression(bquote(
    1978-2017~trend:~+.(round(coef(sen.78max)[2]*10,2))~dec^-1))),
  lwd=2, lty=c(2,3), col=c("red", "black"), bty='n')


# Legend and titles for sounding/ceilometer data
plot.new()
legend("bottom", legend=c("Hilo", "Lihue"), lty=c(1,2), horiz=T)
mtext("2 PM", side=3, adj=0.2, line=-3)
mtext("2 AM", side=3, adj=0.8, line=-3)

################# DAY #################################
# DAY Sounding RH (surface)
plot(H.00.sfc$relh ~H.00.sfc$X, type="l", lwd=1, 
     las=1, xaxt="n", ylab='', ylim=c(60,75))
lines(L.00.sfc$relh ~L.00.sfc$X, lty=2, lwd=1)
mtext("Surface RH (%)", side=2, line=3.5, cex=0.7)
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))

# Add in trends IF significant
sen.78rhhi<-zyp.sen(relh~X, data=H.00.sfc)  # -0.1438 (-0.2291667, -0.0700) per yr
confint(sen.78rhhi) # signif
usr <- par("usr")  # recent trend
clip(x1=1978, usr[2], usr[3], usr[4])
abline(coef(sen.78rhhi), col="black", lty=1, lwd=2)

sen.78rhhiL<-zyp.sen(relh~X, data=L.00.sfc) # -0.03589 (-0.1095679, 0.0297619) per yr
confint(sen.78rhhiL) # NS
#abline(coef(sen.78rhhiL), col="black", lty=2, lwd=2)

sen.98rhhi<-zyp.sen(relh~X, data=subset(H.00.sfc, X>=1998))  # -0.3958 (-0.6458333, -0.1083333) per yr
confint(sen.98rhhi) # signif
usr <- par("usr")  # recent trend
clip(x1=1998, usr[2], usr[3], usr[4])
abline(coef(sen.98rhhi), col="black", lty=1, lwd=2)

sen.98rhhiL<-zyp.sen(relh~X, data=subset(L.00.sfc, X>=1998))  # 0.1077 (-0.109375, 0.2500) per yr
confint(sen.98rhhiL) # NOT signif
#abline(coef(sen.98rhhiL), col="black", lty=1, lwd=2)

# DAY Sounding LCL #####################################
plot(sHilo.78$Pres..hPa..of.the.Lifted.Condensation.Level~sHilo.78$Year, 
     col="black", type="l", ylim=c(930, 890), xaxt="n", las=1)
mtext("LCL pressure (hPa)", side=2, line=3.5,cex=0.7)
lines(sLih.78$Pres..hPa..of.the.Lifted.Condensation.Level~sLih.78$Year, 
      col="black", lty=2)
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))

# Add in trends IF significant
sen.78lclH<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level~Year, data=sHilo.78)
confint(sen.78lclH) # signif # -0.2841 (-0.49525, -0.1080147) per yr
usr <- par("usr")  # recent trend
clip(x1=1978, usr[2], usr[3], usr[4])
abline(coef(sen.78lclH), col="black", lty=1, lwd=2)

sen.78lclL<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level~Year, data=sLih.78)
confint(sen.78lclL) # NS
# abline(coef(sen.78lclL), col="black", lty=2, lwd=2)

sen.98lclH<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level~Year, data=sHilo.98)
confint(sen.98lclH) # signif # -0.9841 (-1.469167, -0.5920076) per yr
usr <- par("usr")  # recent trend
clip(x1=1998, usr[2], usr[3], usr[4])
abline(coef(sen.98lclH), col="black", lty=1, lwd=2)

sen.98lclL<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level~Year, data=sLih.98)
confint(sen.98lclL) # NS
# abline(coef(sen.98lclL), col="black", lty=2, lwd=2)

# DAY Ceilometer Cloud Base Height #####################################
plot(cphto.98$phto_OBSFday.z~cphto.98$Year, col="black",
     type="l", las=1, xaxt="n", ylim=c(600, 1050), xlim=range(cphto.78$Year))
mtext("Cloud base height (m)", side=2, line=3.5, cex=0.7)
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
lines(cphli.98$phli_OBSFday.z~cphli.98$Year, col="black", lty=2)

# Add in trends IF significant
sen.98OBSFH<-zyp.sen(phto_OBSFday.z~Year, data=cphto.98)
confint(sen.98OBSFH) # signif
usr <- par("usr")  # recent trend
clip(x1=1998, usr[2], usr[3], usr[4])
abline(coef(sen.98OBSFH), col="black", lty=1, lwd=2)

sen.98OBSFL<-zyp.sen(phli_OBSFday.z~Year, data=cphli.98)
confint(sen.98OBSFL) # NOT signif
#abline(coef(sen.98OBSFL), col="black", lty=2, lwd=2)

# DAY Ceilometer Frequency #####################################
plot(cphto.98$phto_CLRday.fr~cphto.98$Year, type="l",
     ylab="", las=1, xlab="Year", ylim=c(0,0.3), xlim=range(cphto.78$Year))
lines(cphli.98$phli_CLRday.fr~cphli.98$Year, lty=2)
mtext("Clear sky frequency", side=2, line=3.5,cex=0.7)

# Add in trends IF significant
sen.98CLRH<-zyp.sen(phto_CLRday.fr~Year, data=cphto.98)
confint(sen.98CLRH) # NS
# abline(coef(sen.98CLRH), col="black", lty=1, lwd=2)

sen.98CLRL<-zyp.sen(phli_CLRday.fr~Year, data=cphli.98)
confint(sen.98CLRL) # signif
#abline(coef(sen.98CLRL), col="black", lty=2, lwd=2)

################# NIGHT #################################
# NIGHT Sounding RH (surface)
plot(H.12.sfc$relh ~H.12.sfc$X, type="l", lwd=1, 
     las=1, xaxt="n", ylab='', ylim=c(75,95), yaxt='n')
axis(side=4, las=1)
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
lines(L.12.sfc$relh ~L.12.sfc$X, lty=2)
mtext("Surface RH (%)", side=4, line=3.5,cex=0.7)

# Add in trends IF significant
sen.78rhhi<-zyp.sen(relh~X, data=H.12.sfc) # -0.08333 (-0.1481481, 0.0000)
confint(sen.78rhhi) # marginally NS
usr <- par("usr")  # recent trend
clip(x1=1978, usr[2], usr[3], usr[4])
abline(coef(sen.78rhhi), col="black", lty=1, lwd=2)

sen.78rhhiL<-zyp.sen(relh~X, data=L.12.sfc) # -0.02083 (-0.06190476, 0.02083333)
confint(sen.78rhhiL) # NS
# abline(coef(sen.78rhhiL), col="black", lty=2, lwd=1)

sen.98rhhi<-zyp.sen(relh~X, data=subset(H.12.sfc, X>=1998)) # -0.2545 (-0.4513889, -0.04861111)
confint(sen.98rhhi) # signif
usr <- par("usr")  # recent trend
clip(x1=1998, usr[2], usr[3], usr[4])
abline(coef(sen.98rhhi), col="black", lty=1, lwd=2)

sen.98rhhiL<-zyp.sen(relh~X, data=subset(L.12.sfc, X>=1998)) # -0.06073 (-0.1666667, 0.04583333)
confint(sen.98rhhiL) # NS
# abline(coef(sen.98rhhiL), col="black", lty=2, lwd=1)

# NIGHT Sounding LCL #####################################
plot(sHilo.78$Pres..hPa..of.the.Lifted.Condensation.Level.1~sHilo.78$Year, 
     col="black", type="l", ylim=c(960,920), yaxt="n", xaxt="n")
lines(sLih.78$Pres..hPa..of.the.Lifted.Condensation.Level.1~sLih.78$Year, 
      col="black", lty=2)
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
axis(4, las=1)
mtext("LCL pressure (hPa)", side=4, line=3.5,cex=0.7)

# Add in trends IF significant
sen.78lclH<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level.1~Year, data=sHilo.78)
confint(sen.78lclH) # NS
# abline(coef(sen.78lclH), col="black", lty=1, lwd=2)

sen.78lclL<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level.1~Year, data=sLih.78)
confint(sen.78lclL) # NS
# abline(coef(sen.78lclL), col="black", lty=2, lwd=2)

sen.98lclH<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level.1~Year, data=sHilo.98)
confint(sen.98lclH) # Signif -0.7408 (-1.28191, -0.07041667)
usr <- par("usr")  # recent trend
clip(x1=1998, usr[2], usr[3], usr[4])
abline(coef(sen.98lclH), col="black", lty=1, lwd=2)

sen.98lclL<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level.1~Year, data=sLih.98)
confint(sen.98lclL) # NS
# abline(coef(sen.98lclL), col="black", lty=2, lwd=2)

# NIGHT Ceilometer Cloud Base Height #####################################
plot(cphto.98$phto_OBSFnt.z~cphto.98$Year, 
     type="l", las=1, xaxt="n", 
     yaxt="n", ylim=c(600,1050), xlim=range(cphto.78$Year))
lines(cphli.98$phli_OBSFnt.z~cphli.98$Year, col="black", lty=2)
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
axis(4, las=1)
mtext("Cloud base height (m)", side=4, line=3.5,cex=0.7)

# Add in trends IF significant
sen.98OBSFH<-zyp.sen(phto_OBSFnt.z~Year, data=cphto.98)
confint(sen.98OBSFH) # signif
usr <- par("usr")  # recent trend
clip(x1=1998, usr[2], usr[3], usr[4])
abline(coef(sen.98OBSFH), col="black", lty=1, lwd=2)

sen.98OBSFL<-zyp.sen(phli_OBSFnt.z~Year, data=cphli.98)
confint(sen.98OBSFL) # NOT signif
#abline(coef(sen.98OBSFL), col="black", lty=2, lwd=2)

# NIGHT Ceilometer Cloud frequency #####################################
plot(cphto.98$phto_CLRnt.fr~cphto.98$Year, type="l",
     ylab="", yaxt="n", ylim=c(0,0.3), xlim=range(cphto.78$Year))
lines(cphli.98$phli_CLRnt.fr~cphli.98$Year, lty=2)
axis(4, las=1)
mtext("Clear sky frequency", side=4, line=3.5,cex=0.7)

# Add in trends IF significant
sen.98CLRH<-zyp.sen(phto_CLRnt.fr~Year, data=cphto.98)
confint(sen.98CLRH) # NS
# abline(coef(sen.98CLRH), col="black", lty=2, lwd=2)

sen.98CLRL<-zyp.sen(phli_CLRnt.fr~Year, data=cphli.98)
confint(sen.98CLRL) # signif
#abline(coef(sen.98CLRL), col="black", lty=2, lwd=2)

#######################################################################
## For SUPPLEMENT  ####################################################
mat<-matrix(c(1,1,0,2,3,3,4,4,5,5,6,6,1,1,0,2,7,7,8,8,9,9,10,10), 
            byrow=F, ncol=2, nrow=12)
layout(mat)
par(mar=c(0.1, 0.1, 0.1, 0.1), oma=c(3,6,3,6), 
    tcl=-0.3, mgp=c(2,1,0))

################# DAY/NIGHT Surface #################################
# Surface, Tmax, daytime
plot(Tdat.58$coefs_Tmax.m ~Tdat.58$Year, ylim=c(5.5, 10), xlim=c(1958, 2017),
     col="red",type="l", las=1)
# Shaded area for 1978-2017
coord.x<-c(1978, 1978, 2017, 2017)
coord.y<-c(5.5, 10, 10, 5.5)
polygon (coord.x, coord.y, col="lightgray", border=NA)

# Surface, Tmin, nighttime
lines((Tdat.58$coefs_Tmin.m) ~Tdat.58$Year, col="blue", lwd=1)
lines((Tdat.58$coefs_Tmax.m) ~Tdat.58$Year, col="red", lwd=3)
abline(9.8,0)  # dry adiabatic lapse rate
text(2014, 9.5, "DALR")
abline(6.49,0) # environmental lapse rate
text(2014, 6.2, "ELR")

# Add in trends IF significant
sen.58max<-zyp.sen(coefs_Tmax.m~Year, data=Tdat.58)
confint(sen.58max) # signif
usr <- par("usr")  # recent trend
clip(x1=1958, usr[2], usr[3], usr[4])
abline(coef(sen.58max), col="red", lty=2, lwd=2)

sen.78max<-zyp.sen(coefs_Tmax.m~Year, data=Tdat.78)
confint(sen.78max) # signif
usr <- par("usr")  # recent trend
clip(x1=1978, usr[2], usr[3], usr[4])
abline(coef(sen.78max), col="black", lty=3, lwd=2)

sen.98max<-zyp.sen(coefs_Tmax.m~Year, data=Tdat.98)
confint(sen.98max) # NOT signif

sen.58min<-zyp.sen(coefs_Tmin.m~Year, data=Tdat.58)
confint(sen.58min) # NS
# abline(coef(sen.58min), col="blue", lty=2)

sen.78min<-zyp.sen(coefs_Tmin.m~Year, data=Tdat.78)
confint(sen.78min) # NS

mtext(expression(Surface~-dT/dz~(degree*C~km^-1)), 
      side=2, line=3.5, cex=0.7)
#axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
legend(1958, 10, legend=c(expression(-dT[min]/dz),expression(-dT[max]/dz)),
       lwd=c(1,3), lty=1, col=c("blue", "red"), bty='n')
legend(1975, 10, legend=c(as.expression(bquote(
  1958-2017~trend:~+.(round(coef(sen.58max)[2]*10,2))~dec^-1)),
  as.expression(bquote(
    1978-2017~trend:~+.(round(coef(sen.78max)[2]*10,2))~dec^-1))),
  lwd=2, lty=c(2,3), col=c("red", "black"), bty='n')

# Legend and title for sounding/ceilometer records
plot.new()
legend("bottom", legend=c("Hilo", "Lihue"), lty=c(1,2), horiz=T)
mtext("2 PM", side=3, adj=0.2, line=-3)
mtext("2 AM", side=3, adj=0.8, line=-3)

################# DAY #################################
# DAY Sounding Mixing Ratio #####################################
plot(H.00.sfc$mixr ~H.00.sfc$X, type="l", lwd=1, 
     las=1, xaxt="n", ylab='', ylim=c(12,16))
lines(L.00.sfc$mixr ~L.00.sfc$X, lty=2, lwd=1)
mtext("Sfc mixing ratio (g/kg)", side=2, line=3.5, cex=0.7)
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))

# Add in trends IF significant
sen.78mixrH<-zyp.sen(mixr~X, data=H.00.sfc) # -0.03627 (-0.05694444, -0.01638021)
confint(sen.78mixrH) # signif
usr <- par("usr")  # recent trend
clip(x1=1978, usr[2], usr[3], usr[4])
abline(coef(sen.78mixrH), col="black", lty=1, lwd=2)

sen.78mixrL<-zyp.sen(mixr~X, data=L.00.sfc) # -0.02301 (-0.04051075, -0.005465686)
confint(sen.78mixrL) # signif
usr <- par("usr")  # recent trend
clip(x1=1978, usr[2], usr[3], usr[4])
abline(coef(sen.78mixrL), col="black", lty=2, lwd=2)

sen.98mixrH<-zyp.sen(mixr~X, data=subset(H.00.sfc, X>=1998)) # -0.0641 (-0.1189744, 0.01229167)
confint(sen.98mixrH) # NOT signif
#abline(coef(sen.98mixrH), col="black", lty=1, lwd=2)

sen.98mixrL<-zyp.sen(mixr~X, data=subset(L.00.sfc, X>=1998)) # 0.0176 (-0.03961111, 0.06805556)
confint(sen.98mixrL) # NOT signif
#abline(coef(sen.98mixrL), col="black", lty=2, lwd=2)

# DAY Sounding LCL #####################################
plot(sHilo.78$Pres..hPa..of.the.Lifted.Condensation.Level~sHilo.78$Year, 
     col="black", type="l", ylim=c(930, 890), xaxt="n", las=1)
mtext("LCL pressure (hPa)", side=2, line=3.5,cex=0.7)
lines(sLih.78$Pres..hPa..of.the.Lifted.Condensation.Level~sLih.78$Year, 
      col="black", lty=2)
# axis(side=4, labels=F, tick=T )
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))

# Add in trends IF significant
sen.78lclH<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level~Year, data=sHilo.78)
confint(sen.78lclH) # signif
usr <- par("usr")  # recent trend
clip(x1=1978, usr[2], usr[3], usr[4])
abline(coef(sen.78lclH), col="black", lty=1, lwd=2)

sen.78lclL<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level~Year, data=sLih.78)
confint(sen.78lclL) # NS
# abline(coef(sen.78lclL), col="black", lty=2, lwd=2)

sen.98lclH<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level~Year, data=sHilo.98)
confint(sen.98lclH) # signif
usr <- par("usr")  # recent trend
clip(x1=1998, usr[2], usr[3], usr[4])
abline(coef(sen.98lclH), col="black", lty=1, lwd=2)

sen.98lclL<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level~Year, data=sLih.98)
confint(sen.98lclL) # NS
# abline(coef(sen.98lclL), col="black", lty=2, lwd=2)

# DAY OVC/BKN Cloud Base Height #####################################
plot(cphto.98$phto_OBday.z~cphto.98$Year,
     type="l", las=1, xaxt="n", ylim=c(1000,1600), xlim=range(cphto.78$Year))
mtext("OVC/BKN base height (m)", side=2, line=3.5, cex=0.7)
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
lines(cphli.98$phli_OBday.z~cphli.98$Year, lty=2)

# Add in trends IF significant
sen.98OBH<-zyp.sen(phto_OBday.z~Year, data=cphto.98)
confint(sen.98OBH) # NOT signif
#abline(coef(sen.98OBH), col="black", lty=1, lwd=2)

sen.98OBL<-zyp.sen(phli_OBday.z~Year, data=cphli.98)
confint(sen.98OBL) # NS
# abline(coef(sen.98OBL), col="black", lty=2, lwd=2)

# DAY OVC/BKN Frequency #####################################
plot(cphto.98$phto_OBday.fr~cphto.98$Year, type="l",
     ylab="", las=1, xlab="Year", ylim=c(0.3,0.9), xlim=range(cphto.78$Year))
lines(cphli.98$phli_OBday.fr~cphli.98$Year, lty=2)
mtext("OVC/BKN frequency", side=2, line=3.5,cex=0.7)

# Add in trends IF significant
sen.98OBfrH<-zyp.sen(phto_OBday.fr~Year, data=cphto.98)
confint(sen.98OBfrH) # signif
usr <- par("usr")  # recent trend
clip(x1=1998, usr[2], usr[3], usr[4])
abline(coef(sen.98OBfrH), col="black", lty=1, lwd=2)

sen.98OBfrL<-zyp.sen(phli_OBday.fr~Year, data=cphli.98)
confint(sen.98OBfrL) # NS
# abline(coef(sen.98OBfrL), col="black", lty=2, lwd=2)

################# NIGHT #################################
# NIGHT Sounding Mixing ratio
plot(H.12.sfc$mixr ~H.12.sfc$X, type="l", lwd=1, 
     las=1, xaxt="n", ylab='', ylim=c(12,16), yaxt='n')
axis(side=4, las=1)
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
lines(L.12.sfc$mixr ~L.12.sfc$X, lty=2)
mtext("Sfc mixing ratio (g/kg)", side=4, line=3.5,cex=0.7)

# Add in trends IF significant
sen.78mixrH<-zyp.sen(mixr~X, data=H.12.sfc) # -0.01182 (-0.0321875  0.003444444)
confint(sen.78mixrH) # NS
# abline(coef(sen.78mixrH), col="black", lty=1, lwd=1)

sen.78mixrL<-zyp.sen(mixr~X, data=L.12.sfc) # -0.004981 (-0.01656944  0.006527778)
confint(sen.78mixrL) # NS

sen.98mixrH<-zyp.sen(mixr~X, data=subset(H.12.sfc, X>=1998)) # 
confint(sen.98mixrH) # NS
# abline(coef(sen.98mixrH), col="black", lty=1, lwd=1)

sen.98mixrL<-zyp.sen(mixr~X, data=subset(L.12.sfc, X>=1998)) # 
confint(sen.98mixrL) # NS

# NIGHT Sounding LCL #####################################
plot(sHilo.78$Pres..hPa..of.the.Lifted.Condensation.Level.1~sHilo.78$Year, 
     col="black", type="l", ylim=c(960,920), yaxt="n", xaxt="n")
lines(sLih.78$Pres..hPa..of.the.Lifted.Condensation.Level.1~sLih.78$Year, 
      col="black", lty=2)
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
axis(4, las=1)
mtext("LCL pressure (hPa)", side=4, line=3.5,cex=0.7)

# Add in trends IF significant
sen.78lclH<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level.1~Year, data=sHilo.78)
confint(sen.78lclH) # NS
# abline(coef(sen.78lclH), col="black", lty=1, lwd=2)

sen.78lclL<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level.1~Year, data=sLih.78)
confint(sen.78lclL) # NS
# abline(coef(sen.78lclL), col="black", lty=2, lwd=2)

sen.98lclH<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level.1~Year, data=sHilo.98)
confint(sen.98lclH) # signif
usr <- par("usr")  # recent trend
clip(x1=1998, usr[2], usr[3], usr[4])
abline(coef(sen.98lclH), col="black", lty=1, lwd=2)

sen.98lclL<-zyp.sen(Pres..hPa..of.the.Lifted.Condensation.Level.1~Year, data=sLih.98)
confint(sen.98lclL) # NS
# abline(coef(sen.98lclL), col="black", lty=2, lwd=2)

# NIGHT Ceilometer Cloud Base Height #####################################
plot(cphto.98$phto_OBnt.z~cphto.98$Year, 
     type="l", las=1, xaxt="n", 
     yaxt="n", ylim=c(1000,1600), xlim=range(cphto.78$Year))
lines(cphli.98$phli_OBnt.z~cphli.98$Year, col="black", lty=2)
axis(side=1, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
axis(4, las=1)
mtext("OVC/BKN base height (m)", side=4, line=3.5,cex=0.7)

# Add in trends IF significant
sen.98OBH<-zyp.sen(phto_OBnt.z~Year, data=cphto.98)
confint(sen.98OBH) # NOT signif
#abline(coef(sen.98OBH), col="black", lty=1, lwd=2)

sen.98OBL<-zyp.sen(phli_OBnt.z~Year, data=cphli.98)
confint(sen.98OBL) # NS
# abline(coef(sen.98OBL), col="black", lty=2, lwd=2)

# NIGHT Ceilometer Cloud frequency #####################################
plot(cphto.98$phto_OBnt.fr~cphto.98$Year, type="l",
     ylab="", yaxt="n", ylim=c(0.3,0.9), xlim=range(cphto.78$Year))
lines(cphli.98$phli_OBnt.fr~cphli.98$Year, lty=2)
axis(4, las=1)
mtext("OVC/BKN frequency", side=4, line=3.5,cex=0.7)

# Add in trends IF significant
sen.98OBfrH<-zyp.sen(phto_OBnt.fr~Year, data=cphto.98)
confint(sen.98OBfrH) # signif
usr <- par("usr")  # recent trend
clip(x1=1998, usr[2], usr[3], usr[4])
abline(coef(sen.98OBfrH), col="black", lty=1, lwd=2)

sen.98OBfrL<-zyp.sen(phli_OBnt.fr~Year, data=cphli.98)
confint(sen.98OBfrL) # NS
#abline(coef(sen.98OBfrL), col="black", lty=2, lwd=2)

dev.off()


#############################################################################
################# Figure 6: profiles

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

pdf("LapseFig6_RH_MR_T_WSprofile.pdf", width=8, height=7)
mat<-matrix(c(1:10), byrow=F, ncol=5, nrow=2)
layout(mat)
par(mar=c(0.1, 0.1, 0.1, 0.1), oma=c(8,6,3,2), 
    tcl=-0.3, mgp=c(2,1,0), bg=NA)

### Daytime RH
# Hilo
plot(mid~H00r[,2], xlim=c(-3, 5), pch=1, xlab="", xaxt="n", las=1)
arrows(H00r[,1], mid, H00r[,3], mid, length=0, lwd=2)
abline(v=0)
abline(h=1600)
#abline(h=2150)
#text(-3, 2150, "mean TWI base")

points(L00r[,2], mid+50, pch=8)
arrows(L00r[,1], mid+50, L00r[,3], mid+50, length=0, lwd=1)
mtext("Altitude (m), 2 PM", side=2, line=3.5, cex=0.8)
# mtext("Midday", side=3, line=0.5, cex=0.9)
axis(1, labels=F, tcl=0.3)

par(new=T)
plot(H00r$H00r.mean, mid, type="l", lwd=1, xlim=c(0,100), yaxt="n", xaxt="n")
lines(L00r$L00r.mean, mid, type="l", lwd=1, lty=2)
# axis(1, line=4)
mtext("Relative Humidity", side=3, line=1)

### Nighttime RH
plot(mid~H12r[,2], xlim=c(-3, 5), pch=1, xlab="", las=1)
arrows(H12r[,1], mid, H12r[,3], mid, length=0, lwd=2)
abline(v=0)
abline(h=1600)
# abline(h=2150)

points(L12r[,2], mid+50, pch=8)
arrows(L12r[,1], mid+50, L12r[,3], mid+50, length=0, lwd=1)
# axis(side=2, labels=F, tick=T, tcl=0.3, mgp=c(0,-1,0))
mtext("Altitude (m), 2 AM", side=2, line=3.5, cex=0.8)

# mtext("Midnight", side=3, line=0.5, cex=0.9)
# mtext(expression('RH trend'~('%'*dec^-1)), side=1, line=2.5, cex=0.9)

par(new=T)
plot(H12r$H12r.mean, mid, type="l", lwd=1,  xlim=c(0,100),
     yaxt="n", xaxt="n")
lines(L12r$L12r.mean, mid, type="l", lwd=1,  lty=2)
axis(1, line=4)
#mtext("RH", side=1, line=6, cex=0.9)

mtext(expression('RH trend'~('%'~dec^-1)), side=1, line=2.5, cex=0.7)
mtext(expression('RH'~('%')), side=1, line=6.5, cex=0.7)

#######################
### Daytime Mixing Ratio
range(H00m[,1:3], H12m[,1:3], L00m[,1:3], L12m[,1:3])

plot(mid~H00m[,2], xlim=c(-0.35, 0.45), pch=1, xlab="", xaxt="n", yaxt="n")
arrows(H00m[,1], mid, H00m[,3], mid, length=0, lwd=2)
abline(v=0)
abline(h=1600)
# abline(h=2150)

points(L00m[,2], mid+50, pch=8)
arrows(L00m[,1], mid+50, L00m[,3], mid+50, length=0, lwd=1)
axis(1, labels=F, tcl=0.3)
# mtext("Midday", side=3, line=0.5, cex=0.9)

range(H00m$H00m.mean, H12m$H12m.mean, L00m$L00m.mean, L12m$L12m.mean)
par(new=T)
plot(H00m$H00m.mean, mid, type="l",  lwd=1,  xlim=c(0,55),
     yaxt="n", ylab="", xlab="", xaxt="n")
lines(L00t$L00t.mean, mid, type="l", lwd=1,  lty=2)
# axis(1, line=4)
mtext("Mixing Ratio", side=3, line=1)

# Nighttime mixing ratio
plot(mid~H12m[,2], xlim=c(-0.35, 0.45), pch=1, xlab="", yaxt="n", bg="lightgrey")
arrows(H12m[,1], mid, H12m[,3], mid, length=0, lwd=2)
abline(v=0)
abline(h=1600)
# abline(h=2150)

points(L12m[,2], mid+50, pch=8)
arrows(L12m[,1], mid+50, L12m[,3], mid+50, length=0, lwd=1)

# mtext("Altitude (m)", side=2, line=3.5, cex=0.8)
# mtext("Midnight", side=3, line=0.5, cex=0.9)
mtext(expression('MR trend'~(g~kg^-1~dec^-1)), side=1, line=2.5, cex=0.7)
mtext(expression("MR"~(g~kg^-1)), side=1, line=6.5, cex=0.7)

par(new=T)
plot(H12m$H12m.mean, mid, type="l", lwd=1,  xlim=c(0,25), yaxt="n", xaxt="n")
lines(L12t$L12t.mean, mid, type="l", lwd=1,  lty=2)
axis(1, line=4)

#######################
### Daytime Temp
range(H00t[,1:3], H12t[,1:3], L00t[,1:3], L12t[,1:3])

plot(mid~H00t[,2], xlim=c(-0.25, 0.45), pch=1, xlab="", xaxt="n", yaxt="n")
arrows(H00t[,1], mid, H00t[,3], mid, length=0, lwd=2)
abline(v=0)
abline(h=1600)
# abline(h=2150)

points(L00t[,2], mid+50, pch=8)
arrows(L00t[,1], mid+50, L00t[,3], mid+50, length=0, lwd=1)
axis(1, labels=F, tcl=0.3)
# mtext("Midday", side=3, line=0.5, cex=0.9)

par(new=T)
plot(H00t$H00t.mean, mid, type="l", lwd=1,  xlim=c(0,25),
     yaxt="n", ylab="", xlab="", xaxt="n")
lines(L00t$L00t.mean, mid, type="l", lwd=1,  lty=2)
# axis(1, line=4)
mtext("Temperature", side=3, line=1)

# Nighttime temp
plot(mid~H12t[,2], xlim=c(-0.25, 0.45), pch=1, xlab="", yaxt="n")
arrows(H12t[,1], mid, H12t[,3], mid, length=0, lwd=2)
abline(v=0)
abline(h=1600)
# abline(h=2150)

points(L12t[,2], mid+50, pch=8)
arrows(L12t[,1], mid+50, L12t[,3], mid+50, length=0, lwd=1)

# mtext("Altitude (m)", side=2, line=3.5, cex=0.8)
# mtext("Midnight", side=3, line=0.5, cex=0.9)
mtext(expression('Temp trend'~(degree*C~dec^-1)), side=1, line=2.5, cex=0.7)
mtext(expression("Temp"~(degree*C)), side=1, line=6.5, cex=0.7)

par(new=T)
plot(H12t$H12t.mean, mid, type="l", lwd=1,  xlim=c(0,25), yaxt="n", xaxt="n")
lines(L12t$L12t.mean, mid, type="l", lwd=1,  lty=2)
axis(1, line=4)

#######################
### Daytime Wind speed
range(H00s[,1:3], H12s[,1:3], L00s[,1:3], L12s[,1:3])
range(H00s[,4], L00s[,4], H12s[,4], L12s[,4])

plot(mid~H00s[,2], xlim=c(-0.31, 0.2), pch=1, xlab="", yaxt="n", xaxt="n")
arrows(H00s[,1], mid, H00s[,3], mid, length=0, lwd=2)
abline(v=0)
abline(h=1600)
# abline(h=2150)

points(L00s[,2], mid+50, pch=8)
arrows(L00s[,1], mid+50, L00s[,3], mid+50, length=0, lwd=1)
axis(1, labels=F, tcl=0.3)
axis(4, tcl=0.3, labels=F)
# mtext("Midday", side=3, line=0.5, cex=0.9)

par(new=T)
plot(H00s$H00s.mean, mid, type="l", lwd=1,  xlim=c(2.5,8.0),
     yaxt="n", ylab="", xlab="", xaxt="n")
lines(L00s$L00s.mean, mid, type="l", lwd=1,  lty=2)
mtext("Wind Speed", side=3, line=1)

# Nighttime temp
plot(mid~H12s[,2], xlim=c(-0.31, 0.2), pch=1, xlab="", yaxt="n")
arrows(H12s[,1], mid, H12s[,3], mid, length=0, lwd=2)
abline(v=0)
abline(h=1600)
# abline(h=2150)

points(L12s[,2], mid+50, pch=8)
arrows(L12s[,1], mid+50, L12s[,3], mid+50, length=0, lwd=1)
axis(4, tcl=0.3, labels=F)

# mtext("Altitude (m)", side=2, line=3.5, cex=0.8)
# mtext("Midnight", side=3, line=0.5, cex=0.9)
mtext(expression('WS trend'~(m~s^-1~dec^-1)), side=1, line=2.5, cex=0.7)
mtext(expression("WS"~(m~s^-1)), side=1, line=6.5, cex=0.7)

par(new=T)
plot(H12s$H12s.mean, mid, type="l", lwd=1,  xlim=c(2.5,8.0), yaxt="n", xaxt="n")
lines(L12s$L12s.mean, mid, type="l", lwd=1,  lty=2)
axis(1, line=4)

### Insert legend
plot.new()
legend("left", inset=0.1, title="Trend & 95% CI",
       legend=c("Hilo", "Lihue"), cex=1.2,
       pch=c(1,8), lty=1, lwd=c(2, 1), bty='n')
legend("bottomleft", inset=0.1, title="Mean value",
       legend=c("Hilo", "Lihue"), cex=1.2, 
       lty=c(1,2), lwd=1, bty='n')

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

##########
# Calculate trends for SupportingInformation_tableS8.xlsx
sen.98relhH<-zyp.sen(relh~X, data=subset(H.00.sfc, X>=1998)) # -0.3958 (-0.6458333  -0.1083333)
confint(sen.98relhH) # NS

sen.98relhL<-zyp.sen(relh~X, data=subset(L.00.sfc, X>=1998)) # 0.1077  (-0.109375    0.2500)
confint(sen.98relhL) # NS

sen.98relhH<-zyp.sen(relh~X, data=subset(H.12.sfc, X>=1998)) # -0.2545 (-0.4513889  -0.04861111)
confint(sen.98relhH) # NS

sen.98relhL<-zyp.sen(relh~X, data=subset(L.12.sfc, X>=1998)) # -0.06073  (-0.1666667   0.04583333)
confint(sen.98relhL) # NS
###
sen.98mixrH<-zyp.sen(mixr~X, data=subset(H.00.sfc, X>=1998)) # -0.0641 (-0.1189744   0.01229167)
confint(sen.98mixrH) # NS

sen.98mixrL<-zyp.sen(mixr~X, data=subset(L.00.sfc, X>=1998)) # 0.0176  (-0.03961111   0.06805556)
confint(sen.98mixrL) # NS

sen.98mixrH<-zyp.sen(mixr~X, data=subset(H.12.sfc, X>=1998)) # -0.001641 (-0.04466667  0.04854167)
confint(sen.98mixrH) # NS

sen.98mixrL<-zyp.sen(mixr~X, data=subset(L.12.sfc, X>=1998)) # 0.007569  (-0.023250  0.04458333)
confint(sen.98mixrL) # NS
