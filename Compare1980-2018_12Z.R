##########################################################
## Compare1980-2018.R
## Project: HI Temp
## Author: Aurora Kagawa-Viviani
## Date: 5/30/2019
## Notes: Script for looking at lapse rate trends:
#      - breakpoint analysis
#      - are trends driven by certain periods?
#
#   Bring in sounding data, ceilometer
#    
# 

setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping")

# Fitted temperature indices
Tdat<-read.csv("annual_lm_2017.csv", stringsAsFactors = F)
Tdat$Year<-Tdat$coefs_Year
Tdat<-Tdat[,-c(1,2)]

# Soundings
sHilo<-read.csv("NOT_USED/AKV_Sonde/UWyo/Hilo_annual.csv", stringsAsFactors = F)
sLih<-read.csv("NOT_USED/AKV_Sonde/UWyo/Lihue_annual.csv", stringsAsFactors = F)
sHilo$Year<-as.numeric(substr(sHilo$X, 1, 4))
sLih$Year<-as.numeric(substr(sLih$X, 1, 4))

# Ceilometer
ceil<-read.csv("NOT_USED/ASOS_Cloud/CeilometerOB_all_yearly.csv", stringsAsFactors = F)
ceil$Year<-as.numeric(substr(ceil$X, 1, 4))

cphto<-ceil[,grep("phto", names(ceil))]
cphto$Year<-ceil$Year 

cphli<-ceil[,grep("phli", names(ceil))]
cphli$Year<-ceil$Year

# Sounding layers information
RH_Hilo<-read.csv("NOT_USED/AKV_Sonde/UWyo/RH_layers_Hilo_12Z.csv", stringsAsFactors = F)
RH_Lih<-read.csv("NOT_USED/AKV_Sonde/UWyo/RH_layers_Lihue_12Z.csv", stringsAsFactors = F)

T_Hilo<-read.csv("NOT_USED/AKV_Sonde/UWyo/TempC_layers_Hilo_12Z.csv", stringsAsFactors = F)
T_Lih<-read.csv("NOT_USED/AKV_Sonde/UWyo/TempC_layers_Lihue_12Z.csv", stringsAsFactors = F)

thte_Hilo<-read.csv("NOT_USED/AKV_Sonde/UWyo/ThetaeK_layers_Hilo_12Z.csv", stringsAsFactors = F)
thte_Lih<-read.csv("NOT_USED/AKV_Sonde/UWyo/ThetaeK_layers_Lihue_12Z.csv", stringsAsFactors = F)

RH_Hilo$Year<-T_Hilo$Year<-thte_Hilo$Year<-as.numeric(substr(RH_Hilo$X, 1, 4))
RH_Lih$Year<-T_Lih$Year<-thte_Lih$Year<-as.numeric(substr(RH_Lih$X, 1, 4))

####### Focus on most interesting and informative metrics and overlapping period
range(Tdat$Year)   # 1905-2017

range(sHilo$Year)  # 1972-2019  but weird behavior around then?
range(sLih$Year)   # 1972-2019

range(cphto$Year)  # 1948-2019
range(cphli$Year)

range(RH_Hilo$Year) #1972-2018
range(RH_Lih$Year)  

Tdat.80<-Tdat[Tdat$Year>=1980,]

sHilo.80<-sHilo[sHilo$Year>=1980 & sHilo$Year<=2017,]
sLih.80<-sLih[sLih$Year>=1980 & sHilo$Year<=2017,]

cphto.80<-cphto[cphto$Year>=1980 & cphto$Year<=2017,]
cphli.80<-cphli[cphli$Year>=1980& cphli$Year<=2017,]

RH_Hilo.80<-RH_Hilo[RH_Hilo$Year>=1980 & RH_Hilo$Year<=2017,]
RH_Lih.80<-RH_Lih[RH_Lih$Year>=1980 & RH_Lih$Year<=2017,]
T_Hilo.80<-T_Hilo[T_Hilo$Year>=1980 & T_Hilo$Year<=2017,]
T_Lih.80<-T_Lih[T_Lih$Year>=1980 & T_Lih$Year<=2017,]
thte_Hilo.80<-thte_Hilo[thte_Hilo$Year>=1980 & thte_Hilo$Year<=2017,]
thte_Lih.80<-thte_Lih[thte_Lih$Year>=1980 & thte_Lih$Year<=2017,]

##### Compare PHTO and PHLI annual series
par(mfrow=c(2,2))
for (i in 1:8){
  metr<-unlist(strsplit(names(cphto.80)[i], "_"))[2]
  plot(cphto.80[,i]~cphto.80$Year, 
       main=metr,ylab=metr, 
       ylim=range(cphto.80[,i], cphli.80[,i], na.rm=T),
       type="l", col="red")
  lines(cphli.80[,i]~cphli.80$Year, col="green")
}

##### Compare Tmin
plot(Tdat.80$coefs_Tmin.b~Tdat.80$Year,
     type="l", col="black", lwd=2)
plot(Tdat.80$coefs_Tmin.m~Tdat.80$Year,
     type="l", col="black", lwd=2)

plot(cphto.80$phto_OBnt.z~cphto.80$Year, type="l", col="red", 
     ylim=range(cphto.80$phto_OBnt.z, cphto.80$phto_OBSFnt.z, na.rm=T))
lines(cphto.80$phto_OBSFnt.z~cphto.80$Year, col="purple")

plot(cphto.80$phto_OBnt.fr~cphto.80$Year, type="l", col="red", 
     ylim=range(cphto.80$phto_OBnt.fr, cphto.80$phto_OBSFnt.fr, na.rm=T))
lines(cphto.80$phto_OBSFnt.fr~cphto.80$Year, col="purple")

plot(sHilo.80$Mean.mixed.layer.mixing.ratio.1~sHilo.80$Year, 
     type="l", col="red")
plot(sHilo.80$Precipitable.water..mm..for.entire.sounding.1~sHilo.80$Year, 
     type="l", col="red")
plot(sHilo.80$Pres..hPa..of.the.Lifted.Condensation.Level.1~sHilo.80$Year, 
     type="l", col="red")
plot(sHilo.80$LFCT.using.virtual.temperature.1~sHilo.80$Year, 
     type="l", col="red")

par(mfrow=c(5,5))
for (i in 26:49){   # look at all the indices over 1980-2017
  plot(sHilo.80[,i]~sHilo.80$Year, type="l")
}
par(mfrow=c(5,5))
for (i in 26:49){   # look at all the indices over 1980-2017
  plot(sLih.80[,i]~sLih.80$Year, type="l")
}
## Compare nighttime LCL to that from ceilometer for Hilo (left) and Lihue (right)
par(mfcol=c(2,2))
plot(cphto.80$phto_OBnt.z~cphto.80$Year, col="red",type="l",
     ylim=range(cphto.80$phto_OBnt.z, cphto.80$phto_OBSFnt.z, na.rm=T))
lines(cphto.80$phto_OBSFnt.z~cphto.80$Year, col="purple")
plot(sHilo.80$Pres..hPa..of.the.Lifted.Condensation.Level.1~sHilo.80$Year, col="red", type="l")

summary(lm(cphto.80$phto_OBSFnt.z~sHilo.80$Pres..hPa..of.the.Lifted.Condensation.Level.1))
plot(cphto.80$phto_OBSFnt.z~sHilo.80$Pres..hPa..of.the.Lifted.Condensation.Level.1)

plot(cphli.80$phli_OBnt.z~cphli.80$Year, col="red",type="l",
     ylim=range(cphli.80$phli_OBnt.z, cphli.80$phli_OBSFnt.z, na.rm=T))
lines(cphli.80$phli_OBSFnt.z~cphli.80$Year, col="purple")
plot(sLih.80$Pres..hPa..of.the.Lifted.Condensation.Level.1~sLih.80$Year, col="red", type="l")

summary(lm(cphli.80$phli_OBSFnt.z~sLih.80$Pres..hPa..of.the.Lifted.Condensation.Level.1))
plot(cphli.80$phli_OBSFnt.z~sLih.80$Pres..hPa..of.the.Lifted.Condensation.Level.1)

## Compare nighttime frequency of OB/OBSF to LCL

#pdf("MDNT_Sfc_Sound_Ceil.pdf", width=8, height=9)
par(mfcol=c(3,2), mar=c(2,4,3,4))

# Surface
plot((Tdat.80$coefs_Tmin.m)*(-1) ~Tdat.80$Year, 
     col="black",type="l", main="Surface",
     ylab="-dT/dz", xlab="Year")
par(new=T)
plot(Tdat.80$coefs_Tmin.b ~ Tdat.80$Year, col="red", type="l", lty=2,
     yaxt="n", ylab="", xlab="")
axis(4)
mtext("Tmin (C)", side=4, line=3,cex=0.7)

# Sounding
plot(RH_Hilo.80$RH_800.1600m ~RH_Hilo.80$Year, 
     col="dark gray",type="l",lty=2, main="Hilo soundings",
     ylab="RH", xlab="Year", ylim=c(75,95))
lines(RH_Hilo.80$RH_0.1600 ~RH_Hilo.80$Year, col="blue")
lines(RH_Hilo.80$RH_0.50m ~RH_Hilo.80$Year, col="red")
par(new=T)
plot(sHilo.80$Pres..hPa..of.the.Lifted.Condensation.Level.1~sHilo.80$Year, 
     col="black", type="l", ylim=c(920,960), yaxt="n", ylab="", xlab="")
axis(4)
mtext("LCL pressure (hPa)", side=4, line=3,cex=0.7)

# Ceilometer
plot(cphto.80$phto_OBnt.fr~cphto.80$Year, col="red",type="l",
     ylab="Frequency", xlab="Year", ylim=c(0,0.2), main="Hilo ceilometer")
#lines(cphto.80$phto_OBSFnt.fr~cphto.80$Year, col="purple")
lines(cphto.80$phto_CLRnt.fr~cphto.80$Year, col="black")

par(new=T)
plot(cphto.80$phto_OBnt.z~cphto.80$Year, col="red",type="l",lty=2, yaxt="n",
     ylab="", xlab="", ylim=c(600,1000))
lines(cphto.80$phto_OBSFnt.z~cphto.80$Year, col="purple", lty=2)
axis(4)
mtext("Ceiling height (m)", side=4, line=3,cex=0.7)

################# Lihue #################################
# Surface
plot((Tdat.80$coefs_Tmin.m)*(-1) ~Tdat.80$Year, 
     col="black",type="l", main="Surface",
     ylab="-dT/dz", xlab="Year")
par(new=T)
plot(Tdat.80$coefs_Tmin.b ~ Tdat.80$Year, col="red", type="l", lty=2,
     yaxt="n", ylab="", xlab="")
axis(4)
mtext("Tmin (C)", side=4, line=3,cex=0.7)

# Sounding
plot(RH_Lih.80$RH_800.1600m ~RH_Lih.80$Year, 
     col="dark gray",type="l",lty=2, main="Lihue soundings",
     ylab="RH", xlab="Year", ylim=c(75,95))
lines(RH_Lih.80$RH_0.1600 ~RH_Lih.80$Year, col="blue")
lines(RH_Lih.80$RH_0.50m ~RH_Lih.80$Year, col="red")
par(new=T)
plot(sLih.80$Pres..hPa..of.the.Lifted.Condensation.Level.1~sLih.80$Year, 
     col="black", type="l", ylim=c(920,960), yaxt="n", ylab="", xlab="")
axis(4)
mtext("LCL pressure (hPa)", side=4, line=3,cex=0.7)

# Ceilometer
plot(cphli.80$phli_OBnt.fr~cphli.80$Year, col="red",type="l",
     ylab="Frequency", xlab="Year", ylim=c(0,0.2), main="Lihue ceilometer")
#lines(cphli.80$phli_OBSFnt.fr~cphli.80$Year, col="purple")
lines(cphli.80$phli_CLRnt.fr~cphli.80$Year, col="black")

par(new=T)
plot(cphli.80$phli_OBnt.z~cphli.80$Year, col="red",type="l",lty=2, yaxt="n",
     ylab="", xlab="", ylim=c(600,1000))
lines(cphli.80$phli_OBSFnt.z~cphli.80$Year, col="purple", lty=2)
axis(4)
mtext("Ceiling height (m)", side=4, line=3,cex=0.7)

## Relationship of LCL and Ceilometer
par(mfrow=c(2,2))
summary(lm(cphto.80$phto_OBSFnt.z~sHilo.80$Pres..hPa..of.the.Lifted.Condensation.Level.1))
plot(cphto.80$phto_OBSFnt.z~sHilo.80$Pres..hPa..of.the.Lifted.Condensation.Level.1)
lm.phto_ceil.lcl<-lm(cphto.80$phto_OBSFnt.z~sHilo.80$Pres..hPa..of.the.Lifted.Condensation.Level.1)
abline(lm.phto_ceil.lcl)

summary(lm(cphli.80$phli_OBSFnt.z~sLih.80$Pres..hPa..of.the.Lifted.Condensation.Level.1))
plot(cphli.80$phli_OBSFnt.z~sLih.80$Pres..hPa..of.the.Lifted.Condensation.Level.1)
lm.phli_ceil.lcl<-lm(cphli.80$phli_OBSFnt.z~sLih.80$Pres..hPa..of.the.Lifted.Condensation.Level.1)
abline(lm.phli_ceil.lcl)

## Relationship of LCL and RH
rh.lcl<-merge(RH_Hilo.80, sHilo.80, by="Year", all=T)

summary(lm(Pres..hPa..of.the.Lifted.Condensation.Level.1~RH_800.1600m, data=rh.lcl))
plot(Pres..hPa..of.the.Lifted.Condensation.Level.1~RH_800.1600m, data=rh.lcl)
lm.phto_rh.lcl<-lm(Pres..hPa..of.the.Lifted.Condensation.Level.1~RH_800.1600m, data=rh.lcl)
abline(lm.phto_rh.lcl)

rh.lcl.lih<-merge(RH_Lih.80, sLih.80, by="Year", all=T)
summary(lm(Pres..hPa..of.the.Lifted.Condensation.Level.1~RH_800.1600m, data=rh.lcl.lih))
plot(Pres..hPa..of.the.Lifted.Condensation.Level.1~RH_800.1600m, data=rh.lcl.lih)
lm.phli_rh.lcl<-lm(Pres..hPa..of.the.Lifted.Condensation.Level.1~RH_800.1600m, data=rh.lcl.lih)
abline(lm.phli_rh.lcl)


#dev.off()

