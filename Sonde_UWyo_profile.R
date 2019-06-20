## Project: HI Temp Map
## Script: Sonde_UWyo_profile.R  (00Z only)
## Date: 6/1/2019
## Author: A. Kagawa-Viviani
## Notes: this script pulls and analyzes surface data from U Wyo raw soundings:
##    1) merge soundings 
##    2) pull surface and different layers: RH, temp, theta-e
##    3) visualize time series, export csv
##################################################################

setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping/NOT_USED/AKV_Sonde/UWyo/")
library(xts)

H.files<-list.files("Hilo_profiles/", full.names=T)
L.files<-list.files("Lihue_profiles/", full.names=T)

H<-read.csv(H.files[1], stringsAsFactors = F)
for (i in 2:length(H.files)){
  dat<-read.csv(H.files[i])
  H<-rbind(H, dat, all=T)
}

dt.utc<-as.POSIXct(paste(H$date.UTC, substr(H$tm.utc, 1,2)), 
               format="%Y-%m-%d %H", tz="UTC")
H$dt<-as.Date(format(dt.utc, tz="HST"))
  
## Hilo 2pm soundings: 0-50, 0-100, 0-800, 0-1600
h50m.00Z<-subset(H, stn=="PHTO" & tm.utc=="00Z"& hght<=50 & temp>0)
h100m.00Z<-subset(H, stn=="PHTO" & tm.utc=="00Z"& hght<=100)
h800m.00Z<-subset(H, stn=="PHTO"& tm.utc=="00Z"& hght<=800)
h1600m.00Z<-subset(H, stn=="PHTO"& tm.utc=="00Z"& hght<=1600)

## Hilo 2pm soundings: 800-1600, 800-2100, 1600-2500
h800_1600m.00Z<-subset(H, stn=="PHTO"& tm.utc=="00Z"& hght>800 & hght<=1600) # Cloud zone
h800_2100m.00Z<-subset(H, stn=="PHTO"& tm.utc=="00Z"& hght>800 & hght<=2100) # Cloud up to TWI
h1600_2500m.00Z<-subset(H, stn=="PHTO"& tm.utc=="00Z"& hght>1600 & hght<=2500) # spanning TWI

## Make into xts objects for reducing and plotting
h50m.00.xts<-xts(h50m.00Z, order.by=as.Date(h50m.00Z$dt))
h100m.00.xts<-xts(h100m.00Z, order.by=as.Date(h100m.00Z$dt))
h800m.00.xts<-xts(h800m.00Z, order.by=as.Date(h800m.00Z$dt))
h1600m.00.xts<-xts(h1600m.00Z, order.by=as.Date(h1600m.00Z$dt))

h800_1600m.00.xts<-xts(h800_1600m.00Z, order.by=as.Date(h800_1600m.00Z$dt))
h800_2100m.00.xts<-xts(h800_2100m.00Z, order.by=as.Date(h800_2100m.00Z$dt))
h1600_2500m.00.xts<-xts(h1600_2500m.00Z, order.by=as.Date(h1600_2500m.00Z$dt))

## Sounding observation precision?
precis<-tapply(h1600m.00Z$temp, h1600m.00Z$dt, FUN=function(x) length(which(!is.na(x))))
precis.xts<-xts(precis, order.by=as.Date(names(precis)))
precis.ann<-apply.yearly(precis.xts, FUN=mean, na.rm=T)  
### precision jumps from 2008-2012

## Bin to annual
## Surface
h50m.ann.t<-apply.yearly(h50m.00.xts$temp, mean, na.rm=T)
h50m.ann.rh<-apply.yearly(h50m.00.xts$relh, mean, na.rm=T)
h50m.ann.thte<-apply.yearly(h50m.00.xts$thte, mean, na.rm=T)

h100m.ann.t<-apply.yearly(h100m.00.xts$temp, mean, na.rm=T)
h100m.ann.rh<-apply.yearly(h100m.00.xts$relh, mean, na.rm=T)
h100m.ann.thte<-apply.yearly(h100m.00.xts$thte, mean, na.rm=T)

h800m.ann.t<-apply.yearly(h800m.00.xts$temp, mean, na.rm=T)
h800m.ann.rh<-apply.yearly(h800m.00.xts$relh, mean, na.rm=T)
h800m.ann.thte<-apply.yearly(h800m.00.xts$thte, mean, na.rm=T)

h1600m.ann.t<-apply.yearly(h1600m.00.xts$temp, mean, na.rm=T)
h1600m.ann.rh<-apply.yearly(h1600m.00.xts$relh, mean, na.rm=T)
h1600m.ann.thte<-apply.yearly(h1600m.00.xts$thte, mean, na.rm=T)

# Clouds and above
h800_1600m.ann.t<-apply.yearly(h800_1600m.00.xts$temp, mean, na.rm=T)
h800_1600m.ann.rh<-apply.yearly(h800_1600m.00.xts$relh, mean, na.rm=T)
h800_1600m.ann.thte<-apply.yearly(h800_1600m.00.xts$thte, mean, na.rm=T)

h800_2100m.ann.t<-apply.yearly(h800_2100m.00.xts$temp, mean, na.rm=T)
h800_2100m.ann.rh<-apply.yearly(h800_2100m.00.xts$relh, mean, na.rm=T)
h800_2100m.ann.thte<-apply.yearly(h800_2100m.00.xts$thte, mean, na.rm=T)

h1600_2500m.ann.t<-apply.yearly(h1600_2500m.00.xts$temp, mean, na.rm=T)
h1600_2500m.ann.rh<-apply.yearly(h1600_2500m.00.xts$relh, mean, na.rm=T)
h1600_2500m.ann.thte<-apply.yearly(h1600_2500m.00.xts$thte, mean, na.rm=T)
# index()

## Merge and visualize
rh.H<-merge(h50m.ann.rh, h100m.ann.rh, h800m.ann.rh, h1600m.ann.rh, 
          h800_1600m.ann.rh, h800_2100m.ann.rh, h1600_2500m.ann.rh)
plot(rh.H, col=c("red", "purple", "green", "blue", 
               "gray", "turquoise", "black"),
     main="Hilo", lty=c(rep(1, 4), rep(2, 3)))
names(rh.H)<-paste0("RH_", c("0-50m", "0-100m", "0-800m", "0-1600", 
                             "800-1600m", "800-2100m", "1600-2500m"))

temp.H<-merge(h50m.ann.t, h100m.ann.t, h800m.ann.t, h1600m.ann.t, 
            h800_1600m.ann.t, h800_2100m.ann.t, h1600_2500m.ann.t)
plot(temp.H, col=c("red", "purple", "green", "blue", 
                 "gray", "turquoise", "black"),
     main="Hilo", lty=c(rep(1, 4), rep(2, 3)))
names(temp.L)<-paste0("T_", c("0-50m", "0-100m", "0-800m", "0-1600", 
                             "800-1600m", "800-2100m", "1600-2500m"))

thte.H<-merge(h50m.ann.thte, h100m.ann.thte, h800m.ann.thte, h1600m.ann.thte, 
              h800_1600m.ann.thte, h800_2100m.ann.thte, h1600_2500m.ann.thte)
plot(thte.H, col=c("red", "purple", "green", "blue", 
                   "gray", "turquoise", "black"),
     main="Hilo", lty=c(rep(1, 4), rep(2, 3)))
names(thte.L)<-paste0("thte_", c("0-50m", "0-100m", "0-800m", "0-1600", 
                             "800-1600m", "800-2100m", "1600-2500m"))

#### Lihue ########################################################################
L<-read.csv(L.files[1], stringsAsFactors = F)
for (i in 2:length(L.files)){
  dat<-read.csv(L.files[i])
  L<-rbind(L, dat, all=T)
}

dt.utc<-as.POSIXct(paste(L$date.UTC, substr(L$tm.utc, 1,2)), 
                   format="%Y-%m-%d %H", tz="UTC")
L$dt<-as.Date(format(dt.utc, tz="HST"))

## Lihue 2pm soundings: 0-50, 0-100, 0-800, 0-1600
l50m.00Z<-subset(L, stn=="PHLI" & tm.utc=="00Z"& hght<=50 & temp>0)
l100m.00Z<-subset(L, stn=="PHLI" & tm.utc=="00Z"& hght<=100)
l800m.00Z<-subset(L, stn=="PHLI"& tm.utc=="00Z"& hght<=800)
l1600m.00Z<-subset(L, stn=="PHLI"& tm.utc=="00Z"& hght<=1600)

## Lihue 2pm soundings: 800-1600, 800-2100, 1600-2500
l800_1600m.00Z<-subset(L, stn=="PHLI"& tm.utc=="00Z"& hght>800 & hght<=1600) # Cloud zone
l800_2100m.00Z<-subset(L, stn=="PHLI"& tm.utc=="00Z"& hght>800 & hght<=2100) # Cloud up to TWI
l1600_2500m.00Z<-subset(L, stn=="PHLI"& tm.utc=="00Z"& hght>1600 & hght<=2500) # spanning TWI


## Make into xts objects for reducing and plotting
l50m.00.xts<-xts(l50m.00Z, order.by=as.Date(l50m.00Z$dt))
l100m.00.xts<-xts(l100m.00Z, order.by=as.Date(l100m.00Z$dt))
l800m.00.xts<-xts(l800m.00Z, order.by=as.Date(l800m.00Z$dt))
l1600m.00.xts<-xts(l1600m.00Z, order.by=as.Date(l1600m.00Z$dt))

l800_1600m.00.xts<-xts(l800_1600m.00Z, order.by=as.Date(l800_1600m.00Z$dt))
l800_2100m.00.xts<-xts(l800_2100m.00Z, order.by=as.Date(l800_2100m.00Z$dt))
l1600_2500m.00.xts<-xts(l1600_2500m.00Z, order.by=as.Date(l1600_2500m.00Z$dt))

## Sounding observation precision?
precis<-tapply(l1600m.00Z$temp, l1600m.00Z$dt, FUN=function(x) length(which(!is.na(x))))
precis.xts<-xts(precis, order.by=as.Date(names(precis)))
precis.ann<-apply.yearly(precis.xts, FUN=mean, na.rm=T)  
### precision jumps from 2008-2012

## Bin to annual
## Surface
l50m.ann.t<-apply.yearly(l50m.00.xts$temp, mean, na.rm=T)
l50m.ann.rh<-apply.yearly(l50m.00.xts$relh, mean, na.rm=T)
l50m.ann.thte<-apply.yearly(l50m.00.xts$thte, mean, na.rm=T)

l100m.ann.t<-apply.yearly(l100m.00.xts$temp, mean, na.rm=T)
l100m.ann.rh<-apply.yearly(l100m.00.xts$relh, mean, na.rm=T)
l100m.ann.thte<-apply.yearly(l100m.00.xts$thte, mean, na.rm=T)

l800m.ann.t<-apply.yearly(l800m.00.xts$temp, mean, na.rm=T)
l800m.ann.rh<-apply.yearly(l800m.00.xts$relh, mean, na.rm=T)
l800m.ann.thte<-apply.yearly(l800m.00.xts$thte, mean, na.rm=T)

l1600m.ann.t<-apply.yearly(l1600m.00.xts$temp, mean, na.rm=T)
l1600m.ann.rh<-apply.yearly(l1600m.00.xts$relh, mean, na.rm=T)
l1600m.ann.thte<-apply.yearly(l1600m.00.xts$thte, mean, na.rm=T)

# Clouds and above
l800_1600m.ann.t<-apply.yearly(l800_1600m.00.xts$temp, mean, na.rm=T)
l800_1600m.ann.rh<-apply.yearly(l800_1600m.00.xts$relh, mean, na.rm=T)
l800_1600m.ann.thte<-apply.yearly(l800_1600m.00.xts$thte, mean, na.rm=T)

l800_2100m.ann.t<-apply.yearly(l800_2100m.00.xts$temp, mean, na.rm=T)
l800_2100m.ann.rh<-apply.yearly(l800_2100m.00.xts$relh, mean, na.rm=T)
l800_2100m.ann.thte<-apply.yearly(l800_2100m.00.xts$thte, mean, na.rm=T)

l1600_2500m.ann.t<-apply.yearly(l1600_2500m.00.xts$temp, mean, na.rm=T)
l1600_2500m.ann.rh<-apply.yearly(l1600_2500m.00.xts$relh, mean, na.rm=T)
l1600_2500m.ann.thte<-apply.yearly(l1600_2500m.00.xts$thte, mean, na.rm=T)

## Merge and visualize
rh.L<-merge(l50m.ann.rh, l100m.ann.rh, l800m.ann.rh, l1600m.ann.rh, 
          l800_1600m.ann.rh, l800_2100m.ann.rh, l1600_2500m.ann.rh)
plot(rh.L, col=c("red", "purple", "green", "blue", 
               "gray", "turquoise", "black"),
     main="Lihue", lty=c(rep(1, 4), rep(2, 3)))
names(rh.L)<-paste0("RH_", c("0-50m", "0-100m", "0-800m", "0-1600", 
               "800-1600m", "800-2100m", "1600-2500m"))

temp.L<-merge(l50m.ann.t, l100m.ann.t, l800m.ann.t, l1600m.ann.t, 
              l800_1600m.ann.t, l800_2100m.ann.t, l1600_2500m.ann.t)
plot(temp.L, col=c("red", "purple", "green", "blue", 
                   "gray", "turquoise", "black"),
     main="Lihue", lty=c(rep(1, 4), rep(2, 3)))
names(temp.L)<-paste0("T_", c("0-50m", "0-100m", "0-800m", "0-1600", 
                             "800-1600m", "800-2100m", "1600-2500m"))

thte.L<-merge(l50m.ann.thte, l100m.ann.thte, l800m.ann.thte, l1600m.ann.thte, 
              l800_1600m.ann.thte, l800_2100m.ann.thte, l1600_2500m.ann.thte)
plot(thte.L, col=c("red", "purple", "green", "blue", 
                   "gray", "turquoise", "black"),
     main="Lihue", lty=c(rep(1, 4), rep(2, 3)))
names(thte.L)<-paste0("thte_", c("0-50m", "0-100m", "0-800m", "0-1600", 
                             "800-1600m", "800-2100m", "1600-2500m"))

write.csv(as.data.frame(rh.H), "RH_layers_Hilo_00Z.csv", row.names=T)
write.csv(as.data.frame(rh.L), "RH_layers_Lihue_00Z.csv", row.names=T)

write.csv(as.data.frame(temp.H), "TempC_layers_Hilo_00Z.csv", row.names=T)
write.csv(as.data.frame(temp.L), "TempC_layers_Lihue_00Z.csv", row.names=T)

write.csv(as.data.frame(thte.H), "ThetaeK_layers_Hilo_00Z.csv", row.names=T)
write.csv(as.data.frame(thte.L), "ThetaeK_layers_Lihue_00Z.csv", row.names=T)

###### Windowing for recent period to compare with Longman et al 2015
rh.L80<-window(rh.L, start="1980-01-01", end="2018-12-31")
rh.H80<-window(rh.H, start="1980-01-01", end="2018-12-31")

t.L80<-window(temp.L, start="1980-01-01", end="2018-12-31")
t.H80<-window(temp.H, start="1980-01-01", end="2018-12-31")

thte.L80<-window(thte.L, start="1980-01-01", end="2018-12-31")
thte.H80<-window(thte.H, start="1980-01-01", end="2018-12-31")

#precis.80<-window(precis.ann, start="1980-01-01", end="2018-12-31")

pdf("Sounding_profiles_00Z.pdf")
par(mfrow=c(1,2))
h1<-plot(rh.H80, col=c("red", "purple", "green", "blue", 
                 "gray", "turquoise", "black"),
     main="Hilo", lty=c(rep(1, 4), rep(2, 3)), ylim=c(45, 90))
h1<-addLegend("bottomleft",ncol=2, cex=0.5,
          legend.names=c("0-50m", "0-100m", "0-800m", "0-1600", 
                         "800-1600m", "800-2100m", "1600-2500m"),
          col=c("red", "purple", "green", "blue", 
                "gray", "turquoise", "black"),
          lty=c(rep(1, 4), rep(2, 3)))
h2<-plot(rh.L80, col=c("red", "purple", "green", "blue", 
                 "gray", "turquoise", "black"),
     main="Lihue", lty=c(rep(1, 4), rep(2, 3)), ylim=c(45, 90))
h1
h2

t1<-plot(t.H80, col=c("red", "purple", "green", "blue", 
                       "gray", "turquoise", "black"),
         main="Hilo", lty=c(rep(1, 4), rep(2, 3)))
t1<-addLegend("bottomleft",ncol=2, cex=0.5,
              legend.names=c("0-50m", "0-100m", "0-800m", "0-1600", 
                             "800-1600m", "800-2100m", "1600-2500m"),
              col=c("red", "purple", "green", "blue", 
                    "gray", "turquoise", "black"),
              lty=c(rep(1, 4), rep(2, 3)))
t2<-plot(t.L80, col=c("red", "purple", "green", "blue", 
                       "gray", "turquoise", "black"),
         main="Lihue", lty=c(rep(1, 4), rep(2, 3)))
t1
t2

th1<-plot(thte.H80, col=c("red", "purple", "green", "blue", 
                      "gray", "turquoise", "black"),
         main="Hilo", lty=c(rep(1, 4), rep(2, 3)))
th1<-addLegend("bottomleft",ncol=2, cex=0.5,
              legend.names=c("0-50m", "0-100m", "0-800m", "0-1600", 
                             "800-1600m", "800-2100m", "1600-2500m"),
              col=c("red", "purple", "green", "blue", 
                    "gray", "turquoise", "black"),
              lty=c(rep(1, 4), rep(2, 3)))
th2<-plot(thte.L80, col=c("red", "purple", "green", "blue", 
                        "gray", "turquoise", "black"),
          main="Lihue", lty=c(rep(1, 4), rep(2, 3)))
th1
th2
dev.off()
