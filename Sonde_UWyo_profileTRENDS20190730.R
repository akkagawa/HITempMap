## Project: HI Temp Map
## Script: Sonde_UWyo_profileTRENDS.R  (Temp, RH, Mixing ration)
## Date: 6/6/2019
## Author: A. Kagawa-Viviani
## Notes: this script pulls and analyzes surface data from U Wyo raw soundings:
##    1) slices 
##    2) sen's slope
##    3) visualize time series, export csv
##################################################################

setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping/NOT_USED/AKV_Sonde/UWyo/")
library(xts)

H.files<-list.files("Hilo_profiles/", full.names=T)
L.files<-list.files("Lihue_profiles/", full.names=T)

##### Hilo 
H<-read.csv(H.files[1], stringsAsFactors = F)
for (i in 2:length(H.files)){
  dat<-read.csv(H.files[i])
  H<-rbind(H, dat, all=T)
}

dt.utc<-as.POSIXct(paste(H$date.UTC, substr(H$tm.utc, 1,2)), 
                   format="%Y-%m-%d %H", tz="UTC")
H$dt<-as.Date(format(dt.utc, tz="HST"))

##### Lihue
L<-read.csv(L.files[1], stringsAsFactors = F)
for (i in 2:length(L.files)){
  dat<-read.csv(L.files[i])
  L<-rbind(L, dat, all=T)
}

dt.utc<-as.POSIXct(paste(L$date.UTC, substr(L$tm.utc, 1,2)), 
                   format="%Y-%m-%d %H", tz="UTC")
L$dt<-as.Date(format(dt.utc, tz="HST"))

## Surface observations <1000 ##############################################
H.00.sfc<-subset(H, tm.utc=="00Z"& hght<20) # >1000 hPa
H.12.sfc<-subset(H, tm.utc=="12Z"& hght<20)
L.00.sfc<-subset(L, tm.utc=="00Z"& hght<50)
L.12.sfc<-subset(L, tm.utc=="12Z"& hght<50)

## Make into xts objects for reducing and plotting
H.00.sfc.xts<-xts(H.00.sfc[,-c(1:3,15)], order.by=as.Date(H.00.sfc$dt))
H.12.sfc.xts<-xts(H.12.sfc[,-c(1:3,15)], order.by=as.Date(H.12.sfc$dt))
L.00.sfc.xts<-xts(L.00.sfc[,-c(1:3,15)], order.by=as.Date(L.00.sfc$dt))
L.12.sfc.xts<-xts(L.12.sfc[,-c(1:3,15)], order.by=as.Date(L.12.sfc$dt))

H.00.sfc.mo<-aggregate(H.00.sfc.xts, by=list(my=substr(index(H.00.sfc.xts), start=1, stop=7)), 
                    median, na.rm=T)
H.12.sfc.mo<-aggregate(H.12.sfc.xts, by=list(my=substr(index(H.12.sfc.xts), start=1, stop=7)), 
                    median, na.rm=T)
L.00.sfc.mo<-aggregate(L.00.sfc.xts, by=list(my=substr(index(L.00.sfc.xts), start=1, stop=7)), 
                    median, na.rm=T)
L.12.sfc.mo<-aggregate(L.12.sfc.xts, by=list(my=substr(index(L.12.sfc.xts), start=1, stop=7)), 
                    median, na.rm=T)

H.00.sfc.yr<-aggregate(H.00.sfc.mo, by=list(my=substr(index(H.00.sfc.mo), start=1, stop=4)), 
                   mean, na.rm=T)
H.12.sfc.yr<-aggregate(H.12.sfc.mo, by=list(my=substr(index(H.12.sfc.mo), start=1, stop=4)), 
                   mean, na.rm=T)
L.00.sfc.yr<-aggregate(L.00.sfc.mo, by=list(my=substr(index(L.00.sfc.mo), start=1, stop=4)), 
                   mean, na.rm=T)
L.12.sfc.yr<-aggregate(L.12.sfc.mo, by=list(my=substr(index(L.12.sfc.mo), start=1, stop=4)), 
                   mean, na.rm=T)

H.00.sfc.yr78<-subset(H.00.sfc.yr, index(H.00.sfc.yr)>1977&index(H.00.sfc.yr)<2018)
H.12.sfc.yr78<-subset(H.12.sfc.yr, index(H.12.sfc.yr)>1977&index(H.12.sfc.yr)<2018)
L.00.sfc.yr78<-subset(L.00.sfc.yr, index(L.00.sfc.yr)>1977&index(L.00.sfc.yr)<2018)
L.12.sfc.yr78<-subset(L.12.sfc.yr, index(L.12.sfc.yr)>1977&index(L.12.sfc.yr)<2018)

plot(H.00.sfc.yr78)
plot(H.12.sfc.yr78)
plot(L.00.sfc.yr78)
plot(L.12.sfc.yr78)

write.csv(as.data.frame(H.00.sfc.yr78), "H.00.sfc.yr78.csv", row.names=T)
write.csv(as.data.frame(H.12.sfc.yr78), "H.12.sfc.yr78.csv", row.names=T)
write.csv(as.data.frame(L.00.sfc.yr78), "L.00.sfc.yr78.csv", row.names=T)
write.csv(as.data.frame(L.12.sfc.yr78), "L.12.sfc.yr78.csv", row.names=T)

library(Kendall)
library(zyp)

yrs<-1978:2017   ## same outcome even if you add 2018
sfc.rh.H.00<-as.numeric(H.00.sfc.yr78$relh)
sfc.mr.H.00<-as.numeric(H.00.sfc.yr78$mixr)
zs<-zyp.sen(sfc.rh.H.00~yrs); confint(zs) # decreasing
zs<-zyp.sen(sfc.mr.H.00~yrs); confint(zs) # decreasing

sfc.rh.H.12<-as.numeric(H.12.sfc.yr78$relh)
sfc.mr.H.12<-as.numeric(H.12.sfc.yr78$mixr)
zs<-zyp.sen(sfc.rh.H.12~yrs); confint(zs) # no trend
zs<-zyp.sen(sfc.mr.H.12~yrs); confint(zs) # no trend

sfc.rh.L.00<-as.numeric(L.00.sfc.yr78$relh)
sfc.mr.L.00<-as.numeric(L.00.sfc.yr78$mixr)
zs<-zyp.sen(sfc.rh.L.00~yrs); confint(zs) # no trend
zs<-zyp.sen(sfc.mr.L.00~yrs); confint(zs) # decreasing

sfc.rh.L.12<-as.numeric(L.12.sfc.yr78$relh)
sfc.mr.L.12<-as.numeric(L.12.sfc.yr78$mixr)
zs<-zyp.sen(sfc.rh.L.12~yrs); confint(zs) # no trend
zs<-zyp.sen(sfc.mr.H.12~yrs); confint(zs) # no trend



## Standard pressure levels ####################################################
H.00.1000<-subset(H, tm.utc=="00Z"& pres==1000) # 1000 hPa
H.12.1000<-subset(H, tm.utc=="12Z"& pres==1000)
L.00.1000<-subset(L, tm.utc=="00Z"& pres==1000)
L.12.1000<-subset(L, tm.utc=="12Z"& pres==1000)

H.00.700<-subset(H, tm.utc=="00Z"& pres==700)  # 700 hPa
H.12.700<-subset(H, tm.utc=="12Z"& pres==700)
L.00.700<-subset(L, tm.utc=="00Z"& pres==700)
L.12.700<-subset(L, tm.utc=="12Z"& pres==700)

## Make into xts objects for reducing and plotting
H.00.1000.xts<-xts(H.00.1000[,-c(1:3,15)], order.by=as.Date(H.00.1000$dt))
H.12.1000.xts<-xts(H.12.1000[,-c(1:3,15)], order.by=as.Date(H.12.1000$dt))
L.00.1000.xts<-xts(L.00.1000[,-c(1:3,15)], order.by=as.Date(L.00.1000$dt))
L.12.1000.xts<-xts(L.12.1000[,-c(1:3,15)], order.by=as.Date(L.12.1000$dt))

H.00.700.xts<-xts(H.00.700[,-c(1:3,15)], order.by=as.Date(H.00.700$dt))
H.12.700.xts<-xts(H.12.700[,-c(1:3,15)], order.by=as.Date(H.12.700$dt))
L.00.700.xts<-xts(L.00.700[,-c(1:3,15)], order.by=as.Date(L.00.700$dt))
L.12.700.xts<-xts(L.12.700[,-c(1:3,15)], order.by=as.Date(L.12.700$dt))

# Calculate LTS as theta.700-theta.1000
H.00<-merge(H.00.1000.xts, H.00.700.xts)
H.00$lts<-H.00$thta.1-H.00$thta

H.12<-merge(H.12.1000.xts, H.12.700.xts)
H.12$lts<-H.12$thta.1-H.12$thta

L.00<-merge(L.00.1000.xts, L.00.700.xts)
L.00$lts<-L.00$thta.1-L.00$thta

L.12<-merge(L.12.1000.xts, L.12.700.xts)
L.12$lts<-L.12$thta.1-L.12$thta

#

my<-substr(index(H.00), start=1, stop=7)
H.00.agg<-aggregate(H.00, by=list(my=substr(index(H.00), start=1, stop=7)), 
                    median, na.rm=T)
H.12.agg<-aggregate(H.12, by=list(my=substr(index(H.12), start=1, stop=7)), 
                    median, na.rm=T)
L.00.agg<-aggregate(L.00, by=list(my=substr(index(L.00), start=1, stop=7)), 
                    median, na.rm=T)
L.12.agg<-aggregate(L.12, by=list(my=substr(index(L.12), start=1, stop=7)), 
                    median, na.rm=T)

H.00.yr<-aggregate(H.00.agg, by=list(my=substr(index(H.00.agg), start=1, stop=4)), 
                   mean, na.rm=T)
H.12.yr<-aggregate(H.12.agg, by=list(my=substr(index(H.12.agg), start=1, stop=4)), 
                   mean, na.rm=T)
L.00.yr<-aggregate(L.00.agg, by=list(my=substr(index(L.00.agg), start=1, stop=4)), 
                   mean, na.rm=T)
L.12.yr<-aggregate(L.12.agg, by=list(my=substr(index(L.12.agg), start=1, stop=4)), 
                   mean, na.rm=T)

write.csv(H.00.yr, "Hilo_1000_700.00z.csv", row.names=T)
write.csv(H.12.yr, "Hilo_1000_700.12z.csv", row.names=T)
write.csv(L.00.yr, "Lihue_1000_700.00z.csv", row.names=T)
write.csv(L.12.yr, "Lihue_1000_700.12z.csv", row.names=T)

LTS<-merge(H.00.yr$lts, H.12.yr$lts, L.00.yr$lts, L.12.yr$lts)[-1,]
plot(LTS[,1], col="red", lwd=2, ylim=c(13,17.5))
lines(LTS[,2], col="blue", lwd=2)
lines(LTS[,3], col="red", lty=2, lwd=2)
lines(LTS[,4], col="blue", lty=2, lwd=2)

## Hilo 2pm soundings: 0-50, 0-100, 0-800, 0-1600
st.nd<-seq(0, 4000, 400)
H.00Z<-L.00Z<-list()
H.12Z<-L.12Z<-list()

for (i in 1:(length(st.nd)-1)){
  H.00Z[[i]]<-subset(H, tm.utc=="00Z"& hght>st.nd[i] & hght<=st.nd[i+1])
  L.00Z[[i]]<-subset(L, tm.utc=="00Z"& hght>st.nd[i] & hght<=st.nd[i+1])
  H.12Z[[i]]<-subset(H, tm.utc=="12Z"& hght>st.nd[i] & hght<=st.nd[i+1])
  L.12Z[[i]]<-subset(L, tm.utc=="12Z"& hght>st.nd[i] & hght<=st.nd[i+1])
}

## Make into xts objects for reducing and plotting
H.00.xts<-L.00.xts<-list()
H.12.xts<-L.12.xts<-list()
lab<-paste0("z", st.nd[-length(st.nd)], ".", st.nd[-1])

for (i in 1:length(lab)){
  H.00.xts[[i]]<-xts(H.00Z[[i]][,-c(1:3,15)], order.by=as.Date(H.00Z[[i]]$dt))
  H.12.xts[[i]]<-xts(H.12Z[[i]][,-c(1:3,15)], order.by=as.Date(H.12Z[[i]]$dt))
  L.00.xts[[i]]<-xts(L.00Z[[i]][,-c(1:3,15)], order.by=as.Date(L.00Z[[i]]$dt))
  L.12.xts[[i]]<-xts(L.12Z[[i]][,-c(1:3,15)], order.by=as.Date(L.12Z[[i]]$dt))
}


## Bin to monthly
H.00.mo.temp<-H.00.mo.relh<-H.00.mo.mixr<-H.00.mo.sknt<-list()
L.00.mo.temp<-L.00.mo.relh<-L.00.mo.mixr<-L.00.mo.sknt<-list()
H.12.mo.temp<-H.12.mo.relh<-H.12.mo.mixr<-H.12.mo.sknt<-list()
L.12.mo.temp<-L.12.mo.relh<-L.12.mo.mixr<-L.12.mo.sknt<-list()

for (i in 1:length(lab)){
  H.00.mo.temp[[i]]<-apply.monthly(H.00.xts[[i]]$temp, median, na.rm=T)
  H.00.mo.relh[[i]]<-apply.monthly(H.00.xts[[i]]$relh, median, na.rm=T)
  H.00.mo.mixr[[i]]<-apply.monthly(H.00.xts[[i]]$mixr, median, na.rm=T)
  H.00.mo.sknt[[i]]<-apply.monthly(H.00.xts[[i]]$sknt, median, na.rm=T)*0.5144
  
  L.00.mo.temp[[i]]<-apply.monthly(L.00.xts[[i]]$temp, median, na.rm=T)
  L.00.mo.relh[[i]]<-apply.monthly(L.00.xts[[i]]$relh, median, na.rm=T)
  L.00.mo.mixr[[i]]<-apply.monthly(L.00.xts[[i]]$mixr, median, na.rm=T)
  L.00.mo.sknt[[i]]<-apply.monthly(L.00.xts[[i]]$sknt, median, na.rm=T)*0.5144
  
  H.12.mo.temp[[i]]<-apply.monthly(H.12.xts[[i]]$temp, median, na.rm=T)
  H.12.mo.relh[[i]]<-apply.monthly(H.12.xts[[i]]$relh, median, na.rm=T)
  H.12.mo.mixr[[i]]<-apply.monthly(H.12.xts[[i]]$mixr, median, na.rm=T)
  H.12.mo.sknt[[i]]<-apply.monthly(H.12.xts[[i]]$sknt, median, na.rm=T)*0.5144
  
  L.12.mo.temp[[i]]<-apply.monthly(L.12.xts[[i]]$temp, median, na.rm=T)
  L.12.mo.relh[[i]]<-apply.monthly(L.12.xts[[i]]$relh, median, na.rm=T)
  L.12.mo.mixr[[i]]<-apply.monthly(L.12.xts[[i]]$mixr, median, na.rm=T)
  L.12.mo.sknt[[i]]<-apply.monthly(L.12.xts[[i]]$sknt, median, na.rm=T)*0.5144
  
}

## Bin to annual
H.00.yr.temp<-H.00.yr.relh<-H.00.yr.mixr<-H.00.yr.sknt<-list()
L.00.yr.temp<-L.00.yr.relh<-L.00.yr.mixr<-L.00.yr.sknt<-list()
H.12.yr.temp<-H.12.yr.relh<-H.12.yr.mixr<-H.12.yr.sknt<-list()
L.12.yr.temp<-L.12.yr.relh<-L.12.yr.mixr<-L.12.yr.sknt<-list()

idx<-as.Date(paste0(1972:2018, "-12", "-31"))

for (i in 1:length(lab)){
  H.00.yr.temp[[i]]<-apply.yearly(H.00.mo.temp[[i]], mean, na.rm=T)
  H.00.yr.relh[[i]]<-apply.yearly(H.00.mo.relh[[i]], mean, na.rm=T)
  H.00.yr.mixr[[i]]<-apply.yearly(H.00.mo.mixr[[i]], mean, na.rm=T)
  H.00.yr.sknt[[i]]<-apply.yearly(H.00.mo.sknt[[i]], mean, na.rm=T)
  
  L.00.yr.temp[[i]]<-apply.yearly(L.00.mo.temp[[i]], mean, na.rm=T)
  L.00.yr.relh[[i]]<-apply.yearly(L.00.mo.relh[[i]], mean, na.rm=T)
  L.00.yr.mixr[[i]]<-apply.yearly(L.00.mo.mixr[[i]], mean, na.rm=T)
  L.00.yr.sknt[[i]]<-apply.yearly(L.00.mo.sknt[[i]], mean, na.rm=T)
  
  H.12.yr.temp[[i]]<-apply.yearly(H.12.mo.temp[[i]], mean, na.rm=T)
  H.12.yr.relh[[i]]<-apply.yearly(H.12.mo.relh[[i]], mean, na.rm=T)
  H.12.yr.mixr[[i]]<-apply.yearly(H.12.mo.mixr[[i]], mean, na.rm=T)
  H.12.yr.sknt[[i]]<-apply.yearly(H.12.mo.sknt[[i]], mean, na.rm=T)
  
  L.12.yr.temp[[i]]<-apply.yearly(L.12.mo.temp[[i]], mean, na.rm=T)
  L.12.yr.relh[[i]]<-apply.yearly(L.12.mo.relh[[i]], mean, na.rm=T)
  L.12.yr.mixr[[i]]<-apply.yearly(L.12.mo.mixr[[i]], mean, na.rm=T)
  L.12.yr.sknt[[i]]<-apply.yearly(L.12.mo.sknt[[i]], mean, na.rm=T)
  
  index(H.00.yr.temp[[i]])<-index(H.00.yr.relh[[i]])<-index(H.00.yr.mixr[[i]])<-index(H.00.yr.sknt[[i]])<-idx
  index(L.00.yr.temp[[i]])<-index(L.00.yr.relh[[i]])<-index(L.00.yr.mixr[[i]])<-index(L.00.yr.sknt[[i]])<-idx
  index(H.12.yr.temp[[i]])<-index(H.12.yr.relh[[i]])<-index(H.12.yr.mixr[[i]])<-index(H.12.yr.sknt[[i]])<-idx[-1]
  index(L.12.yr.temp[[i]])<-index(L.12.yr.relh[[i]])<-index(L.12.yr.mixr[[i]])<-index(L.12.yr.sknt[[i]])<-idx[-1]
  
}

## Combine and visualize
H.00.temp.all<- H.00.yr.temp[[1]]
H.00.relh.all<- H.00.yr.relh[[1]]
H.00.mixr.all<- H.00.yr.mixr[[1]]
H.00.sknt.all<- H.00.yr.sknt[[1]]

L.00.temp.all<- L.00.yr.temp[[1]]
L.00.relh.all<- L.00.yr.relh[[1]]
L.00.mixr.all<- L.00.yr.mixr[[1]]
L.00.sknt.all<- L.00.yr.sknt[[1]]

H.12.temp.all<- H.12.yr.temp[[1]]
H.12.relh.all<- H.12.yr.relh[[1]]
H.12.mixr.all<- H.12.yr.mixr[[1]]
H.12.sknt.all<- H.12.yr.sknt[[1]]

L.12.temp.all<- L.12.yr.temp[[1]]
L.12.relh.all<- L.12.yr.relh[[1]]
L.12.mixr.all<- L.12.yr.mixr[[1]]
L.12.sknt.all<- L.12.yr.sknt[[1]]

for (i in 2:length(lab)){
  H.00.temp.all<-merge(H.00.temp.all, H.00.yr.temp[[i]], all=T)
  H.00.relh.all<-merge(H.00.relh.all, H.00.yr.relh[[i]], all=T)
  H.00.mixr.all<-merge(H.00.mixr.all, H.00.yr.mixr[[i]], all=T)
  H.00.sknt.all<-merge(H.00.sknt.all, H.00.yr.sknt[[i]], all=T)
  
  L.00.temp.all<-merge(L.00.temp.all, L.00.yr.temp[[i]], all=T)
  L.00.relh.all<-merge(L.00.relh.all, L.00.yr.relh[[i]], all=T)
  L.00.mixr.all<-merge(L.00.mixr.all, L.00.yr.mixr[[i]], all=T)
  L.00.sknt.all<-merge(L.00.sknt.all, L.00.yr.sknt[[i]], all=T)
  
  H.12.temp.all<-merge(H.12.temp.all, H.12.yr.temp[[i]], all=T)
  H.12.relh.all<-merge(H.12.relh.all, H.12.yr.relh[[i]], all=T)
  H.12.mixr.all<-merge(H.12.mixr.all, H.12.yr.mixr[[i]], all=T)
  H.12.sknt.all<-merge(H.12.sknt.all, H.12.yr.sknt[[i]], all=T)
  
  L.12.temp.all<-merge(L.12.temp.all, L.12.yr.temp[[i]], all=T)
  L.12.relh.all<-merge(L.12.relh.all, L.12.yr.relh[[i]], all=T)
  L.12.mixr.all<-merge(L.12.mixr.all, L.12.yr.mixr[[i]], all=T)
  L.12.sknt.all<-merge(L.12.sknt.all, L.12.yr.sknt[[i]], all=T)
}


## Daytime
par(mfrow=c(2,4))
plot(H.00.temp.all[-1,])
plot(L.00.temp.all[-1,])

plot(H.00.relh.all[-1,])
plot(L.00.relh.all[-1,])

plot(H.00.mixr.all[-1,])
plot(L.00.mixr.all[-1,])

plot(H.00.sknt.all[-1,])
plot(L.00.sknt.all[-1,])

### Nighttime
plot(H.12.temp.all)
plot(L.12.temp.all)

plot(H.12.relh.all)
plot(L.12.relh.all)

plot(H.12.mixr.all)
plot(L.12.mixr.all)

plot(H.12.sknt.all)
plot(L.12.sknt.all)

## Calculate trends (1978-2017)
yrs<-1978:2017
H.00.temp.80<-data.frame(H.00.temp.all["1978-12-31/2017-12-31"])
H.00.relh.80<-data.frame(H.00.relh.all["1978-12-31/2017-12-31"])
H.00.mixr.80<-data.frame(H.00.mixr.all["1978-12-31/2017-12-31"])
H.00.sknt.80<-data.frame(H.00.sknt.all["1978-12-31/2017-12-31"])

L.00.temp.80<-data.frame(L.00.temp.all["1978-12-31/2017-12-31"])
L.00.relh.80<-data.frame(L.00.relh.all["1978-12-31/2017-12-31"])
L.00.mixr.80<-data.frame(L.00.mixr.all["1978-12-31/2017-12-31"])
L.00.sknt.80<-data.frame(L.00.sknt.all["1978-12-31/2017-12-31"])

H.12.temp.80<-data.frame(H.12.temp.all["1978-12-31/2017-12-31"])
H.12.relh.80<-data.frame(H.12.relh.all["1978-12-31/2017-12-31"])
H.12.mixr.80<-data.frame(H.12.mixr.all["1978-12-31/2017-12-31"])
H.12.sknt.80<-data.frame(H.12.sknt.all["1978-12-31/2017-12-31"])

L.12.temp.80<-data.frame(L.12.temp.all["1978-12-31/2017-12-31"])
L.12.relh.80<-data.frame(L.12.relh.all["1978-12-31/2017-12-31"])
L.12.mixr.80<-data.frame(L.12.mixr.all["1978-12-31/2017-12-31"])
L.12.sknt.80<-data.frame(L.12.sknt.all["1978-12-31/2017-12-31"])


names(H.00.temp.80)<-names(H.00.relh.80)<-names(H.00.mixr.80)<-names(H.00.sknt.80)<-lab
names(L.00.temp.80)<-names(L.00.relh.80)<-names(L.00.mixr.80)<-names(L.00.sknt.80)<-lab
names(H.12.temp.80)<-names(H.12.relh.80)<-names(H.12.mixr.80)<-names(H.12.sknt.80)<-lab
names(L.12.temp.80)<-names(L.12.relh.80)<-names(L.12.mixr.80)<-names(L.12.sknt.80)<-lab

H.00.temp.80$year<-H.00.relh.80$year<-H.00.mixr.80$year<-H.00.sknt.80$year<-yrs
L.00.temp.80$year<-L.00.relh.80$year<-L.00.mixr.80$year<-L.00.sknt.80$year<-yrs
H.12.temp.80$year<-H.12.relh.80$year<-H.12.mixr.80$year<-H.12.sknt.80$year<-yrs
L.12.temp.80$year<-L.12.relh.80$year<-L.12.mixr.80$year<-L.12.sknt.80$year<-yrs

mrg.Ht<-merge(H.00.temp.80, H.12.temp.80, by="year", suffixes = c(".H00",".H12"))
mrg.Lt<-merge(L.00.temp.80, L.12.temp.80, by="year", suffixes = c(".L00",".L12"))
mrg.temp<-merge(mrg.Ht, mrg.Lt, by="year")

mrg.Hr<-merge(H.00.relh.80, H.12.relh.80, by="year", suffixes = c(".H00",".H12"))
mrg.Lr<-merge(L.00.relh.80, L.12.relh.80, by="year", suffixes = c(".L00",".L12"))
mrg.relh<-merge(mrg.Hr, mrg.Lr, by="year")

mrg.Hm<-merge(H.00.mixr.80, H.12.mixr.80, by="year", suffixes = c(".H00",".H12"))
mrg.Lm<-merge(L.00.mixr.80, L.12.mixr.80, by="year", suffixes = c(".L00",".L12"))
mrg.mixr<-merge(mrg.Hm, mrg.Lm, by="year")

mrg.Hs<-merge(H.00.sknt.80, H.12.sknt.80, by="year", suffixes = c(".H00",".H12"))
mrg.Ls<-merge(L.00.sknt.80, L.12.sknt.80, by="year", suffixes = c(".L00",".L12"))
mrg.sknt<-merge(mrg.Hs, mrg.Ls, by="year")

write.csv(mrg.temp, "temp.profileTS.csv", row.names=F)
write.csv(mrg.relh, "relh.profileTS.csv", row.names=F)
write.csv(mrg.mixr, "mixr.profileTS.csv", row.names=F)
write.csv(mrg.sknt, "sknt.profileTS.csv", row.names=F)

library(Kendall)
library(zyp)

H00t<-H00r<-H00m<-H00s<-matrix(nrow=length(lab), ncol=3)
L00t<-L00r<-L00m<-L00s<-matrix(nrow=length(lab), ncol=3)
H12t<-H12r<-H12m<-H12s<-matrix(nrow=length(lab), ncol=3)
L12t<-L12r<-L12m<-L12s<-matrix(nrow=length(lab), ncol=3)

for (i in 1:length(lab)){
  yH00t<-as.numeric(H.00.temp.80[,i])
  yH00r<-as.numeric(H.00.relh.80[,i])
  yH00m<-as.numeric(H.00.mixr.80[,i])
  yH00s<-as.numeric(H.00.sknt.80[,i])
  
  yL00t<-as.numeric(L.00.temp.80[,i])
  yL00r<-as.numeric(L.00.relh.80[,i])
  yL00m<-as.numeric(L.00.mixr.80[,i])
  yL00s<-as.numeric(L.00.sknt.80[,i])
  
  yH12t<-as.numeric(H.12.temp.80[,i])
  yH12r<-as.numeric(H.12.relh.80[,i])
  yH12m<-as.numeric(H.12.mixr.80[,i])
  yH12s<-as.numeric(H.12.sknt.80[,i])
  
  yL12t<-as.numeric(L.12.temp.80[,i])
  yL12r<-as.numeric(L.12.relh.80[,i])
  yL12m<-as.numeric(L.12.mixr.80[,i])
  yL12s<-as.numeric(L.12.sknt.80[,i])
  
  mod.H00t<-zyp.sen(yH00t~yrs)
  mod.H00r<-zyp.sen(yH00r~yrs)
  mod.H00m<-zyp.sen(yH00m~yrs)
  mod.H00s<-zyp.sen(yH00s~yrs)
  
  mod.L00t<-zyp.sen(yL00t~yrs)
  mod.L00r<-zyp.sen(yL00r~yrs)
  mod.L00m<-zyp.sen(yL00m~yrs)
  mod.L00s<-zyp.sen(yL00s~yrs)
  
  mod.H12t<-zyp.sen(yH12t~yrs)
  mod.H12r<-zyp.sen(yH12r~yrs)
  mod.H12m<-zyp.sen(yH12m~yrs)
  mod.H12s<-zyp.sen(yH12s~yrs)
  
  mod.L12t<-zyp.sen(yL12t~yrs)
  mod.L12r<-zyp.sen(yL12r~yrs)
  mod.L12m<-zyp.sen(yL12m~yrs)
  mod.L12s<-zyp.sen(yL12s~yrs)
  
  H00t[i,2]<-coef(mod.H00t)[2]*10
  H00t[i,c(1,3)]<-confint(mod.H00t)[2,]*10
  H00r[i,2]<-coef(mod.H00r)[2]*10
  H00r[i,c(1,3)]<-confint(mod.H00r)[2,]*10
  H00m[i,2]<-coef(mod.H00m)[2]*10
  H00m[i,c(1,3)]<-confint(mod.H00m)[2,]*10
  H00s[i,2]<-coef(mod.H00s)[2]*10
  H00s[i,c(1,3)]<-confint(mod.H00s)[2,]*10
  
  L00t[i,2]<-coef(mod.L00t)[2]*10
  L00t[i,c(1,3)]<-confint(mod.L00t)[2,]*10
  L00r[i,2]<-coef(mod.L00r)[2]*10
  L00r[i,c(1,3)]<-confint(mod.L00r)[2,]*10
  L00m[i,2]<-coef(mod.L00m)[2]*10
  L00m[i,c(1,3)]<-confint(mod.L00m)[2,]*10
  L00s[i,2]<-coef(mod.L00s)[2]*10
  L00s[i,c(1,3)]<-confint(mod.L00s)[2,]*10
  
  H12t[i,2]<-coef(mod.H12t)[2]*10
  H12t[i,c(1,3)]<-confint(mod.H12t)[2,]*10
  H12r[i,2]<-coef(mod.H12r)[2]*10
  H12r[i,c(1,3)]<-confint(mod.H12r)[2,]*10
  H12m[i,2]<-coef(mod.H12m)[2]*10
  H12m[i,c(1,3)]<-confint(mod.H12m)[2,]*10
  H12s[i,2]<-coef(mod.H12s)[2]*10
  H12s[i,c(1,3)]<-confint(mod.H12s)[2,]*10
  
  L12t[i,2]<-coef(mod.L12t)[2]*10
  L12t[i,c(1,3)]<-confint(mod.L12t)[2,]*10
  L12r[i,2]<-coef(mod.L12r)[2]*10
  L12r[i,c(1,3)]<-confint(mod.L12r)[2,]*10
  L12m[i,2]<-coef(mod.L12m)[2]*10
  L12m[i,c(1,3)]<-confint(mod.L12m)[2,]*10
  L12s[i,2]<-coef(mod.L12s)[2]*10
  L12s[i,c(1,3)]<-confint(mod.L12s)[2,]*10
  
}


H00t.mean<-apply(H.00.temp.80[,-11], 2, mean)
L00t.mean<-apply(L.00.temp.80[,-11], 2, mean)
H12t.mean<-apply(H.12.temp.80[,-11], 2, mean)
L12t.mean<-apply(L.12.temp.80[,-11], 2, mean)

H00r.mean<-apply(H.00.relh.80[,-11], 2, mean)
L00r.mean<-apply(L.00.relh.80[,-11], 2, mean)
H12r.mean<-apply(H.12.relh.80[,-11], 2, mean)
L12r.mean<-apply(L.12.relh.80[,-11], 2, mean)

H00m.mean<-apply(H.00.mixr.80[,-11], 2, mean)
L00m.mean<-apply(L.00.mixr.80[,-11], 2, mean)
H12m.mean<-apply(H.12.mixr.80[,-11], 2, mean)
L12m.mean<-apply(L.12.mixr.80[,-11], 2, mean)

H00s.mean<-apply(H.00.sknt.80[,-11], 2, mean)
L00s.mean<-apply(L.00.sknt.80[,-11], 2, mean)
H12s.mean<-apply(H.12.sknt.80[,-11], 2, mean)
L12s.mean<-apply(L.12.sknt.80[,-11], 2, mean)

pdf("Profiles.pdf", width=8)
par(mfrow=c(1,2), mar=c(7, 4, 4, 2) + 0.1)
mid<-st.nd[-1]-200

### Hilo RH
plot(mid~H00r[,2], xlim=c(-3, 5), col="red",pch=1, xlab="",
     ylab="Altitude (m)", main="Hilo RH trend/profile (1978-2017)")
arrows(H00r[,1], mid, H00r[,3], mid, col="red", length=0, lwd=2)
abline(v=0)
points(H12r[,2], mid+20, col="blue", pch=1)
arrows(H12r[,1], mid+20, H12r[,3], mid+20, col="blue", length=0, lwd=2)
mtext(expression('RH trend'~('%'*dec^-1)), side=1, line=2.5, cex=0.9)
par(new=T)
plot(H00r.mean, mid, type="l", col="red", xlim=c(0,100),
     yaxt="n", ylab="", xlab="", xaxt="n")
lines(H12r.mean, mid, type="l", col="blue")
axis(1, line=4)
mtext("RH", side=1, line=6, cex=0.9)

### Lihue RH
plot(mid~L00r[,2], xlim=c(-3, 5), col="red",pch=1, xlab="",
     ylab="Altitude (m)", main="Lihue RH trend/profile (1978-2017)")
arrows(L00r[,1], mid, L00r[,3], mid, col="red", length=0, lwd=2)
abline(v=0)
points(L12r[,2], mid+20, col="blue", pch=1)
arrows(L12r[,1], mid+20, L12r[,3], mid+20, col="blue", length=0, lwd=2)
mtext(expression('RH trend'~('%'*dec^-1)), side=1, line=2.5, cex=0.9)
par(new=T)
plot(L00r.mean, mid, type="l", col="red", xlim=c(0,100),
     yaxt="n", ylab="", xlab="", xaxt="n")
lines(L12r.mean, mid, type="l", col="blue")
axis(1, line=4)
mtext("RH", side=1, line=6, cex=0.9)

##### Mixing ratio:
### Hilo mixing ratio
par(mfrow=c(1,2), mar=c(7, 4, 4, 2) + 0.1)
plot(mid~H00m[,2], xlim=c(-0.4, 0.5), col="red",pch=1, xlab="",
     ylab="Altitude (m)", main="Hilo MR trend/profile (1978-2017)")
arrows(H00m[,1], mid, H00m[,3], mid, col="red", length=0, lwd=2)
abline(v=0)
points(H12m[,2], mid+20, col="blue", pch=1)
arrows(H12m[,1], mid+20, H12m[,3], mid+20, col="blue", length=0, lwd=2)
mtext(expression('MR trend'~(dec^-1)), side=1, line=2.5, cex=0.9)
par(new=T)
plot(H00m.mean, mid, type="l", col="red", xlim=c(0,15),
     yaxt="n", ylab="", xlab="", xaxt="n")
lines(H12m.mean, mid, type="l", col="blue")
axis(1, line=4)
mtext("Mixing Ratio", side=1, line=6, cex=0.9)

### Lihue Mixing ratio
plot(mid~L00m[,2], xlim=c(-0.4, 0.5), col="red",pch=1, xlab="",
     ylab="Altitude (m)", main="Lihue MR trend/profile (1978-2017)")
arrows(L00m[,1], mid, L00m[,3], mid, col="red", length=0, lwd=2)
abline(v=0)
points(L12m[,2], mid+20, col="blue", pch=1)
arrows(L12m[,1], mid+20, L12m[,3], mid+20, col="blue", length=0, lwd=2)
mtext(expression('MR trend'~(dec^-1)), side=1, line=2.5, cex=0.9)
par(new=T)
plot(L00m.mean, mid, type="l", col="red", xlim=c(0,15),
     yaxt="n", ylab="", xlab="", xaxt="n")
lines(L12m.mean, mid, type="l", col="blue")
axis(1, line=4)
mtext("Mixing Ratio", side=1, line=6, cex=0.9)

##### Temperature:
### Hilo temperature
par(mfrow=c(1,2), mar=c(7, 4, 4, 2) + 0.1)
plot(mid~H00t[,2], xlim=c(-0.25, 0.45), col="red",pch=1, xlab="",
     ylab="Altitude (m)", main="Hilo Temp trend/profile (1978-2017)")
arrows(H00t[,1], mid, H00t[,3], mid, col="red", length=0, lwd=2)
abline(v=0)
points(H12t[,2], mid+20, col="blue", pch=1)
arrows(H12t[,1], mid+20, H12t[,3], mid+20, col="blue", length=0, lwd=2)
mtext(expression('Temp trend'~(degree*C~dec^-1)), side=1, line=2.5, cex=0.9)
par(new=T)
plot(H00t.mean, mid, type="l", col="red", xlim=c(0,25),
     yaxt="n", ylab="", xlab="", xaxt="n")
lines(H12t.mean, mid, type="l", col="blue")
axis(1, line=4)
mtext(expression("Temperature"~(degree*C)), side=1, line=6, cex=0.9)

### Lihue temperature
plot(mid~L00t[,2], xlim=c(-0.25, 0.45), col="red",pch=1, xlab="",
     ylab="Altitude (m)", main="Lihue Temp trend/profile (1978-2017)")
arrows(L00t[,1], mid, L00t[,3], mid, col="red", length=0, lwd=2)
abline(v=0)
points(L12t[,2], mid+20, col="blue", pch=1)
arrows(L12t[,1], mid+20, L12t[,3], mid+20, col="blue", length=0, lwd=2)
mtext(expression('Temp trend'~(degree*C~dec^-1)), side=1, line=2.5, cex=0.9)
par(new=T)
plot(L00t.mean, mid, type="l", col="red", xlim=c(0,25),
     yaxt="n", ylab="", xlab="", xaxt="n")
lines(L12t.mean, mid, type="l", col="blue")
axis(1, line=4)
mtext(expression("Temperature"~(degree*C)), side=1, line=6, cex=0.9)

##### Wind speed:
range(H00s[,1:3], H12s[,1:3], L00s[,1:3], L12s[,1:3])
range(H00s.mean, H12s.mean, L00s.mean, L12s.mean)

### Hilo wind speed (knots; conversion=0.5144 m/s to 1 knot)
par(mfrow=c(1,2), mar=c(7, 4, 4, 2) + 0.1)
plot(mid~H00s[,2], xlim=c(-0.65, 0.4), col="red",pch=1, xlab="",
     ylab="Altitude (m)", main="Hilo WS trend/profile (1978-2017)")
arrows(H00s[,1], mid, H00s[,3], mid, col="red", length=0, lwd=2)
abline(v=0)
points(H12s[,2], mid+20, col="blue", pch=1)
arrows(H12s[,1], mid+20, H12s[,3], mid+20, col="blue", length=0, lwd=2)
mtext(expression('WS trend'~(knots~dec^-1)), side=1, line=2.5, cex=0.9)
par(new=T)
plot(H00s.mean, mid, type="l", col="red", xlim=c(5,16),
     yaxt="n", ylab="", xlab="", xaxt="n")
lines(H12s.mean, mid, type="l", col="blue")
axis(1, line=4)
mtext(expression("WS"~(knots)), side=1, line=6, cex=0.9)

### Lihue wind speed
plot(mid~L00s[,2], xlim=c(-0.65, 0.4), col="red",pch=1, xlab="",
     ylab="Altitude (m)", main="Lihue WS trend/profile (1978-2017)")
arrows(L00s[,1], mid, L00s[,3], mid, col="red", length=0, lwd=2)
abline(v=0)
points(L12s[,2], mid+20, col="blue", pch=1)
arrows(L12s[,1], mid+20, L12s[,3], mid+20, col="blue", length=0, lwd=2)
mtext(expression('WS trend'~(knots~dec^-1)), side=1, line=2.5, cex=0.9)
par(new=T)
plot(L00s.mean, mid, type="l", col="red", xlim=c(5,16),
     yaxt="n", ylab="", xlab="", xaxt="n")
lines(L12s.mean, mid, type="l", col="blue")
axis(1, line=4)
mtext(expression("WS"~(knots)), side=1, line=6, cex=0.9)
dev.off()

### combine files and export
temp.80.17<-data.frame(H00t, H12t, L00t, L12t, 
                       H00t.mean, H12t.mean, 
                       L00t.mean, L12t.mean)
relh.80.17<-data.frame(H00r, H12r, L00r, L12r, 
                       H00r.mean, H12r.mean, 
                       L00r.mean, L12r.mean)
mixr.80.17<-data.frame(H00m, H12m, L00m, L12m, 
                       H00m.mean, H12m.mean, 
                       L00m.mean, L12m.mean)
sknt.80.17<-data.frame(H00s, H12s, L00s, L12s, 
                       H00s.mean, H12s.mean, 
                       L00s.mean, L12s.mean)

names(temp.80.17)[1:12]<- names(relh.80.17)[1:12]<-
  names(mixr.80.17)[1:12]<-names(sknt.80.17)[1:12]<-
  paste0(rep(c("H00", "H12", "L00", "L12"), each=3), c("_lo", "", "_up"))

row.names(temp.80.17)<-row.names(relh.80.17)<-
  row.names(mixr.80.17)<-row.names(sknt.80.17)<-lab

write.csv(temp.80.17, "temp.trends2.csv", row.names=T)
write.csv(relh.80.17, "relh.trends2.csv", row.names=T)
write.csv(mixr.80.17, "mixr.trends2.csv", row.names=T)
write.csv(sknt.80.17, "sknt.trends2.csv", row.names=T)





