##########################################################
## Profile_Trends; accompanies LapseRateFigure2.R
## Project: HI Temp
## Author: Aurora Kagawa-Viviani
## Date: 6/16/2019
## Notes: Script for looking at lapse rate trends:
#      - breakpoint analysis
#      - are trends driven by certain periods?
#   Bring in sounding index data, ceilometer, look at trends
#   Bring in sounding profile data, look at trends


setwd("xxx/TempMapping")

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

# Gridded stuff
index<-read.csv("WindCloud/MonthlyTimeSeries_2017.csv", stringsAsFactors = F) # new master time series

# Clean up various time series
## Merge mei and meiX (is this legit?) since MEI starts 1950
meiNAl<-(is.na(index[,"indices.mei"])) # which MEI are NA? replace with MEIX
index$MEI<-c(index[meiNAl,"indices.meiX"], index[!meiNAl,"indices.mei"])

## Log-transform RFI
index$lnrfi<-log(index[,"indices.rfi"])

## Square root transform cloud cover
index$sqtcc<-sqrt(index[,"windcloud.tcc"])
index$sqlcc<-sqrt(index[,"windcloud.lcc"])
index$sqmcc<-sqrt(index[,"windcloud.mcc"])
index$sqhcc<-sqrt(index[,"windcloud.hcc"])
index$sqacsh<-sqrt(index[,"HNLcw.ACSH.pct"]/100)
index$sqacmh<-sqrt(index[,"HNLcw.ACMH.pct"]/100)

## Convert wind to m/s
index$awnd<-index[,"HNLcw.AWND.tenthms"]/10

indexy<-aggregate(index[-c(2:5)], by=list(Year=index$YEAR), mean)

# Sounding layers information 400m slices
rh<-read.csv("NOT_USED/AKV_Sonde/UWyo/relh.profileTS.csv", stringsAsFactors = F)
temp<-read.csv("NOT_USED/AKV_Sonde/UWyo/temp.profileTS.csv", stringsAsFactors = F)
mixr<-read.csv("NOT_USED/AKV_Sonde/UWyo/mixr.profileTS.csv", stringsAsFactors = F)
sknt<-read.csv("NOT_USED/AKV_Sonde/UWyo/sknt.profileTS.csv", stringsAsFactors = F)


###################################################################################
###### Trends over 1978-2017: ###### 
Tdat40<-subset(Tdat, Tdat$Year>=1978 & Tdat$Year<=2017 )

rh40<-subset(rh, rh$year>=1978)
temp40<-subset(temp, temp$year>=1978)
mixr40<-subset(mixr, mixr$year>=1978)
sknt40<-subset(sknt, sknt$year>=1978)

cphto40<-subset(cphto, cphto$Year>=1978 & cphto$Year<=2017 )
cphli40<-subset(cphli, cphli$Year>=1978 & cphli$Year<=2017 )

sHilo40<-subset(sHilo, sHilo$Year>=1978 & sHilo$Year<=2017 )
sLih40<-subset(sLih, sLih$Year>=1978 & sLih$Year<=2017 )

indices40<-subset(indexy, indexy$Year>=1978 & indexy$YEAR<=2017 )

## Surface temperatures
yr<-Tdat40$Year
Tdat.sen40<-ci.Tdat.sen40<-list()
cphli.sen40<-ci.cphli.sen40<-list()

for (i in 1:(ncol(Tdat40)-1)){
  yH<-Tdat40[,i]
  Tdat.sen40[[i]]<-zyp.sen(yH~yr)
  ci.Tdat.sen40[[i]]<-confint(Tdat.sen40[[i]]) 
}


trend.Tdat.40<-data.frame(trend=unlist(lapply(Tdat.sen40, function(x) coef(x)["yr"])),
                           CI=matrix(unlist(lapply(ci.Tdat.sen40, function(x) x["yr",])), 
                                     ncol=2, byrow=T),
                           metric=names(Tdat40)[-ncol(Tdat40)],
                           start="1978", end="2017")

## ceilometers
yr<-cphto40$Year
cphto.sen40<-ci.cphto.sen40<-list()
cphli.sen40<-ci.cphli.sen40<-list()

for (i in 1:(ncol(cphto40)-1)){
  yH<-cphto40[,i]
  cphto.sen40[[i]]<-zyp.sen(yH~yr)
  ci.cphto.sen40[[i]]<-confint(cphto.sen40[[i]]) 
  
  yL<-cphli40[,i]
  cphli.sen40[[i]]<-zyp.sen(yL~yr)
  ci.cphli.sen40[[i]]<-confint(cphli.sen40[[i]]) 
  
}


trend.cphto.40<-data.frame(trend=unlist(lapply(cphto.sen40, function(x) coef(x)["yr"])),
                     CI=matrix(unlist(lapply(ci.cphto.sen40, function(x) x["yr",])), 
                               ncol=2, byrow=T),
                     metric=names(cphto40)[-ncol(cphto40)],
                     start="1978", end="2017")
trend.cphli.40<-data.frame(trend=unlist(lapply(cphli.sen40, function(x) coef(x)["yr"])),
                           CI=matrix(unlist(lapply(ci.cphli.sen40, function(x) x["yr",])), 
                                     ncol=2, byrow=T),
                           metric=names(cphli40)[-ncol(cphli40)],
                           start="1978", end="2017")

## sounding
yr<-sHilo40$Year
sHilo.sen40<-ci.sHilo.sen40<-list()
sLih.sen40<-ci.sLih.sen40<-list()

for (i in 1:(ncol(sHilo40)-1)){
  yH<-sHilo40[,i]
  if(!any(is.na(yH))){
    sHilo.sen40[[i]]<-zyp.sen(yH~yr)
    ci.sHilo.sen40[[i]]<-confint(sHilo.sen40[[i]]) 
    names(sHilo.sen40)[i]<-names(ci.sHilo.sen40)[i]<-names(sHilo40)[i]
  }
  
  yL<-sLih40[,i]
  if(!any(is.na(yL))){
    sLih.sen40[[i]]<-zyp.sen(yL~yr)
    ci.sLih.sen40[[i]]<-confint(sLih.sen40[[i]]) 
    names(sLih.sen40)[i]<-names(ci.sLih.sen40)[i]<-names(sLih40)[i]
  }
}


trend.sHilo.40<-data.frame(trend=unlist(lapply(sHilo.sen40, function(x) coef(x)["yr"])),
                           CI=matrix(unlist(lapply(ci.sHilo.sen40, function(x) x["yr",])), 
                                     ncol=2, byrow=T),
                           metric=paste0("Hilo_", names(sHilo40)[-ncol(sHilo40)]),
                           start="1978", end="2017")
trend.sLih.40<-data.frame(trend=unlist(lapply(sLih.sen40, function(x) coef(x)["yr"])),
                           CI=matrix(unlist(lapply(ci.sLih.sen40, function(x) x["yr",])), 
                                     ncol=2, byrow=T),
                           metric=paste0("Lih_", names(sLih40)[-ncol(sLih40)]),
                           start="1978", end="2017")


## sounding PROFILES
yr<-rh40$year
rh.sen40<-ci.rh.sen40<-list()
temp.sen40<-ci.temp.sen40<-list()
mixr.sen40<-ci.mixr.sen40<-list()
sknt.sen40<-ci.sknt.sen40<-list()

for (i in 1:(ncol(rh40)-1)){
  yrh<-rh40[,i+1]
  if(!any(is.na(yrh))){
    rh.sen40[[i]]<-zyp.sen(yrh~yr)
    ci.rh.sen40[[i]]<-confint(rh.sen40[[i]]) 
  }
  
  yt<-temp40[,i+1]
  if(!any(is.na(yt))){
    temp.sen40[[i]]<-zyp.sen(yt~yr)
    ci.temp.sen40[[i]]<-confint(temp.sen40[[i]]) 
  }
  
  ym<-mixr40[,i+1]
  if(!any(is.na(ym))){
    mixr.sen40[[i]]<-zyp.sen(ym~yr)
    ci.mixr.sen40[[i]]<-confint(mixr.sen40[[i]]) 
  }
  
  ys<-sknt40[,i+1]
  if(!any(is.na(ys))){
    sknt.sen40[[i]]<-zyp.sen(ys~yr)
    ci.sknt.sen40[[i]]<-confint(sknt.sen40[[i]]) 
  }
}


trend.rh.40<-data.frame(trend=unlist(lapply(rh.sen40, function(x) coef(x)["yr"])),
                           CI=matrix(unlist(lapply(ci.rh.sen40, function(x) x["yr",])), 
                                     ncol=2, byrow=T),
                           metric=paste0("rh_",names(rh40)[-1]),
                           start="1978", end="2017")
trend.temp.40<-data.frame(trend=unlist(lapply(temp.sen40, function(x) coef(x)["yr"])),
                          CI=matrix(unlist(lapply(ci.temp.sen40, function(x) x["yr",])), 
                                    ncol=2, byrow=T),
                         metric=paste0("temp_",names(temp40)[-1]),
                          start="1978", end="2017")

trend.mixr.40<-data.frame(trend=unlist(lapply(mixr.sen40, function(x) coef(x)["yr"])),
                        CI=matrix(unlist(lapply(ci.mixr.sen40, function(x) x["yr",])), 
                                  ncol=2, byrow=T),
                        metric=paste0("mixr_",names(mixr40)[-1]),
                        start="1978", end="2017")
trend.sknt.40<-data.frame(trend=unlist(lapply(sknt.sen40, function(x) coef(x)["yr"])),
                          CI=matrix(unlist(lapply(ci.sknt.sen40, function(x) x["yr",])), 
                                    ncol=2, byrow=T),
                          metric=paste0("ws_",names(sknt40)[-1]),
                          start="1978", end="2017")

yr<-indices40$Year
i.sen40<-ci.i.sen40<-list()
st.nd<-matrix(0, ncol=2, nrow=ncol(indices40)-2)
  
for (i in 1:(ncol(indices40)-2)){
  yi<-indices40[,i+2]
    i.sen40[[i]]<-zyp.sen(yi~yr)
    ci.i.sen40[[i]]<-confint(i.sen40[[i]]) 
    st.nd[i,]<-range(indices40$Year[!is.na(indices40[,i+2])])
}

trend.i.40<-data.frame(trend=unlist(lapply(i.sen40, function(x) coef(x)["yr"])),
                        CI=matrix(unlist(lapply(ci.i.sen40, function(x) x["yr",])), 
                                  ncol=2, byrow=T),
                        metric=names(indices40)[-c(1:2)],
                        start=as.character(st.nd[,1]), end=as.character(st.nd[,2]))


trend.40<-rbind(trend.Tdat.40, trend.temp.40, trend.rh.40, trend.mixr.40, trend.sknt.40,
                trend.cphto.40, trend.cphli.40, trend.i.40, trend.sHilo.40, trend.sLih.40)

names(trend.40)[2:3]<-c("CI.025", "CI.975")

###################################################################################
###### Trends over 1998-2017: ###### 
Tdat20<-subset(Tdat, Tdat$Year>=1998 & Tdat$Year<=2017 )

rh20<-subset(rh, rh$year>=1998)
temp20<-subset(temp, temp$year>=1998)
mixr20<-subset(mixr, mixr$year>=1998)
sknt20<-subset(sknt, sknt$year>=1998)

cphto20<-subset(cphto, cphto$Year>=1998 & cphto$Year<=2017 )
cphli20<-subset(cphli, cphli$Year>=1998 & cphli$Year<=2017 )

sHilo20<-subset(sHilo, sHilo$Year>=1998 & sHilo$Year<=2017 )
sLih20<-subset(sLih, sLih$Year>=1998 & sLih$Year<=2017 )

indices20<-subset(indexy, indexy$Year>=1998 & indexy$YEAR<=2017 )
om<-grep(c("ac"), names(indices20), ignore.case=T)
indices20<-indices20[,-om]

## Surface temperatures
yr<-Tdat20$Year
Tdat.sen20<-ci.Tdat.sen20<-list()
cphli.sen20<-ci.cphli.sen20<-list()

for (i in 1:(ncol(Tdat20)-1)){
  yH<-Tdat20[,i]
  Tdat.sen20[[i]]<-zyp.sen(yH~yr)
  ci.Tdat.sen20[[i]]<-confint(Tdat.sen20[[i]]) 
}


trend.Tdat.20<-data.frame(trend=unlist(lapply(Tdat.sen20, function(x) coef(x)["yr"])),
                          CI=matrix(unlist(lapply(ci.Tdat.sen20, function(x) x["yr",])), 
                                    ncol=2, byrow=T),
                          metric=names(Tdat20)[-ncol(Tdat20)],
                          start="1998", end="2017")

## ceilometers
yr<-cphto20$Year
cphto.sen20<-ci.cphto.sen20<-list()
cphli.sen20<-ci.cphli.sen20<-list()

for (i in 1:(ncol(cphto20)-1)){
  yH<-cphto20[,i]
  cphto.sen20[[i]]<-zyp.sen(yH~yr)
  ci.cphto.sen20[[i]]<-confint(cphto.sen20[[i]]) 
  
  yL<-cphli20[,i]
  cphli.sen20[[i]]<-zyp.sen(yL~yr)
  ci.cphli.sen20[[i]]<-confint(cphli.sen20[[i]]) 
  
}


trend.cphto.20<-data.frame(trend=unlist(lapply(cphto.sen20, function(x) coef(x)["yr"])),
                           CI=matrix(unlist(lapply(ci.cphto.sen20, function(x) x["yr",])), 
                                     ncol=2, byrow=T),
                           metric=names(cphto20)[-ncol(cphto20)],
                           start="1998", end="2017")
trend.cphli.20<-data.frame(trend=unlist(lapply(cphli.sen20, function(x) coef(x)["yr"])),
                           CI=matrix(unlist(lapply(ci.cphli.sen20, function(x) x["yr",])), 
                                     ncol=2, byrow=T),
                           metric=names(cphli20)[-ncol(cphli20)],
                           start="1998", end="2017")

## sounding
yr<-sHilo20$Year
sHilo.sen20<-ci.sHilo.sen20<-list()
sLih.sen20<-ci.sLih.sen20<-list()

for (i in 1:(ncol(sHilo20)-1)){
  yH<-sHilo20[,i]
  if(!any(is.na(yH))){
    sHilo.sen20[[i]]<-zyp.sen(yH~yr)
    ci.sHilo.sen20[[i]]<-confint(sHilo.sen20[[i]]) 
    names(sHilo.sen20)[i]<-names(ci.sHilo.sen20)[i]<-names(sHilo20)[i]
  }
  
  yL<-sLih20[,i]
  if(!any(is.na(yL))){
    sLih.sen20[[i]]<-zyp.sen(yL~yr)
    ci.sLih.sen20[[i]]<-confint(sLih.sen20[[i]]) 
    names(sLih.sen20)[i]<-names(ci.sLih.sen20)[i]<-names(sLih20)[i]
  }
}


trend.sHilo.20<-data.frame(trend=unlist(lapply(sHilo.sen20, function(x) coef(x)["yr"])),
                           CI=matrix(unlist(lapply(ci.sHilo.sen20, function(x) x["yr",])), 
                                     ncol=2, byrow=T),
                           metric=paste0("Hilo_", names(sHilo20)[-ncol(sHilo20)]),
                           start="1998", end="2017")
trend.sLih.20<-data.frame(trend=unlist(lapply(sLih.sen20, function(x) coef(x)["yr"])),
                          CI=matrix(unlist(lapply(ci.sLih.sen20, function(x) x["yr",])), 
                                    ncol=2, byrow=T),
                          metric=paste0("Lih_", names(sLih20)[-ncol(sLih20)]),
                          start="1998", end="2017")


## sounding PROFILES
yr<-rh20$year
rh.sen20<-ci.rh.sen20<-list()
temp.sen20<-ci.temp.sen20<-list()
mixr.sen20<-ci.mixr.sen20<-list()
sknt.sen20<-ci.sknt.sen20<-list()

for (i in 1:(ncol(rh20)-1)){
  yrh<-rh20[,i+1]
  if(!any(is.na(yrh))){
    rh.sen20[[i]]<-zyp.sen(yrh~yr)
    ci.rh.sen20[[i]]<-confint(rh.sen20[[i]]) 
  }
  
  yt<-temp20[,i+1]
  if(!any(is.na(yt))){
    temp.sen20[[i]]<-zyp.sen(yt~yr)
    ci.temp.sen20[[i]]<-confint(temp.sen20[[i]]) 
  }
  
  ym<-mixr20[,i+1]
  if(!any(is.na(ym))){
    mixr.sen20[[i]]<-zyp.sen(ym~yr)
    ci.mixr.sen20[[i]]<-confint(mixr.sen20[[i]]) 
  }
  
  ys<-sknt20[,i+1]
  if(!any(is.na(ys))){
    sknt.sen20[[i]]<-zyp.sen(ys~yr)
    ci.sknt.sen20[[i]]<-confint(sknt.sen20[[i]]) 
  }
}


trend.rh.20<-data.frame(trend=unlist(lapply(rh.sen20, function(x) coef(x)["yr"])),
                        CI=matrix(unlist(lapply(ci.rh.sen20, function(x) x["yr",])), 
                                  ncol=2, byrow=T),
                        metric=paste0("rh_", names(rh20)[-1]),
                        start="1998", end="2017")
trend.temp.20<-data.frame(trend=unlist(lapply(temp.sen20, function(x) coef(x)["yr"])),
                          CI=matrix(unlist(lapply(ci.temp.sen20, function(x) x["yr",])), 
                                    ncol=2, byrow=T),
                          metric=paste0("temp_", names(temp20)[-1]),
                          start="1998", end="2017")

trend.mixr.20<-data.frame(trend=unlist(lapply(mixr.sen20, function(x) coef(x)["yr"])),
                          CI=matrix(unlist(lapply(ci.mixr.sen20, function(x) x["yr",])), 
                                    ncol=2, byrow=T),
                          metric=paste0("mixr_", names(mixr20)[-1]),
                          start="1998", end="2017")
trend.sknt.20<-data.frame(trend=unlist(lapply(sknt.sen20, function(x) coef(x)["yr"])),
                          CI=matrix(unlist(lapply(ci.sknt.sen20, function(x) x["yr",])), 
                                    ncol=2, byrow=T),
                          metric=paste0("ws_", names(sknt20)[-1]),
                          start="1998", end="2017")

yr<-indices20$Year
i.sen20<-ci.i.sen20<-list()
st.nd<-matrix(0, ncol=2, nrow=ncol(indices20)-2)

for (i in 1:(ncol(indices20)-2)){
  yi<-indices20[,i+2]
  if(!all(is.na(yi))){
    i.sen20[[i]]<-zyp.sen(yi~yr)
    ci.i.sen20[[i]]<-confint(i.sen20[[i]]) 
    st.nd[i,]<-range(indices20$Year[!is.na(indices20[,i+2])])
    names(i.sen20)[i]<-names(indices20)[i+2]
  }
  
}

trend.i.20<-data.frame(trend=unlist(lapply(i.sen20, function(x) coef(x)["yr"])),
                       CI=matrix(unlist(lapply(ci.i.sen20, function(x) x["yr",])), 
                                 ncol=2, byrow=T),
                       metric=names(i.sen20),
                       start=as.character(st.nd[,1]), end=as.character(st.nd[,2]))


trend.20<-rbind(trend.Tdat.20, trend.temp.20, trend.rh.20, trend.mixr.20, trend.sknt.20,
                trend.cphto.20, trend.cphli.20, trend.i.20, trend.sHilo.20, trend.sLih.20)

names(trend.20)[2:3]<-c("CI.025", "CI.975")


trends<-rbind(trend.40, trend.20)

write.csv(trends, "Trends.csv", row.names=F)
