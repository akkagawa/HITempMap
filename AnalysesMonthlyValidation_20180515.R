## AnalysesMonthlyValidation_20180525.R
## 6/17/2017; re-run 5/9/2018, 5/16/2018, 5/25/2018
## A. Kagawa-Viviani
## Notes: this script performs model fitting, evaluation, and mapping:
##    1) mean MONTHLY station values (2006-2017)
##        a) simple linear regression on elevation for Tmax, Tmin, Tavg
##        b) multiple linear regression using other predictor variables
##            for Tmax, Tmin, Tavg, and DTR, using variables selected through AIC
##    2) month-year station values
##        a) regression on elevation (2006-2017)
##        b) multiple linear regression using other predictor variables
##            for Tmax, Tmin, Tavg, and DTR, using variables selected through AIC


setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping")
par.default<-par()
library(segmented)

## Read in annual data file (12 station months/observation)
annual<-read.csv("Merged_yr.csv", stringsAsFactors = F)
offshore<-c(grep("USC00516271", annual$Station), # remove Midway (USC00516271) 
            grep("USC00510350", annual$Station), # remove Moku o Loe (USC00510350)
            grep("USR0000HMOL", annual$Station)) # remove Moloaa Dairy (USR0000HMOL) QA/QC fail
annual<-annual[-offshore,]

# Look at when high elevation (above TWI) stations kick in
hi<-subset(annual, CorrElev_m>=2150)  
tapply(hi$Year, INDEX=hi$Year, FUN=length)  

# RESTRICT YEARS to current (stations above TWI>3)
annual<-annual[annual$Year>=2006 & annual$Year<=2017,]  
length(unique(annual$Station)) # 81 stations

# Now focus on MONTHLY data
monthly<-read.csv("Merged_mo.csv", stringsAsFactors = F)

# RESTRICT to the 81 stations with complete years
fullyr<-data.frame(Station=unique(annual$Station))  # 81 stations
monthly<-merge(fullyr, monthly, all.x=T)   # 65131->25699 observations, 39%

# RESTRICT YEARS to current (stations above TWI>3)
monthly<-monthly[monthly$Year>=2006 & monthly$Year<=2017,]  # 25699->9135 obs, 36%
length(unique(monthly$Station)) # 81 stations

### You can skip ahead to monthly 2006-2017 models/maps from here (line 350) ######

# Define station-months
stnmo<-paste0(monthly$Station, sprintf("_%02d", monthly$Month))

## Calculate station mean (for all years available)
Tmax<-tapply(monthly$Tmax, stnmo, mean)
Tmin<-tapply(monthly$Tmin, stnmo, mean)
Tavg<-tapply(monthly$Tavg, stnmo, mean)
DTR<-tapply(monthly$DTR, stnmo, mean)
yrs<-tapply(monthly$Tavg, stnmo, length)
length(yrs)   # 972 mean station months

## Organize response and predictor variables into a master dataframe
stn_mo<-names(Tmax)
ID<-matrix(unlist(strsplit(stn_mo, "_")), ncol=2, byrow=T)[,1]
mo<-matrix(unlist(strsplit(stn_mo, "_")), ncol=2, byrow=T)[,2]

ys<-data.frame(Tmax, Tmin, Tavg, DTR, yrs, stn_mo, 
               ID=as.character(ID), mo=as.character(mo))
xs<-read.csv("Analyses/SpatialCovar.csv")

xyt<-merge(ys, xs[,-c(2:11)], by="ID", all.x=T)  # merge yvals and xvals (minus unnecessary columns)

## Identify the extra monthly rainfall and cloud frequency and hourly wind values
excl<-which(grepl("staterf", names(xyt))|  # drop monthly, hourly series
              grepl("cl_frq", names(xyt))| 
              grepl("wind", names(xyt)))

incl<-c(grep("staterf_mmann",names(xyt)),  # keep annual
        grep("cl_frq_ann",names(xyt)),
        grep("wind_sp_ms",names(xyt)))

xyty<-xyt[,-excl]
xyty<-cbind(xyty, xyt[,incl])

# Focus on consistent station-months, 2006-2017 50% coverage:
xyty.s<-xyty[xyty$yrs>=6,]  # drop station-months with 5 years or less
length(unique(xyty.s$ID))   # down to 73 stations with 6 years+ data for a given month
xyty.s$U1.CorrElev_m<-(xyty.s$CorrElev_m-2150)*(xyty.s$CorrElev_m >2150)

# # Focus on HIGHLY consistent station-months, 2006-2017:
# xyty.12<-xyty[xyty$yrs==12,]  # keep station-months with 12 years data
# length(unique(xyty.12$ID))   # down to 47 station-months with 6 years+ data for a given month
# xyty.12$U1.CorrElev_m<-(xyty.12$CorrElev_m-2150)*(xyty.12$CorrElev_m >2150)

par(mfrow=c(4,3))
for (i in 1:12){
  plot(Tmax~CorrElev_m, data=xyty.s[xyty.12$mo==unique(xyty.s$mo)[i],], col="red")
}


######### Monthly models #############  ### need to fit one for each mean month
mos<-sprintf("%02d", 1:12)

##### For each month, fit SEGMENTED multiple regressions
names.smlr<-c("Tmax", "Tmin", "Tavg", "DTR")
mlr.mod<-list(Tmax=list(), Tmin=list(), Tavg=list(), DTR=list()) 
smlr.mod<-list(Tmax=list(), Tmin=list(), Tavg=list(), DTR=list()) 
smlr.coefs<-list(Tmax=matrix(nrow=12, ncol=7), Tmin=matrix(nrow=12, ncol=7), 
                 Tavg=matrix(nrow=12, ncol=7), DTR=matrix(nrow=12, ncol=7))

par(mfrow=c(4,3))

for (i in 1:length(mos)){
  dat<-xyty.s[xyty.s$mo==mos[i],] # subset data for a given month
  plot(dat$Tmin~dat$CorrElev_m, col="blue",
       ylim=range(c(dat$Tmin, dat$Tmax)), main=mos[i])
  points(dat$Tmax~dat$CorrElev_m, col="red")
  
  # fit linear model for subsequent segmented() function
  mlr.mod$Tmax[[i]]<-lm(Tmax~ CorrElev_m + cpi_wgs84 + staterf_mmann + cl_frq_ann, data=dat)
  mlr.mod$Tmin[[i]]<-lm(Tmin ~ CorrElev_m + cpi_wgs84 + lai_ann + wind_sp_ms, data=dat)
  mlr.mod$Tavg[[i]]<-lm(Tavg ~ CorrElev_m + staterf_mmann + wind_sp_ms + cl_frq_ann, data=dat)
  mlr.mod$DTR[[i]]<-lm(DTR ~ CorrElev_m + cpi_wgs84 + cl_frq_ann + wind_sp_ms, data=dat)
  
  # fit segmented linear model
  smlr.mod$Tmax[[i]]<-segmented.lm(mlr.mod$Tmax[[i]], seg.Z=~CorrElev_m, psi=2150, 
                                 control=seg.control(it.max=0))
  smlr.mod$Tmin[[i]]<-segmented.lm(mlr.mod$Tmin[[i]], seg.Z=~CorrElev_m, psi=2150, 
                                 control=seg.control(it.max=0))
  smlr.mod$Tavg[[i]]<-segmented.lm(mlr.mod$Tavg[[i]], seg.Z=~CorrElev_m, psi=2150, 
                                 control=seg.control(it.max=0))
  smlr.mod$DTR[[i]]<-segmented.lm(mlr.mod$DTR[[i]], seg.Z=~CorrElev_m, psi=2150, 
                                control=seg.control(it.max=0))
  
  pred.max<-predict(smlr.mod$Tmax[[i]],dat)
  points(pred.max~dat$CorrElev_m, pch="*")
  pred.min<-predict(smlr.mod$Tmin[[i]],dat)
  points(pred.min~dat$CorrElev_m, pch="*")
}

# extract coefficients
smlr.coefs$Tmax<-lapply(smlr.mod$Tmax, FUN= function(x) {c(coef(x), summary(x)$adj.r.squared)})
smlr.coefs$Tmin<-lapply(smlr.mod$Tmin, FUN= function(x) {c(coef(x), summary(x)$adj.r.squared)})
smlr.coefs$Tavg<-lapply(smlr.mod$Tavg, FUN= function(x) {c(coef(x), summary(x)$adj.r.squared)})
smlr.coefs$DTR<-lapply(smlr.mod$DTR, FUN= function(x) {c(coef(x), summary(x)$adj.r.squared)})

tmaxcoefs<-data.frame(mos, matrix(unlist(smlr.coefs$Tmax), byrow=T, ncol=7))
names(tmaxcoefs)<-c("Month", names(coef(smlr.mod$Tmax[[1]])), "adj.R.sq")

tmincoefs<-data.frame(mos, matrix(unlist(smlr.coefs$Tmin), byrow=T, ncol=7))
names(tmincoefs)<-c("Month", names(coef(smlr.mod$Tmin[[1]])), "adj.R.sq")

tavgcoefs<-data.frame(mos, matrix(unlist(smlr.coefs$Tavg), byrow=T, ncol=7))
names(tavgcoefs)<-c("Month", names(coef(smlr.mod$Tavg[[1]])), "adj.R.sq")

dtrcoefs<-data.frame(mos, matrix(unlist(smlr.coefs$DTR), byrow=T, ncol=7))
names(dtrcoefs)<-c("Month", names(coef(smlr.mod$DTR[[1]])), "adj.R.sq")

write.csv(tmaxcoefs, file="Analyses/Maps/coefsMO.Tmax.csv", row.names=F)
write.csv(tmincoefs, file="Analyses/Maps/coefsMO.Tmin.csv", row.names=F)
write.csv(tavgcoefs, file="Analyses/Maps/coefsMO.Tavg.csv", row.names=F)
write.csv(dtrcoefs, file="Analyses/Maps/coefsMO.DTR.csv", row.names=F)

par(mfrow=c(2,2))  # plot fitted coefficients through time
for (j in 2:8){
  plot(tmaxcoefs[,j])
  plot(tmincoefs[,j])
  plot(tavgcoefs[,j])
  plot(dtrcoefs[,j])
}

# look at summaries
smlr.summary<-list()
smlr.summary$Tmax<-lapply(smlr.mod$Tmax, FUN= summary)
smlr.summary$Tmin<-lapply(smlr.mod$Tmin, FUN= summary)
smlr.summary$Tavg<-lapply(smlr.mod$Tavg, FUN= summary)
smlr.summary$DTR<-lapply(smlr.mod$DTR, FUN= summary)

######## Map these back onto a raster!
## First read in rasters
library('raster')
library('rgdal')

## Most HI Temp covariates here:
dirs<-list.dirs("C:/Users/Aurora/OneDrive/GIS", full.names=T)  # list directories
dem<-dirs[grep("HITemp_rastercovariates/clipdem", dirs)]      # dem, 2 files
cpi<-dirs[grep("HITemp_rastercovariates/cpi", dirs)]          # coastal proximity
staterf<-dirs[grep("StateRFGrids_mm/staterf_mmann", dirs)] #Rainfall Atlas, annual
clouds<-dirs[grep("CloudFreq_month_raster/cl_frq_ann", dirs)]# cloud cover
winds<-dirs[grep("WindSpeed_ann_hr_raster/wind_sd", dirs)]    # all 24 hour wind
lai<-dirs[grep("HITemp_rastercovariates/lai_ann", dirs)]      # LAI

# ### Not used
# tpi<-dirs[grep("tpi", dirs)]       # topographic position
# albedo<-dirs[grep("albedo", dirs)] # albedo
# ndvi<-dirs[grep("ndvi_ann", dirs)]
# evi<-dirs[grep("evi_ann", dirs)]
# veg_ht<-dirs[grep("veg_ht_ann", dirs)]
# fr_veg<-dirs[grep("fr_v_c_ann", dirs)]

#### Read in raster data
# wind_sp_ms<-rowMeans(covariates[, grep(pattern="wind_sd",names(covariates))])

dem.ann<-raster(dem[2])  #these have different extent, resolution
cpi.ann<-raster(cpi)
staterf_mmann<-raster(staterf)
cl_frq_ann<-raster(clouds)
winds.24<-stack(winds); wind_sp_ms<-mean(winds.24)
lai_ann<-raster(lai)

cov<-list(dem.ann, cpi.ann, 
          staterf_mmann, cl_frq_ann, wind_sp_ms, lai_ann)

lapply(cov, FUN=proj4string)  # check projection, crs  (all same)
lapply(cov, FUN=extent)       # check extent (first two are different)
lapply(cov, FUN=res)          # check resolution (first two are different)

inters<-intersect(extent(dem.ann), extent(staterf_mmann)) # smallest extent
inters

# Resample these to match other rasters' extent and resolution; save to file
#demMatch<-resample(dem.ann, staterf_mmann, filename="Analyses/Maps/DEMresample2.grd")
#cpiMatch<-resample(cpi.ann, staterf_mmann, filename="Analyses/Maps/CPIresample2.grd")
# this step takes a long time

# Read in resampled rasters and rename rasters for easy prediction
CorrElev_m<-raster("Analyses/Maps/DEMresample2.grd")
cpi_wgs84<-raster("Analyses/Maps/CPIresample2.grd")

U1.CorrElev_m<-(CorrElev_m-2150)*(CorrElev_m >2150)

covars<-stack(x=list(CorrElev_m=CorrElev_m, 
                     cpi_wgs84=cpi_wgs84,
                     staterf_mmann=staterf_mmann, 
                     cl_frq_ann=cl_frq_ann, 
                     wind_sp_ms=wind_sp_ms,
                     lai_ann=lai_ann, 
                     U1.CorrElev_m=U1.CorrElev_m))
  
# For each month,
Tmax.pred<-list()
Tmin.pred<-list()
Tavg.pred<-list()
DTR.pred<-list()

# predict raster
for (i in 1:12){   
  Tmax.pred[[i]]<-predict(covars, smlr.mod$Tmax[[i]], progress='text')
  Tmin.pred[[i]]<-predict(covars, smlr.mod$Tmin[[i]], progress='text')
  Tavg.pred[[i]]<-predict(covars, smlr.mod$Tavg[[i]], progress='text')
  DTR.pred[[i]]<-predict(covars, smlr.mod$DTR[[i]], progress='text')
}

# stack raster
Tmax.pred.stack<-stack(Tmax.pred)
Tmin.pred.stack<-stack(Tmin.pred)
Tavg.pred.stack<-stack(Tavg.pred)
DTR.pred.stack<-stack(DTR.pred)

# set color scale for stacks
fixstackcolors<-function(tmax, tmin,n){   
  rng<-range(cellStats(tmax, max), cellStats(tmin, min))
  round(seq(from=floor(rng[1]), to=ceiling(rng[2]), length.out=n))}
my.colors<-colorRampPalette(c("blue", "green", "light green", "yellow", "orange", "red"))

fixbreaks<-fixstackcolors(Tmax.pred.stack,    # fix color scale for monthly raster stacks
                          Tmin.pred.stack, 15) 


par(mfrow=c(4,3))
plot(Tmax.pred.stack, breaks=fixbreaks, col=my.colors(15))
plot(Tmin.pred.stack, breaks=fixbreaks, col=my.colors(15))
plot(Tavg.pred.stack, breaks=fixbreaks, col=my.colors(15))

plot(DTR.pred.stack, breaks=fixstackcolors(DTR.pred.stack, DTR.pred.stack, 15),
     col=my.colors(15))

Temp<-list(Tmax=Tmax.pred.stack, 
           Tmin=Tmin.pred.stack,
           Tavg=Tavg.pred.stack, 
           DTR=DTR.pred.stack)

## Now save these files as geotiffs------------------------------------------------------
for (i in 1:length(Temp)){
  writeRaster(Temp[[i]],
              filename=paste0("Analyses/Maps/Monthly_", 
                              names(Temp)[i], "stack.tif"),
              format="GTiff", bylayer=F, overwrite=T)
}

## And print them as pngs for easy viewing:
for (i in 1:length(mos)){
  png(file=paste0("Analyses/Maps/Tmax_", mos[i], ".png"), bg="transparent")
  plot(Temp$Tmax[[i]], main=paste("Tmax, Month =", month.abb[i]),
       zlim=c(0,32))      # set color ramp
  dev.off()
  
  png(file=paste0("Analyses/Maps/Tmin_", mos[i], ".png"), bg="transparent")
  plot(Temp$Tmin[[i]], main=paste("Tmin, Month =", month.abb[i]),
       zlim=c(0,32))      # set color ramp
  dev.off()
  
  png(file=paste0("Analyses/Maps/Tavg_", mos[i], ".png"), bg="transparent")
  plot(Temp$Tavg[[i]], main=paste("Tavg, Month =", month.abb[i]),
       zlim=c(0,32))     # set color ramp
  dev.off()
  
  png(file=paste0("Analyses/Maps/DTR_", mos[i], ".png"), bg="transparent")
  plot(Temp$DTR[[i]], main=paste("DTR, Month =", month.abb[i]), zlim=c(2,16))
  dev.off()
}


# pred vs obs by month
for (i in 1:12){
  dat<-xyty.s[xyty.s$mo==mos[i],]
  pred.max<-predict(smlr.mod$Tmax[[i]],dat) 
  plot(dat$Tmax~pred.max, col="red", main=mos[i])
  abline(0,1)
} 

# pred vs obs by station
Ids<-unique(xyty.s$ID)
pred.max<-numeric(length=12)
pred.min<-numeric(length=12)
pred.avg<-numeric(length=12)
pred.dtr<-numeric(length=12)

par(mfrow=c(2,2))
for (i in 1:length(Ids)){
  stn<-xyty.s[xyty.s$ID==Ids[i],]
  stn<-merge(data.frame(mo=mos), stn, all.x=T)
  for (j in 1:12){
    dat<-stn[stn$mo==mos[j],]
    if (dim(dat)[1]!=0){
      pred.max[j]<-predict(smlr.mod$Tmax[[j]],dat) 
      pred.min[j]<-predict(smlr.mod$Tmin[[j]],dat) 
      pred.avg[j]<-predict(smlr.mod$Tavg[[j]],dat) 
      pred.dtr[j]<-predict(smlr.mod$DTR[[j]],dat)
    }else{
      pred.max[j]<-NA
      pred.min[j]<-NA 
      pred.avg[j]<-NA
      pred.dtr[j]<-NA
    }
  } 
  plot(stn$Tmax~pred.max, main=Ids[i], col="red", xlab="Predicted")
  abline(0,1)
  plot(stn$Tmin~pred.min, sub="Tmin", col="blue", xlab="Predicted")
  abline(0,1)
  plot(stn$Tavg~pred.avg, sub="Tavg", col="black", xlab="Predicted")
  abline(0,1)
  plot(stn$DTR~pred.dtr, sub="DTR", col="dark gray", xlab="Predicted")
  abline(0,1)
}

# we get pretty good... one way to get better maps is with spBayes and geospatial stats
# we consistently over- or under- predict at a given location (across seasons)

############################################################################
########### Develop MONTHLY maps for 2006-2017 #####################################

# Fit and plot by month-year
monthly$ID<-monthly$Station  # previously defined for 2006-2017

## Organize response and predictor variables into a master dataframe
xs<-read.csv("Analyses/SpatialCovar.csv")
ys<-monthly[,c("ID", "MonthYear", "Tmax", "Tmin", "Tavg", "DTR", "Month", "Year")]
m06_17<-merge(ys, xs[,-c(2:11)], by="ID", all.x=T)  # merge yvals and xvals (minus unnecessary columns)

## Identify the extra monthly rainfall and cloud frequency and hourly wind values
excl<-which(grepl("staterf", names(m06_17))|  # drop monthly, hourly series
              grepl("cl_frq", names(m06_17))| 
              grepl("wind", names(m06_17)))

incl<-c(grep("staterf_mmann",names(m06_17)),  # keep annual
        grep("cl_frq_ann",names(m06_17)),
        grep("wind_sp_ms",names(m06_17)))

m06_17x<-m06_17[,-excl]
m06_17x<-cbind(m06_17x, m06_17[,incl])

# Focus on 2006-2017 and use ALL stations:
length(unique(m06_17x$ID))   # 81 stations 
obs<-tapply(m06_17x$Tmax, m06_17x$MonthYear, length)
range(obs)  # 53-73 stations for any given month
obs

m06_17x$U1.CorrElev_m<-(m06_17x$CorrElev_m-2150)*(m06_17x$CorrElev_m >2150)

######### Monthly models from JAN 2006- DEC 2017 #############  
moyr<-sort(unique(m06_17x$MonthYear))  
Year<-sort(unique(m06_17x$Year))
Month<-sort(unique(m06_17x$Month))

##### For each month, fit SEGMENTED multiple regressions
names.smlr.m06_17<-c("Tmax", "Tmin", "Tavg", "DTR")
mlr.mod.m06_17<-list(Tmax=vector("list", 12), 
                     Tmin=vector("list", 12), 
                     Tavg=vector("list", 12), 
                     DTR=vector("list", 12)) 
smlr.mod.m06_17<-list(Tmax=vector("list", 12), 
                     Tmin=vector("list", 12), 
                     Tavg=vector("list", 12), 
                     DTR=vector("list", 12)) 

tmax.coefs.m06_17<-vector("list", 12)
tmin.coefs.m06_17<-vector("list", 12) 
tavg.coefs.m06_17<-vector("list", 12)
dtr.coefs.m06_17<-vector("list", 12)

coefmat<-function(mod) {c(coef(mod), summary(mod)$adj.r.squared)}

# Temporary vectors...
tmax.yr.mod<-vector("list", length=12)
tmin.yr.mod<-vector("list", length=12)
tavg.yr.mod<-vector("list", length=12)
dtr.yr.mod<-vector("list", length=12)

tmax.yr.seg<-vector("list", length=12)
tmin.yr.seg<-vector("list", length=12)
tavg.yr.seg<-vector("list", length=12)
dtr.yr.seg<-vector("list", length=12)


par(mfrow=c(4,3))
for (i in 1:length(Year)){                     # for each year
  for (j in 1:length(Month)) {                  # and month
    dat<-m06_17x[m06_17x$Year==Year[i] & m06_17x$Month==Month[j],] # subset data (don't use subset?!)
    print(paste(Year[i], Month[j], dim(dat)[1]))
    plot(Tmin~CorrElev_m, data=dat, col="blue",
         ylim=range(dat[,c("Tmin", "Tmax")]),
         main=paste(Year[i], Month[j]))
    points(dat$Tmax~dat$CorrElev_m, col="red")
    
    # fit linear model for subsequent segmented() function
    tmax.yr.mod[[j]]<-lm(Tmax~ CorrElev_m + cpi_wgs84 + staterf_mmann + cl_frq_ann, data=dat)
    tmin.yr.mod[[j]]<-lm(Tmin ~ CorrElev_m + cpi_wgs84 + lai_ann + wind_sp_ms, data=dat)
    tavg.yr.mod[[j]]<-lm(Tavg ~ CorrElev_m + staterf_mmann + wind_sp_ms + cl_frq_ann, data=dat)
    dtr.yr.mod[[j]]<-lm(DTR ~ CorrElev_m + cpi_wgs84 + cl_frq_ann + wind_sp_ms, data=dat)
    
    # fit segmented linear model
    tmax.yr.seg[[j]]<-segmented.lm(tmax.yr.mod[[j]], seg.Z=~CorrElev_m, psi=2150, 
                                   control=seg.control(it.max=0))
    tmin.yr.seg[[j]]<-segmented.lm(tmin.yr.mod[[j]], seg.Z=~CorrElev_m, psi=2150, 
                                   control=seg.control(it.max=0))
    tavg.yr.seg[[j]]<-segmented.lm(tavg.yr.mod[[j]], seg.Z=~CorrElev_m, psi=2150, 
                                   control=seg.control(it.max=0))
    dtr.yr.seg[[j]]<-segmented.lm(dtr.yr.mod[[j]], seg.Z=~CorrElev_m, psi=2150, 
                                  control=seg.control(it.max=0))
    
    pred.max<-predict(tmax.yr.seg[[j]],dat)
    points(pred.max~dat$CorrElev_m, pch="*")
    
    pred.min<-predict(tmin.yr.seg[[j]],dat)
    points(pred.min~dat$CorrElev_m, pch="*")
  }
  
  # list of regression models
  mlr.mod.m06_17$Tmax[[i]]<-tmax.yr.mod
  mlr.mod.m06_17$Tmin[[i]]<-tmin.yr.mod
  mlr.mod.m06_17$Tavg[[i]]<-tavg.yr.mod
  mlr.mod.m06_17$DTR[[i]]<-dtr.yr.mod
  
  # list of segmented linear models
  smlr.mod.m06_17$Tmax[[i]]<-tmax.yr.seg
  smlr.mod.m06_17$Tmin[[i]]<-tmin.yr.seg
  smlr.mod.m06_17$Tavg[[i]]<-tavg.yr.seg
  smlr.mod.m06_17$DTR[[i]]<-dtr.yr.seg
  
  # extract coefficients
  tmax.coefs.m06_17[[i]]<-cbind(Month, matrix(unlist(lapply(smlr.mod.m06_17$Tmax[[i]], FUN= coefmat)), 
                                      ncol=7, byrow=T))
  tmin.coefs.m06_17[[i]]<-cbind(Month, matrix(unlist(lapply(smlr.mod.m06_17$Tmin[[i]], FUN= coefmat)), 
                                      ncol=7, byrow=T))
  tavg.coefs.m06_17[[i]]<-cbind(Month, matrix(unlist(lapply(smlr.mod.m06_17$Tavg[[i]], FUN= coefmat)), 
                                      ncol=7, byrow=T))
  dtr.coefs.m06_17[[i]]<-cbind(Month, matrix(unlist(lapply(smlr.mod.m06_17$DTR[[i]], FUN= coefmat)), 
                                     ncol=7, byrow=T))
}

names(tmax.coefs.m06_17)<-names(tmin.coefs.m06_17)<-Year
names(tavg.coefs.m06_17)<-names(dtr.coefs.m06_17)<-Year

for (i in 1:length(Year)){
  tmax.dat<-data.frame(Year=Year[i], tmax.coefs.m06_17[[i]])
  names(tmax.dat)<-c("Year", "Month", names(coef(tmax.yr.seg[[1]])), "adj.R.sq")
  
  tmin.dat<-data.frame(Year=Year[i], tmin.coefs.m06_17[[i]])
  names(tmin.dat)<-c("Year", "Month", names(coef(tmin.yr.seg[[1]])), "adj.R.sq")

  tavg.dat<-data.frame(Year=Year[i], tavg.coefs.m06_17[[i]])
  names(tavg.dat)<-c("Year", "Month", names(coef(tavg.yr.seg[[1]])), "adj.R.sq")

  dtr.dat<-data.frame(Year=Year[i], dtr.coefs.m06_17[[i]])
  names(dtr.dat)<-c("Year", "Month", names(coef(dtr.yr.seg[[1]])), "adj.R.sq")
  
  if (i==1){
    tmax.coefs.fin<-tmax.dat
    tmin.coefs.fin<-tmin.dat
    tavg.coefs.fin<-tavg.dat
    dtr.coefs.fin<-dtr.dat
    
  }else{
    tmax.coefs.fin<-rbind(tmax.coefs.fin, tmax.dat)
    tmin.coefs.fin<-rbind(tmin.coefs.fin, tmin.dat)
    tavg.coefs.fin<-rbind(tavg.coefs.fin, tavg.dat)
    dtr.coefs.fin<-rbind(dtr.coefs.fin, dtr.dat)
    }
}

write.csv(tmax.coefs.fin, file="Analyses/Maps/coefs06_17.Tmax.csv", row.names=F)
write.csv(tmin.coefs.fin, file="Analyses/Maps/coefs06_17.Tmin.csv", row.names=F)
write.csv(tavg.coefs.fin, file="Analyses/Maps/coefs06_17.Tavg.csv", row.names=F)
write.csv(dtr.coefs.fin, file="Analyses/Maps/coefs06_17.DTR.csv", row.names=F)

# plot fitted coefficients through time; sanity check
par(mfrow=c(4,4))  
for (n in 3:9){  # for each coefficient
  if (n %in% c(3,4,8,9)){  # common coefs
    ylim<-range(c(tmax.coefs.fin[,n], tmin.coefs.fin[,n]))
    plot(tmax.coefs.fin[,n], ylim=ylim, ylab=names(tmax.coefs.fin)[n], col="red")
    plot(tmin.coefs.fin[,n], ylim=ylim, ylab=names(tmin.coefs.fin)[n],col="blue")
    plot(tavg.coefs.fin[,n], ylim=ylim, ylab=names(tavg.coefs.fin)[n],col="black")
  }else{
    plot(tmax.coefs.fin[,n], ylab=names(tmax.coefs.fin)[n], col="red")
    plot(tmin.coefs.fin[,n], ylab=names(tmin.coefs.fin)[n],col="blue")
    plot(tavg.coefs.fin[,n], ylab=names(tavg.coefs.fin)[n],col="black")
  }
  plot(dtr.coefs.fin[,n], ylab=names(tmax.coefs.fin)[n], col="dark gray", pch="*")
}

# look at summaries
tmax.m06_17.summary<-vector("list", length=length(Year))
tmin.m06_17.summary<-vector("list", length=length(Year))
tavg.m06_17.summary<-vector("list", length=length(Year))
dtr.m06_17.summary<-vector("list", length=length(Year))
for (i in 1:length(Year)){
  tmax.m06_17.summary[[i]]<-lapply(smlr.mod.m06_17$Tmax[[i]], summary) # 12 months of summaries in each year
  tmin.m06_17.summary[[i]]<-lapply(smlr.mod.m06_17$Tmin[[i]], summary)
  tavg.m06_17.summary[[i]]<-lapply(smlr.mod.m06_17$Tavg[[i]], summary)
  dtr.m06_17.summary[[i]]<-lapply(smlr.mod.m06_17$DTR[[i]], summary)
}

######## Map these back onto a raster!
## First read in rasters
library('raster')
library('rgdal')

## Most HI Temp covariates here:
dirs<-list.dirs("C:/Users/Aurora/OneDrive/GIS", full.names=T)  # list directories
dem<-dirs[grep("HITemp_rastercovariates/clipdem", dirs)]      # dem, 2 files
cpi<-dirs[grep("HITemp_rastercovariates/cpi", dirs)]          # coastal proximity
staterf<-dirs[grep("StateRFGrids_mm/staterf_mmann", dirs)] #Rainfall Atlas, annual
clouds<-dirs[grep("CloudFreq_month_raster/cl_frq_ann", dirs)]# cloud cover
winds<-dirs[grep("WindSpeed_ann_hr_raster/wind_sd", dirs)]    # all 24 hour wind
lai<-dirs[grep("HITemp_rastercovariates/lai_ann", dirs)]      # LAI

## Read in rasters and set them up
dem.ann<-raster(dem[2])  #these have different extent, resolution
cpi.ann<-raster(cpi)
staterf_mmann<-raster(staterf)
cl_frq_ann<-raster(clouds)
winds.24<-stack(winds); wind_sp_ms<-mean(winds.24)
lai_ann<-raster(lai)

cov<-list(dem.ann, cpi.ann, 
          staterf_mmann, cl_frq_ann, wind_sp_ms, lai_ann)

lapply(cov, FUN=proj4string)  # check projection, crs  (all same)
lapply(cov, FUN=extent)       # check extent (first two are different)
lapply(cov, FUN=res)          # check resolution (first two are different)

inters<-intersect(extent(dem.ann), extent(staterf_mmann)) # smallest extent
inters

# Resample these to match other rasters' extent and resolution; save to file
# demMatch<-resample(dem.ann, staterf_mmann, filename="Analyses/Maps/DEMresample2.grd")
# cpiMatch<-resample(cpi.ann, staterf_mmann, filename="Analyses/Maps/CPIresample2.grd")
# Read in resampled rasters and rename rasters for easy prediction
CorrElev_m<-raster("Analyses/Maps/DEMresample2.grd")
cpi_wgs84<-raster("Analyses/Maps/CPIresample2.grd")

U1.CorrElev_m<-(CorrElev_m-2150)*(CorrElev_m >2150)

covars<-stack(x=list(CorrElev_m=CorrElev_m, 
                     cpi_wgs84=cpi_wgs84,
                     staterf_mmann=staterf_mmann, 
                     cl_frq_ann=cl_frq_ann, 
                     wind_sp_ms=wind_sp_ms,
                     lai_ann=lai_ann, 
                     U1.CorrElev_m=U1.CorrElev_m))


# set color scale for plotting
fixstackcolors<-function(tmax, tmin,n){   
  rng<-range(cellStats(tmax, max), cellStats(tmin, min))
  round(seq(from=floor(rng[1]), to=ceiling(rng[2]), length.out=n))}
my.colors<-colorRampPalette(c("blue", "green", "light green", "yellow", "orange", "red"))


# predict raster
for (i in 1:length(Year)){    # For each year,
  
  # For each year, have a temporary vector
  Tmax.pred<-vector("list", length=12)  # length 12 for each month
  Tmin.pred<-vector("list", length=12)
  Tavg.pred<-vector("list", length=12)
  DTR.pred<-vector("list", length=12)
  
  for (j in 1:length(Month)){
    Tmax.pred[[j]]<-predict(covars, smlr.mod.m06_17$Tmax[[i]][[j]], progress='text')
    Tmin.pred[[j]]<-predict(covars, smlr.mod.m06_17$Tmin[[i]][[j]], progress='text')
    Tavg.pred[[j]]<-predict(covars, smlr.mod.m06_17$Tavg[[i]][[j]], progress='text')
    DTR.pred[[j]]<-predict(covars, smlr.mod.m06_17$DTR[[i]][[j]], progress='text')
  }
  
  # stack raster
  Tmax.pred.stack<-stack(Tmax.pred); 
  Tmin.pred.stack<-stack(Tmin.pred)
  Tavg.pred.stack<-stack(Tavg.pred)
  DTR.pred.stack<-stack(DTR.pred)
  
  ## Now save these raster stacks as 12-layer geotiffs------
  writeRaster(Tmax.pred.stack,
              filename=paste0("Analyses/Maps/Tmax_", Year[i], "stack.tif"),
              format="GTiff", bylayer=F, overwrite=T)
  writeRaster(Tmin.pred.stack,
              filename=paste0("Analyses/Maps/Tmin_", Year[i], "stack.tif"),
              format="GTiff", bylayer=F, overwrite=T)
  writeRaster(Tavg.pred.stack,
              filename=paste0("Analyses/Maps/Tavg_", Year[i], "stack.tif"),
              format="GTiff", bylayer=F, overwrite=T)
  writeRaster(DTR.pred.stack,
              filename=paste0("Analyses/Maps/DTR_", Year[i], "stack.tif"),
              format="GTiff", bylayer=F, overwrite=T)
  
  # ## Do some plotting
  # fixbreaks<-fixstackcolors(Tmax.pred.stack,    # fix color scale for monthly raster stacks
  #                           Tmin.pred.stack, 15) 
  # 
  # par(mfrow=c(4,3))
  # plot(Tmax.pred.stack, breaks=fixbreaks, col=my.colors(15))
  # plot(Tmin.pred.stack, breaks=fixbreaks, col=my.colors(15))
  # plot(Tavg.pred.stack, breaks=fixbreaks, col=my.colors(15))
  # 
  # plot(DTR.pred.stack, breaks=fixstackcolors(DTR.pred.stack, DTR.pred.stack, 15),
  #      col=my.colors(15))
  
  range(cellStats(Tmax.pred.stack, max), cellStats(Tmin.pred.stack, min))
  
  ## And print them as pngs for easy viewing:
  for (j in 1:length(Month)){
    png(file=paste0("Analyses/Maps/Tmax_", Year[i], sprintf("%02d", Month[j]), ".png"), bg="transparent")
    plot(Tmax.pred.stack[[j]], main=paste("Tmax", Year[i], month.abb[j]), zlim=c(0,32)) # fix col ramp
    dev.off()
    
    png(file=paste0("Analyses/Maps/Tmin_", Year[i], sprintf("%02d", Month[j]), ".png"), bg="transparent")
    plot(Tmin.pred.stack[[j]], main=paste("Tmin", Year[i], month.abb[j]), zlim=c(0,32)) # fix col ramp
    dev.off()
    
    png(file=paste0("Analyses/Maps/Tavg_", Year[i], sprintf("%02d", Month[j]), ".png"), bg="transparent")
    plot(Tavg.pred.stack[[j]], main=paste("Tavg", Year[i], month.abb[j]), zlim=c(0,32)) # fix col ramp   
    dev.off()
    
    png(file=paste0("Analyses/Maps/DTR_", Year[i], sprintf("%02d", Month[j]), ".png"), bg="transparent")
    plot(DTR.pred.stack[[j]], main=paste("DTR", Year[i], month.abb[j]), zlim=c(2,16))
    dev.off()
  }
}


# pred vs obs by monthyear: Tmax
par(mfrow=c(4,3))
for (i in 1:length(Year)){
  for (j in 1:length(Month)){
    dat<-m06_17x[m06_17x$Year==Year[i] & m06_17x$Month==Month[j],] 
    pred.max<-predict(smlr.mod.m06_17$Tmax[[i]][[j]], dat)
    # pred.min<-predict(smlr.mod.m06_17$Tmin[[i]][[j]], dat)
    # pred.avg<-predict(smlr.mod.m06_17$Tavg[[i]][[j]], dat)
    # pred.dtr<-predict(smlr.mod.m06_17$DTR[[i]][[j]], dat)
    plot(dat$Tmax~pred.max, col="red", main=paste(Year[i], sprintf("%02d", Month[j])))
    abline(0,1)
  }
}


# # pred vs obs by station   # this gets complicated for multiple years...
# Ids<-unique(m06_17x$ID)
# pred.max<-numeric(length=12)
# pred.min<-numeric(length=12)
# pred.avg<-numeric(length=12)
# pred.dtr<-numeric(length=12)
# 
# par(mfrow=c(2,2))
# for (i in 1:length(Ids)){
#   stn<-m06_17x[m06_17x$ID==Ids[i],]
#   stn<-merge(data.frame(MonthYear=moyr), stn, all.x=T)
#   for (j in 1:12){
#     if(dim(dat)!=0){
#     dat<-stn[stn$Month==Month[j],]
#       pred.max[j]<-predict(smlr.mod.m06_17$Tmax[[j]],dat)
#       pred.min[j]<-predict(smlr.mod.m06_17$Tmin[[j]],dat)
#       pred.avg[j]<-predict(smlr.mod.m06_17$Tavg[[j]],dat)
#       pred.dtr[j]<-predict(smlr.mod.m06_17$DTR[[j]],dat)
#     }else{
#       pred.max[j]<-NA
#       pred.min[j]<-NA
#       pred.avg[j]<-NA
#       pred.dtr[j]<-NA
#     }
#   }
#   plot(stn$Tmax~pred.max, main=Ids[i], col="red", xlab="Predicted")
#   abline(0,1)
#   plot(stn$Tmin~pred.min, sub="Tmin", col="blue", xlab="Predicted")
#   abline(0,1)
#   plot(stn$Tavg~pred.avg, sub="Tavg", col="black", xlab="Predicted")
#   abline(0,1)
#   plot(stn$DTR~pred.dtr, sub="DTR", col="dark gray", xlab="Predicted")
#   abline(0,1)
# } 


