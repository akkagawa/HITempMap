##################################################################
## Analysis 3: Spatial Patterns
## Temperature variables vs 
##     topography: elevation, lat/long, TPI, Coastal proximity, 
##     land surface: albedo, LAI, 
##     climate: MAP, Cloud freq, wind speed

## Use station month year data and station annual data
## Focus on RECENT PERIOD for best correlation with land surface/climate datasets
## Use complete years; however some stations have 1 year while others have >10 yrs

# Analyze station mean annual AND mean month for (2006-2017 period)
# Make scatterplot matrix of station mean annual Tmax, Tmin, Tavg, and DTR versus all covariates

setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping")

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
length(unique(annual$Station))
# Now only 81 stations!

## Calculate station mean (for all years available)
Tmax<-tapply(annual$Tmax, annual$Station, mean)
Tmin<-tapply(annual$Tmin, annual$Station, mean)
Tavg<-tapply(annual$Tavg, annual$Station, mean)
DTR<-tapply(annual$DTR, annual$Station, mean)
yrs<-tapply(annual$Tavg, annual$Station, length)

## Organize response and predictor variables into a master dataframe
ys<-data.frame(Tmax, Tmin, Tavg, DTR, yrs, ID=names(Tmax))
xs<-read.csv("Analyses/SpatialCovar.csv")

xy<-merge(ys, xs, by="ID", all=F)  # merge yvals and xvals
xyt<-xy[,-c(7:16)]          # remove unnecessary columns

## Identify the extra monthly rainfall and cloud frequency and hourly wind values
excl<-which(grepl("staterf", names(xyt))|  # drop monthly, hourly series
  grepl("cl_frq", names(xyt))| 
  grepl("wind", names(xyt)))

incl<-c(grep("staterf_mmann",names(xyt)),  # keep annual
        grep("cl_frq_ann",names(xyt)),
        grep("wind_sp_ms",names(xyt)))

xyty<-xyt[,-excl]
xyty<-cbind(xyty, xyt[,incl])

## Create separate datasets for each response variable
Tmax.xy<-xyty[, -c(3:5)]
Tmin.xy<-xyty[, -c(2,4:5)]
Tavg.xy<-xyty[, -c(2:3,5)]
DTR.xy<-xyty[, -c(2:4)]


#### Exploratory: Create scatterplot matrices ############
#### Station mean annual Tmax, Tmin, Tavg, and DTR versus all covariates

## Scatterplot matrix: topography and coastal proximity
plot(Tmax.xy[,c(2,4:12)], col="red", pch="*")
plot(Tmin.xy[,c(2,4:12)], col="blue", pch="*")
plot(Tavg.xy[,c(2,4:12)], col="gray", pch="*")
plot(DTR.xy[,c(2,4:12)], col="forest green", pch="*")

## Scatterplot matrix: land cover variables
plot(Tmax.xy[,c(2,13:18)], col="red", pch="*")
plot(Tmin.xy[,c(2,13:18)], col="blue", pch="*")
plot(Tavg.xy[,c(2,13:18)], col="gray", pch="*")
plot(DTR.xy[,c(2,13:18)], col="forest green", pch="*")

## Scatterplot matrix: climatic variables
plot(Tmax.xy[,c(2,19:21)], col="red", pch="*")
plot(Tmin.xy[,c(2,19:21)], col="blue", pch="*")
plot(Tavg.xy[,c(2,19:21)], col="gray", pch="*")
plot(DTR.xy[,c(2,19:21)], col="forest green", pch="*")

#### Exploratory: Try out PCA ####################################################
## Try using principal components to tease apart some variables?
prcovars<-prcomp(~ CorrElev_m +
                 CorrLat_DD +
                 CorrLong_DD +
                 tpi100_200c +   # revise TPI?
                 cpi_wgs84 +
                 albedo_ann +
                 #ndvi_ann +
                 #evi_ann +
                 lai_ann +
                 #veg_ht_ann +
                 #fr_v_c_ann +
                 staterf_mmann +
                 cl_frq_ann +
                 wind_sp_ms, 
               data= xyty[,-1], 
               scale=TRUE,
               center=TRUE)
summary(prcovars)  #PC1 explains 36% of the variance; PC2: 21%; 5PCs cumulative: 87% 
prcovars           #PC1 dominated by elevational coeffs
                   #PC2 dominated by lat/long/windsp
                   #PC3 dominated by rf, topographic position, lai
                   #PC4 CPI, albedo
                   #PC5 wind speed


######## Modeling ##################################################################
## Fit SIMPLE LINEAR REGRESSION on elevation only
m_z.Tmax<-lm(Tmax.x~CorrElev_m, data=xyty)
m_z.Tmin<-lm(Tmin.x~CorrElev_m, data=xyty)
m_z.Tavg<-lm(Tavg.x~CorrElev_m, data=xyty)
m_z.DTR<-lm(DTR.x~CorrElev_m, data=xyty)

par(mfrow=c(4,5))
  ## Evaluate
  plot(m_z.Tmax, main="")
  plot(dffits(m_z.Tmax)~xyty$CorrElev_m)
  summary(m_z.Tmax) 
  
  plot(m_z.Tmin, main="")
  plot(dffits(m_z.Tmin)~xyty$CorrElev_m)  
  ## NOT homoscedastic; high elev also high leverage
  summary(m_z.Tmin)
  
  plot(m_z.Tavg, main="")
  plot(dffits(m_z.Tavg)~xyty$CorrElev_m)
  summary(m_z.Tavg)
  
  plot(m_z.DTR, main="")
  plot(dffits(m_z.DTR)~xyty$CorrElev_m)
  summary(m_z.DTR)  # R2>90% for Tmax, Tmin, Tavg and <5% for DTR
  
## ------------------------------------------------------------------
## Fit MULTIPLE regression: elevation and annual rainfall
m_zp.Tmax<-lm(Tmax.x~CorrElev_m*staterf_mmann, data=xyty)
summary(m_zp.Tmax)  # no rf effect, no significant elev*rf interaction
plot(m_zp.Tmax)
plot(dffits(m_zp.Tmax)~xyty$CorrElev_m)

m_zp.Tmin<-lm(Tmin.x~CorrElev_m*staterf_mmann, data=xyty)
summary(m_zp.Tmin)  # small rf effect, no significant elev*rf interaction
plot(m_zp.Tmin)
plot(dffits(m_zp.Tmin)~xyty$CorrElev_m)  # STILL high elevation issue

m_zp.Tavg<-lm(Tavg.x~CorrElev_m*staterf_mmann, data=xyty)
summary(m_zp.Tavg)  # significant rf effect, no significant elev*rf interaction 
plot(m_zp.Tavg)
plot(dffits(m_zp.Tavg)~xyty$CorrElev_m)

m_zp.DTR<-lm(DTR.x~CorrElev_m*staterf_mmann, data=xyty)
summary(m_zp.DTR)   # no rf effect (elev*rf interaction is then meaningless)
plot(m_zp.DTR)
plot(dffits(m_zp.DTR)~xyty$CorrElev_m)

## ------------------------------------------------------------------   
## Fit SEGMENTED regression, FIXED breakpoint with elevation
library(segmented)  #add capacity for (floating breakpoint) segmented regression
library(MuMIn)      #add capacity to calculate BIC and AICc

s_z.Tmax<-segmented(m_z.Tmax, seg.Z=~CorrElev_m, psi=2150, control=seg.control(it.max=0))
summary(s_z.Tmax)
plot(s_z.Tmax); plot(dffits(s_z.Tmax)~xyty$CorrElev_m)
   # segmented term not significant at alpha=0.05

s_z.Tmin<-segmented(m_z.Tmin, seg.Z=~CorrElev_m, psi=2150, control=seg.control(it.max=0))
summary(s_z.Tmin)
plot(s_z.Tmin); plot(dffits(s_z.Tmin)~xyty$CorrElev_m)
   # segmented term HIGHLY significant

s_z.Tavg<-segmented(m_z.Tavg, seg.Z=~CorrElev_m, psi=2150, control=seg.control(it.max=0))
summary(s_z.Tavg)
plot(s_z.Tavg); plot(dffits(s_z.Tavg)~xyty$CorrElev_m)
   # segmented term HIGHLY significant

s_z.DTR<-segmented(m_z.DTR, seg.Z=~CorrElev_m, psi=2150, control=seg.control(it.max=0))
summary(s_z.DTR)
plot(s_z.DTR); plot(dffits(s_z.DTR)~xyty$CorrElev_m)
   # segmented term MARGINALLY significant; higher DTR larger std residuals

## ------------------------------------------------------------------
## Fit SEGMENTED regression, 2 FLOATING breakpoints with elevation
par(mfrow=c(1,1))
s_z2f.Tmax<-segmented(m_z.Tmax, seg.Z=~CorrElev_m, psi=c(1500, 2200)) 
summary(s_z2f.Tmax)
plot(s_z2f.Tmax)
   # no convergence or breaks at strange loc

s_z2f.Tmin<-segmented(m_z.Tmin, seg.Z=~CorrElev_m, psi=c(1500, 2200)) 
summary(s_z2f.Tmin)
plot(s_z2f.Tmin)
   # breaks ~1700 and 2500m

s_z2f.Tavg<-segmented(m_z.Tavg, seg.Z=~CorrElev_m, psi=c(1500, 2200)) #breaks at strange loc
summary(s_z2f.Tavg)
plot(s_z2f.Tavg)
   # no convergence or breaks at strange loc

## ------------------------------------------------------------------
## Fit SEGMENTED regression, FIXED breakpoint with RF term 
par(mfrow=c(3,5))
sm_zp.Tmax<-segmented(m_zp.Tmax, seg.Z=~CorrElev_m, psi=2150, control=seg.control(it.max=0))
summary(sm_zp.Tmax)
plot(sm_zp.Tmax); plot(dffits(sm_zp.Tmax)~xyty$CorrElev_m)

sm_zp.Tmin<-segmented(m_zp.Tmin, seg.Z=~CorrElev_m, psi=2150, control=seg.control(it.max=0))
summary(sm_zp.Tmin)
plot(sm_zp.Tmin); plot(dffits(sm_zp.Tmin)~xyty$CorrElev_m)

sm_zp.Tavg<-segmented(m_zp.Tavg, seg.Z=~CorrElev_m, psi=2150, control=seg.control(it.max=0))
summary(sm_zp.Tavg)
plot(sm_zp.Tavg); plot(dffits(sm_zp.Tavg)~xyty$CorrElev_m)

#####################################################################################
######### Multiple-model approach: linear and segmented regression ##################
## Fit MULTIPLE regressions, NO interactions. REQUIRE elevation term
global.Tmax<-lm(Tmax.x ~ CorrElev_m + 
                  CorrLat_DD + CorrLong_DD +
                  cpi_wgs84 + tpi100_200c + 
                  lai_ann + albedo_ann  + 
                  staterf_mmann + cl_frq_ann + wind_sp_ms,
                data=xyty, na.action=na.fail)
combo.Tmax<-dredge(global.Tmax, beta="none", fixed=~CorrElev_m, 
                   extra=c("R^2", "adjR^2"))
combo.Tmax  ## Top vbles: elevation, staterf,  cpi and cloud frequency
mods.Tmax<-get.models(combo.Tmax, subset=TRUE)

global.Tmin<-lm(Tmin.x ~ CorrElev_m + 
                  CorrLat_DD + CorrLong_DD +
                  cpi_wgs84 + tpi100_200c + 
                  lai_ann + albedo_ann  + 
                  staterf_mmann + cl_frq_ann + wind_sp_ms,
                data=xyty, na.action=na.fail)
combo.Tmin<-dredge(global.Tmin, beta="none", fixed=~CorrElev_m, 
                   extra=c("R^2", "adjR^2"))
combo.Tmin  ## Top vbles: elevation, cpi, staterf and wind speed
mods.Tmin<-get.models(combo.Tmin, subset=TRUE)

global.Tavg<-lm(Tavg.x ~ CorrElev_m + 
                  CorrLat_DD + CorrLong_DD +
                  cpi_wgs84 + tpi100_200c + 
                  lai_ann + albedo_ann  + 
                  staterf_mmann + cl_frq_ann + wind_sp_ms,
                data=xyty, na.action=na.fail)
combo.Tavg<-dredge(global.Tavg, beta="none", fixed=~CorrElev_m, 
                   extra=c("R^2", "adjR^2"))
combo.Tavg  ## Top vbles: elevation, staterf, wind speed
mods.Tavg<-get.models(combo.Tavg, subset=TRUE)

global.DTR<-lm(DTR.x ~ CorrElev_m + 
                 CorrLat_DD + CorrLong_DD +
                 cpi_wgs84 + tpi100_200c + 
                 lai_ann + albedo_ann  + 
                 staterf_mmann + cl_frq_ann + wind_sp_ms,
               data=xyty, na.action=na.fail)
combo.DTR<-dredge(global.DTR, beta="none", fixed=~CorrElev_m, 
                  extra=c("R^2", "adjR^2"))  
combo.DTR    ## Top vbles: cpi, cloud frequency, wind speed, and elevation
mods.DTR<-get.models(combo.DTR, subset=TRUE)


###########  Now account for segmented elevation term in MULTIPLE regressions; 
###########  Set single breakpoint @ 2150m

## Segmented regression version for each multiple regression model fitted earlier
smr1.Tmax<-lapply(mods.Tmax, FUN=segmented,
                  seg.Z=~CorrElev_m, psi=2150, control=seg.control(it.max=0))
smr1.Tmin<-lapply(mods.Tmin, FUN=segmented,
                  seg.Z=~CorrElev_m, psi=2150, control=seg.control(it.max=0))
smr1.Tavg<-lapply(mods.Tavg, FUN=segmented,
                  seg.Z=~CorrElev_m, psi=2150, control=seg.control(it.max=0))
smr1.DTR<-lapply(mods.DTR, FUN=segmented,
                 seg.Z=~CorrElev_m, psi=2150, control=seg.control(it.max=0))

Tmax.seg2150<-model.sel(smr1.Tmax, rank=AICc, beta="none")
Tmin.seg2150<-model.sel(smr1.Tmin, rank=AICc, beta="none")
Tavg.seg2150<-model.sel(smr1.Tavg, rank=AICc, beta="none")
DTR.seg2150<-model.sel(smr1.DTR, rank=AICc, beta="none")

R2.smr1.Tmax<-unlist(lapply(smr1.Tmax, function(x) summary(x)$r.squared))
adjR2.smr1.Tmax<-unlist(lapply(smr1.Tmax, function(x) summary(x)$adj.r.squared))
Tmax.seg2150.fin<-merge(data.frame(Tmax.seg2150, mods=row.names(Tmax.seg2150)), 
                         data.frame(R2=R2.smr1.Tmax, adjR2=adjR2.smr1.Tmax, 
                                    mods=names(R2.smr1.Tmax)), 
                         by="mods", all.x=T, sort=F)

R2.smr1.Tmin<-unlist(lapply(smr1.Tmin, function(x) summary(x)$r.squared))
adjR2.smr1.Tmin<-unlist(lapply(smr1.Tmin, function(x) summary(x)$adj.r.squared))
Tmin.seg2150.fin<-merge(data.frame(Tmin.seg2150, mods=row.names(Tmin.seg2150)), 
                         data.frame(R2=R2.smr1.Tmin, adjR2=adjR2.smr1.Tmin, 
                                    mods=names(R2.smr1.Tmin)), 
                         by="mods", all.x=T, sort=F)

R2.smr1.Tavg<-unlist(lapply(smr1.Tavg, function(x) summary(x)$r.squared))
adjR2.smr1.Tavg<-unlist(lapply(smr1.Tavg, function(x) summary(x)$adj.r.squared))
Tavg.seg2150.fin<-merge(data.frame(Tavg.seg2150, mods=row.names(Tavg.seg2150)), 
                         data.frame(R2=R2.smr1.Tavg, adjR2=adjR2.smr1.Tavg, 
                                    mods=names(R2.smr1.Tavg)), 
                         by="mods", all.x=T, sort=F)

R2.smr1.DTR<-unlist(lapply(smr1.DTR, function(x) summary(x)$r.squared))
adjR2.smr1.DTR<-unlist(lapply(smr1.DTR, function(x) summary(x)$adj.r.squared))
DTR.seg2150.fin<-merge(data.frame(DTR.seg2150, mods=row.names(DTR.seg2150)), 
                         data.frame(R2=R2.smr1.DTR, adjR2=adjR2.smr1.DTR, 
                                    mods=names(R2.smr1.DTR)), 
                         by="mods", all.x=T, sort=F)

######################## Compare all models ################################
#dir.create("Analyses/Spatial_2006_2017_fin") # move into Analyses/Spatial directory

## multiple regression (includes univariate case)
write.csv(data.frame(combo.Tmax), file="Analyses/Spatial_2006_2017_fin2/combo.Tmax.z.csv")  # multiple regression global set with AICc
write.csv(data.frame(combo.Tmin), file="Analyses/Spatial_2006_2017_fin2/combo.Tmin.z.csv")
write.csv(data.frame(combo.Tavg), file="Analyses/Spatial_2006_2017_fin2/combo.Tavg.z.csv")
write.csv(data.frame(combo.DTR),  file="Analyses/Spatial_2006_2017_fin2/combo.DTR.z.csv")

## fixed node segmented regression (includes univariate model)
write.csv(Tmax.seg2150.fin, file="Analyses/Spatial_2006_2017_fin2/Tmax.seg2150.csv") 
write.csv(Tmin.seg2150.fin, file="Analyses/Spatial_2006_2017_fin2/Tmin.seg2150.csv")
write.csv(Tavg.seg2150.fin, file="Analyses/Spatial_2006_2017_fin2/Tavg.seg2150.csv")
write.csv(DTR.seg2150.fin, file="Analyses/Spatial_2006_2017_fin2/DTR.seg2150.csv") # fixed node


# ###########  Now account for segmented elevation term; 2 floating breakpoints
# # Does not work- difficulty finding a reasonable breakpoint
# 
# smr2f.Tmax<-lapply(mods.Tmax, FUN=segmented,
#                    seg.Z=~CorrElev_m, psi=c(100, 2500), 
#                    control=seg.control(it.max=20, maxit.glm=50, h=0.5, seed=0))
# smr2f.Tmin<-lapply(mods.Tmin, FUN=segmented,
#                    seg.Z=~CorrElev_m, psi=c(100, 2500), 
#                    control=seg.control(it.max=20, maxit.glm=50, h=0.5, seed=0))
# smr2f.Tavg<-lapply(mods.Tavg, FUN=segmented,
#                    seg.Z=~CorrElev_m, psi=c(100, 2500), 
#                    control=seg.control(it.max=20, maxit.glm=50, h=0.5, seed=0))
# smr2f.DTR<-lapply(mods.DTR, FUN=segmented,
#                    seg.Z=~CorrElev_m, psi=c(100, 2500), 
#                    control=seg.control(it.max=20, maxit.glm=50, h=0.5, seed=0))
# 
# 
# Tmax.segflex<-cbind(as.data.frame(sapply(smr2f.Tmax, coefficients)), 
#                     psi=as.data.frame(lapply(smr2f.Tmax, function(x) x$psi[3:4])),
#                     AICc=as.numeric(AICc.smr2f.Tmax))
# Tmin.segflex<-cbind(as.data.frame(sapply(smr2f.Tmin, coefficients)),
#                     psi=as.data.frame(lapply(smr2f.Tmin, function(x) x$psi[3:4])),
#                     AICc=as.numeric(AICc.smr2f.Tmin))
# Tavg.segflex<-cbind(as.data.frame(sapply(smr2f.Tavg, coefficients)),
#                     psi=as.data.frame(lapply(smr2f.Tavg, function(x) x$psi[3:4])),
#                     AICc=as.numeric(AICc.smr2f.Tavg))
# 
# ## floating node segmented regression
# write.csv(Tmax.segflex, file="Analyses/Spatial_2006_2017/Tmax.segflex.csv") 
# write.csv(Tmin.segflex, file="Analyses/Spatial_2006_2017/Tmin.segflex.csv")
# write.csv(Tavg.segflex, file="Analyses/Spatial_2006_2017/Tavg.segflex.csv")

######################## Compare all models ################################
#####################################################################################
head(combo.Tmax); head(Tmax.seg2150.fin)
head(combo.Tmin); head(Tmin.seg2150.fin)
head(combo.Tavg); head(Tavg.seg2150.fin)
head(combo.DTR); head(DTR.seg2150.fin)
## Examine output in Excel and decide best models for each temperature variable.
##  Old-ish notes:
##  Tmax: cl_frq_ann(-), CorrElev_m(-), cpi_wgs84(+), lai_ann(-, drop), staterf_mmann(-), wind_sp_ms(-, drop)
##  Tmin: albedo_ann(-, drop), CorrElev_m(-), cpi_wgs84(-), lai_ann(-, soso, drop), staterf_mmann(-), wind_sp_ms(+)
##  Tavg: albedo_ann(-, drop), cl_frq_ann(-), CorrElev_m(-), lai_ann(-), staterf_mmann(-)
##  DTR: cl_frq_ann(-), CorrElev_m(-), cpi_wgs84(+), rainfall(-, soso, dropped in 06-15), wind_sp_ms(-)
##  ** looking at AIC, segmented performs better
##  ** note coastal proximity is actually distance FROM the coast
##  conclusions depend on base period of measurements! 
##     should focus these on recent period since veg change relevant...
#####################################################################################

summary(mods.Tmax$"1") # regression on elevation
summary(smr1.Tmax$"1") # segmented, elevation
summary(mods.Tmax$"83") # without lat/long
summary(smr1.Tmax$"83") # top model


summary(mods.Tmin$"1")
summary(smr1.Tmin$"1")
summary(mods.Tmin$"337") # without lat/long
summary(smr1.Tmin$"305") # top model

summary(mods.Tavg$"1")
summary(smr1.Tavg$"1")
summary(mods.Tavg$"323") # without lat/long
summary(smr1.Tavg$"323") # top model

summary(mods.DTR$"1")
summary(smr1.DTR$"1")
summary(mods.DTR$"275") # without lat/long
summary(smr1.DTR$"275") # top model

