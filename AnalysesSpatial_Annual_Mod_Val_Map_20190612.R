## AnalysesSpatial_Annual_Mod_Val_Map_20190612.R
## 6/10/2019
## A. Kagawa-Viviani

##################################################################
## Analysis 3: Spatial Patterns
## Temperature variables vs 
##     topography: elevation, lat/long, TPI, Coastal proximity, 
##     land surface: albedo, LAI, other vegetation indices
##     climate: MAP, Cloud freq, wind speed

## Use station month year data and station annual data
## Focus on RECENT PERIOD for best correlation with land surface/climate datasets
## Use complete years; however some stations have 1 year while others have >10 yrs
# Analyze station mean annual AND mean month for (2006-2017 period)
# Make scatterplot matrix of station mean annual Tmax, Tmin, Tavg, and DTR versus all covariates

# Select best model for annual station-years
# Perform validation
# Predict maps
##################################################################

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

xyt<-merge(ys, xs[,-c(2:11)], by="ID", all=F)  # merge yvals and xvals (minus unnecessary columns)

## Identify the extra monthly rainfall and cloud frequency and hourly wind values
excl<-which(grepl("staterf", names(xyt))|  # drop monthly, hourly series
              grepl("cl_frq", names(xyt))| 
              grepl("wind", names(xyt)))

incl<-c(grep("staterf_mmann",names(xyt)),  # keep annual
        grep("cl_frq_ann",names(xyt)),
        grep("wind_sp_ms",names(xyt)))

xyty<-xyt[,-excl]
xyty<-cbind(xyty, xyt[,incl])

# Include this term to simulate segmented regression with FIXED breakpoint at 2150 m
xyty$U1.CorrElev_m<-(xyty$CorrElev_m-2150)*(xyty$CorrElev_m >2150)

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

## Look at correlation matrix of covariates
cov.cor<-cor(Tmax.xy[,-c(1:3)])
write.csv(data.frame(cov.cor), file="Analyses/Spatial_2006_2017_fin2019/CorrelationMatrix.csv")  # multiple regression global set with AICc

## Look at in Excel. 
##  1) Remove > 0.9 correlations: redundant elevation covariates, redundant TPI, EVI, Long
##  2) Remove fr_v_c, NDVI (corr with LAI); Elevation is strongly correlated with TPI, CPI

## Multicollinearity
# https://statisticalhorizons.com/multicollinearity
# http://www.sthda.com/english/articles/39-regression-model-diagnostics/160-multicollinearity-essentials-and-vif-in-r/


library(MuMIn)      #add capacity to calculate BIC and AICc; dredge() for global combinations

#####################################################################################
######### Multiple-model approach: linear and segmented regression ##################
## Fit MULTIPLE regressions, NO interactions. REQUIRE elevation term
global.Tmax<-lm(Tmax ~ CorrElev_m + U1.CorrElev_m +
                  CorrLat_DD + 
                  cpi_wgs84 + tpi100_200c + 
                  lai_ann + albedo_ann  + veg_ht_ann +
                  staterf_mmann + cl_frq_ann + wind_sp_ms,
                data=xyty, na.action=na.fail)
combo.Tmax<-dredge(global.Tmax, beta="none", fixed=~CorrElev_m, 
                   extra=c("R^2", "adjR^2"))
combo.Tmax  ## Top vbles: elevation, staterf,  cpi and cloud frequency
mods.Tmax<-get.models(combo.Tmax, subset=TRUE)

global.Tmin<-lm(Tmin ~ CorrElev_m + U1.CorrElev_m +
                  CorrLat_DD + 
                  cpi_wgs84 + tpi100_200c + 
                  lai_ann + albedo_ann  + veg_ht_ann +
                  staterf_mmann + cl_frq_ann + wind_sp_ms,
                data=xyty, na.action=na.fail)
combo.Tmin<-dredge(global.Tmin, beta="none", fixed=~CorrElev_m, 
                   extra=c("R^2", "adjR^2"))
combo.Tmin  ## Top vbles: elevation, cpi, staterf and wind speed
mods.Tmin<-get.models(combo.Tmin, subset=TRUE)

global.Tavg<-lm(Tavg ~ CorrElev_m + U1.CorrElev_m +
                  CorrLat_DD + 
                  cpi_wgs84 + tpi100_200c + 
                  lai_ann + albedo_ann  + veg_ht_ann +
                  staterf_mmann + cl_frq_ann + wind_sp_ms,
                data=xyty, na.action=na.fail)
combo.Tavg<-dredge(global.Tavg, beta="none", fixed=~CorrElev_m, 
                   extra=c("R^2", "adjR^2"))
combo.Tavg  ## Top vbles: elevation, staterf, wind speed
mods.Tavg<-get.models(combo.Tavg, subset=TRUE)

global.DTR<-lm(DTR ~ CorrElev_m + U1.CorrElev_m +
                 CorrLat_DD + 
                 cpi_wgs84 + tpi100_200c + 
                 lai_ann + albedo_ann  + veg_ht_ann +
                 staterf_mmann + cl_frq_ann + wind_sp_ms,
               data=xyty, na.action=na.fail)
combo.DTR<-dredge(global.DTR, beta="none", # fixed=~CorrElev_m, 
                  extra=c("R^2", "adjR^2"))  
combo.DTR    ## Top vbles: cpi, cloud frequency, wind speed, and elevation
mods.DTR<-get.models(combo.DTR, subset=TRUE)

######################## Compare all models ################################
dir.create("Analyses/Spatial_2006_2017_fin2019") # move into Analyses/Spatial directory

## multiple regression (includes univariate case)
write.csv(data.frame(combo.Tmax), file="Analyses/Spatial_2006_2017_fin2019/combo.Tmax.z.csv")  # multiple regression global set with AICc
write.csv(data.frame(combo.Tmin), file="Analyses/Spatial_2006_2017_fin2019/combo.Tmin.z.csv")
write.csv(data.frame(combo.Tavg), file="Analyses/Spatial_2006_2017_fin2019/combo.Tavg.z.csv")
write.csv(data.frame(combo.DTR),  file="Analyses/Spatial_2006_2017_fin2019/combo.DTR.z.csv")


######################## Compare all models ################################
#####################################################################################
head(combo.Tmax)
head(row.names(combo.Tmax)) # "363" "107" "235" "491" "111" "43"

head(combo.Tmin) 
head(row.names(combo.Tmin)) # "665" "669" "666" "697" "921" "729"

head(combo.Tavg) 
head(row.names(combo.Tavg)) # "692" "691" "948" "675" "947" "690"

head(combo.DTR)  
head(row.names(combo.DTR)) # "1427" "1303" "1459" "1491" "1311" "1431"


# Identify the models used.  Look at variance inflation factors
car::vif(mods.Tmax[[1]])  # VIF>5 for CorrElev_m (CPI, TPI included in model)
car::vif(mods.Tmax[[2]])  # VIF>5 for CorrElev_m (CPI, TPI included in model) 
car::vif(mods.Tmax[[3]])  # VIF>5 for CorrElev_m (CPI, TPI included in model)
car::vif(mods.Tmax[[4]])  # VIF>10 for CorrElev_m (CPI, TPI included in model)
car::vif(mods.Tmax[[5]])  # VIF>5 CorrElev_m (CPI, TPI included in model)
car::vif(mods.Tmax[[6]])  # cl_frq_ann, cpi_wgs84, staterf_mmann, CorrElev_m 
## likely issues due to strong multicollinearity of TPI, CPI and Elev

combo.Tmax[1:6,]
summary(mods.Tmax[[6]]) # Best model considering multicollinearity issues
combo.Tmax[6,]
summary(mods.Tmax$'129')# segmented regression
summary(mods.Tmax$'1')  # regression on elevation
combo.Tmax[c("129", "1"),]

### Tmin
car::vif(mods.Tmin[[1]])  # VIF all < 5
car::vif(mods.Tmin[[2]])  # VIF all < 5
car::vif(mods.Tmin[[3]])  # VIF all < 5
car::vif(mods.Tmin[[4]])  # VIF>5 for CorrElev_m (CPI included in model)
car::vif(mods.Tmin[[5]])  # VIF all < 5
car::vif(mods.Tmin[[6]])  # VIF>10 for CorrElev_m (CPI, TPI in model) 
## likely issues due to strong multicollinearity of TPI, CPI and Elev

combo.Tmin[1:6,]
summary(mods.Tmin[[1]]) # Best model considering multicollinearity issues
combo.Tmin[1,]
summary(mods.Tmin$'129')# segmented regression
summary(mods.Tmin$'1')  # regression on elevation
combo.Tmin[c("129", "1"),]


### Tavg
car::vif(mods.Tavg[[1]])  # VIF all < 5
car::vif(mods.Tavg[[2]])  # VIF all < 5
car::vif(mods.Tavg[[3]])  # VIF all < 5
car::vif(mods.Tavg[[4]])  # VIF all < 5
car::vif(mods.Tavg[[5]])  # VIF all < 5
car::vif(mods.Tavg[[6]])  # VIF all < 5

combo.Tavg[1:20,]
summary(mods.Tavg[[4]]) # Best model considering multicollinearity issues (LAI marg signif)
combo.Tavg[4,]
summary(mods.Tavg$'129')# segmented regression
summary(mods.Tavg$'1')  # regression on elevation
combo.Tavg[c("129", "1"),]

### DTR
car::vif(mods.DTR[[1]])  # VIF all < 5
car::vif(mods.DTR[[2]])  # VIF all < 5 ** CorrElev VIF ~ 4.27, CPI in model
car::vif(mods.DTR[[3]])  # VIF all < 5
car::vif(mods.DTR[[4]])  # VIF all < 5
car::vif(mods.DTR[[5]])  # VIF all < 5 ** CorrElev VIF ~ 4.3, CPI in model
car::vif(mods.DTR[[6]])  # VIF all > 10 for CorrElev (CPI and TPI in model)

combo.DTR[1:6,]

summary(mods.DTR[[1]]) # Best model considering multicollinearity issues
combo.DTR[1,]
summary(mods.DTR$'261')# segmented regression
combo.DTR["261",]
summary(mods.DTR$'5')  # regression on elevation
combo.DTR["5",]

########################################################################
## K-fold cross validation
# See http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/

library(caret)

# Best Tmax model
tmax.xyty<-as.matrix(xyty[,c("Tmax", "CorrElev_m", 
                             "staterf_mmann", "cpi_wgs84", "cl_frq_ann")])

# Best Tmin model 
tminseg.xyty<-as.matrix(xyty[,c("Tmin", "CorrElev_m","U1.CorrElev_m",
                                "cpi_wgs84", "wind_sp_ms", "lai_ann")])

# Best Tavg model cl_frq_ann     CrE_m    stt_mmn U1.CrE_m wnd_sp_ms
tavgseg.xyty<-as.matrix(xyty[,c("Tavg", "CorrElev_m","U1.CorrElev_m",
                                "staterf_mmann", "cl_frq_ann", "wind_sp_ms")])

# Best DTR model cl_frq_ann cpi_w84   t10_200  U1.CrE_m wnd_sp_ms
dtr.xyty<-as.matrix(xyty[,c("DTR", "cl_frq_ann","cpi_wgs84", "U1.CorrElev_m",
                            "tpi100_200c", "wind_sp_ms")])


set.seed(123)
# define training control
train_control <- trainControl(method="cv", number=5)

# train the model
tmax.model <- train(Tmax~., data=tmax.xyty, trControl=train_control, method="lm")
tminseg.model <- train(Tmin~., data=tminseg.xyty, trControl=train_control, method="lm")
tavgseg.model <- train(Tavg~., data=tavgseg.xyty, trControl=train_control, method="lm")
dtr.model <- train(DTR~., data=dtr.xyty, trControl=train_control, method="lm")

# Summarize the results
print(tmax.model)
print(tminseg.model)
print(tavgseg.model)
print(dtr.model)

tmax.pred<-mods.Tmax[[6]]$fitted.values
tmin.pred<-mods.Tmin[[1]]$fitted.values
tavg.pred<-mods.Tavg[[4]]$fitted.values
dtr.pred<-mods.DTR[[1]]$fitted.values

# train the model
tmax.lm <- train(Tmax~., data=tmax.xyty[,1:2], trControl=train_control, method="lm")
tmin.lm <- train(Tmin~., data=tminseg.xyty[,1:2], trControl=train_control, method="lm")
tavg.lm <- train(Tavg~., data=tavgseg.xyty[,1:2], trControl=train_control, method="lm")
dtr.lm <- train(DTR~., data=dtr.xyty[,1:2], trControl=train_control, method="lm")


########################################################################################
#################   MAKE FIGURE 2.1: Validation Stats, BEST MODELS #########################
## look at observed ~ predicted of the BEST regression models
##   Tmax: multiple linear regression
##   Tmin/Tavg/DTR: segmented multiple regression
pdf("FINAL_FiguresTables/Figure2_fin20190612.pdf", width=9, height=7, res=600)
mat=matrix(c(1,1,2,1,1,3), byrow=T, nrow=2, ncol=3)
layout(mat)
par(mar=c(1.5,1.5,0,0), oma=c(3,3,3,1), cex=1)
plot(xyty$Tmax~tmax.pred, bg="tomato", pch=24,    
     xlim=c(0,35), ylim=c(0, 35), xlab="", ylab="",
     tcl=-0.2, mgp=c(2,0.2,0), cex.axis=0.9)
points(xyty$Tmin~tmin.pred, bg="skyblue", pch=25, cex=0.75)
#points(xyty$Tavg~tavg.pred, bg="forestgreen", pch=21)
abline(0,1)

######## Legend for Tmax, Tmin
Tmax.leg<-as.expression(bquote(T[max]==f(z,CDI,RF,CF)*
                                 ", MAE="*.(round(tmax.model$results[1,4], 2))*
                                 ", RMSE="*.(round(tmax.model$results[1,2], 2))))
Tmin.leg<-as.expression(bquote(T[min]== f (z[seg],CDI,LAI,WS)*
                                 ", MAE="*.(round(tminseg.model$results[1,4], 2))*
                                 ", RMSE="*.(round(tminseg.model$results[1,2], 2)))) 
# Tavg.leg<-as.expression(bquote(T[avg]== f (z[seg],RF,CF,WS)*
#                                  ", MAE="*.(round(tavgseg.model$results[1,4], 2))*
#                                  ", RMSE="*.(round(tavgseg.model$results[1,2], 2))))
# leg<-c(Tmax.leg, Tavg.leg, Tmin.leg)
# legend("topleft", legend=leg, bty="n",
#        pt.bg=c("tomato", "forestgreen", "skyblue"),
#        pch=c(24, 21, 25))

leg<-c(Tmax.leg, Tmin.leg)
legend("topleft", legend=leg, bty="n",
       pt.bg=c("tomato", "skyblue"),cex=1,
       pch=c(24, 25))

mtext(side=3, text=expression(AIC[c]~"-selected regression models"), line=0.5, cex=1.5, outer=T)
mtext(side=1, text=expression("Predicted Temperature ("*degree*C*")"), line=1, outer=T)
mtext(side=2, text=expression("Observed Temperature ("*degree*C*")"), line=2)

#### Tavg inset2: modeled Tavg and modeled (Tmax+Tmin)/2
Tavgmaxmin<-(tmax.pred+tmin.pred)/2
Tavgmaxmin.err<-xyty$Tavg-Tavgmaxmin
Tavgmaxmin.MAE<-sum(abs(Tavgmaxmin.err))/(length(Tavgmaxmin.err)-1); print(Tavgmaxmin.MAE)
Tavgmaxmin.RMSE<-sqrt(sum(Tavgmaxmin.err^2)/(length(Tavgmaxmin.err)-1)); print(Tavgmaxmin.RMSE)

plot(xyty$Tavg~tavg.pred, pch=21, # cex=0.8,
     xlim=c(5,30), ylim=c(5,30), cex.axis=0.9, tcl=-0.2, mgp=c(2,0.2,0))
abline(0,1)
points(xyty$Tavg~Tavgmaxmin, pch=8, cex=0.5)  # see improvement from linear regression

Tavg.leg<-as.expression(bquote(T[avg]== f (z[seg],RF,CF,WS)))
Tavg.legval<-as.expression(bquote("MAE="*.(round(tavgseg.model$results[1,4], 2))*
                                 ", RMSE="*.(round(tavgseg.model$results[1,2], 2))))
Tavgmaxmin.leg1<-as.expression(bquote(T[avg]== (T[max]+T[min])/2))
Tavgmaxmin.legval<-as.expression(bquote("MAE="*.(round(Tavgmaxmin.MAE, 2))*
                                          ", RMSE="*.(round(Tavgmaxmin.RMSE, 2))))
                                
leginset<-c(Tavg.leg, Tavg.legval, Tavgmaxmin.leg1, Tavgmaxmin.legval)
legend("topleft", legend=leginset,
       pch=c(21, NA, 8, NA), pt.cex=c(0.8,0, 0.6,0),bty="n",
       cex=0.75, y.intersp=0.85)

## DTR inset2: modeled DTR and modeled Tmax-Tmin
Tmaxmin<-tmax.pred-tmin.pred
Tmaxmin.err<-xyty$DTR-Tmaxmin
Tmaxmin.MAE<-sum(abs(Tmaxmin.err))/(length(Tmaxmin.err)-1); print(Tmaxmin.MAE)
Tmaxmin.RMSE<-sqrt(sum(Tmaxmin.err^2)/(length(Tmaxmin.err)-1)); print(Tmaxmin.RMSE)

plot(xyty$DTR~dtr.pred, pch=21, cex=0.8,cex.axis=0.8,
     xlim=c(3,15), ylim=c(3, 15),
     #xlab=expression(DTR[pred]),
     #ylab=expression(DTR[obs]),
     cex.lab=0.9, mgp=c(2,0.2,0), tcl=-0.2)
abline(0,1)
points(xyty$DTR~Tmaxmin, pch=8, cex=0.5)  # see improvement from linear regression

DTR.leg1<-as.expression(bquote(DTR== f(z['>2150'],TPI, CDI,CF,WS)))
DTR.legval<-as.expression(bquote("MAE="*.(round(dtr.model$results[1,4], 2))*
                                   ", RMSE="*.(round(dtr.model$results[1,2], 2))))
Tmaxmin.leg1<-as.expression(bquote(DTR==T[max]-T[min]))
Tmaxmin.legval<-as.expression(bquote("MAE="*.(round(Tmaxmin.MAE, 2))*
                                       ", RMSE="*.(round(Tmaxmin.RMSE, 2))))
leginset<-c(DTR.leg1, DTR.legval, Tmaxmin.leg1, Tmaxmin.legval)

legend("topleft",
       legend=leginset,bty="n",
       pch=c(21, NA, 8, NA), pt.cex=c(0.8,0, 0.6,0),
       cex=0.75, y.intersp=0.85)

dev.off()

#########################################
# Percent improvement in RMSE:
## Tmax improvement over regular regression: 28% reduction of RMSE
(tmax.lm$results[1,2]-tmax.model$results[1,2])/tmax.lm$results[1,2]*100  #(orig-better)/orig  

## Tmin improvement over linear regression: 38% reduction of RMSE
(tmin.lm$results[1,2]-tminseg.model$results[1,2])/tmin.lm$results[1,2]*100  #(orig-better)/orig  

## Tavg improvement over linear regression: 31% reduction of RMSE
(tavg.lm$results[1,2]-tavgseg.model$results[1,2])/tavg.lm$results[1,2]*100  #(orig-better)/orig  


#######################################
## use these to generate raster maps!
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
albedo<-dirs[grep("HITemp_rastercovariates/albedo_ann", dirs)]      # albedo

#### Read in raster data
dem.ann<-raster(dem[2])  #these have different extent, resolution
cpi.ann<-raster(cpi)
staterf_mmann<-raster(staterf)
cl_frq_ann<-raster(clouds)
winds.24<-stack(winds); wind_sp_ms<-mean(winds.24)
lai_ann<-raster(lai)
albedo_ann<-raster(albedo)
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
tpi100_200c<-raster("Analyses/Maps/TPIresample.grd")

U1.CorrElev_m<-(CorrElev_m-2150)*(CorrElev_m >2150)

covars<-stack(x=list(CorrElev_m=CorrElev_m, 
                     cpi_wgs84=cpi_wgs84,
                     tpi100_200c=tpi100_200c,
                     staterf_mmann=staterf_mmann, 
                     cl_frq_ann=cl_frq_ann, 
                     wind_sp_ms=wind_sp_ms,
                     lai_ann=lai_ann, 
                     U1.CorrElev_m=U1.CorrElev_m))

# predict raster
Tmax.pred<-predict(covars, mods.Tmax[[6]], progress='text')
Tmin.pred<-predict(covars, mods.Tmin[[1]], progress='text')
Tavg.pred<-predict(covars, mods.Tavg[[4]], progress='text')
DTR.pred<-predict(covars, mods.DTR[[1]], progress='text')  ### not very informative...

Tempstack<-stack(Tmax.pred, Tmin.pred, Tavg.pred, DTR.pred)
names(Tempstack)<-c("Tmax", "Tmin", "Tavg", "DTR")
writeRaster(Tempstack, filename=paste0("Analyses/Maps/MA", names(Tempstack), "_20190612.tif"),
            format="GTiff", bylayer=T, overwrite=T)




