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
#dir.create("Analyses/Spatial_2006_2017_fin2019") # move into Analyses/Spatial directory

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
dtr.xyty<-as.matrix(xyty[,c("DTR", "CorrElev_m", "cl_frq_ann","cpi_wgs84", "U1.CorrElev_m",
                            "tpi100_200c", "wind_sp_ms")])


set.seed(123)
# define training control
train_control <- trainControl(method="cv", number=5)

# train the model
tmax.model <- train(Tmax~., data=tmax.xyty, trControl=train_control, method="lm")
tminseg.model <- train(Tmin~., data=tminseg.xyty, trControl=train_control, method="lm")
tavgseg.model <- train(Tavg~., data=tavgseg.xyty, trControl=train_control, method="lm")
dtr.model <- train(DTR~., data=dtr.xyty[,-2], trControl=train_control, method="lm")

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
pdf("FINAL_FiguresTables/Figure2_fin20190612.pdf", width=9, height=7)
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
       cex=0.8, y.intersp=0.85)

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
       cex=0.8, y.intersp=0.85)

dev.off()

#########################################
# Percent improvement in RMSE:
## Tmax improvement over regular regression: 28% reduction of RMSE
(tmax.lm$results[1,2]-tmax.model$results[1,2])/tmax.lm$results[1,2]*100  #(orig-better)/orig  

## Tmin improvement over linear regression: 38% reduction of RMSE
(tmin.lm$results[1,2]-tminseg.model$results[1,2])/tmin.lm$results[1,2]*100  #(orig-better)/orig  

## Tavg improvement over linear regression: 31% reduction of RMSE
(tavg.lm$results[1,2]-tavgseg.model$results[1,2])/tavg.lm$results[1,2]*100  #(orig-better)/orig  

## DTR improvement over linear regression: 28% reduction of RMSE
(dtr.lm$results[1,2]-dtr.model$results[1,2])/dtr.lm$results[1,2]*100  #(orig-better)/orig  

## Calculated Tavg improvement over model prediction: 6% reduction of RMSE
(tavgseg.model$results[1,2]-Tavgmaxmin.RMSE)/tavgseg.model$results[1,2]*100  #(orig-better)/orig  

## Calculated DTR improvement over model prediction: 4% reduction of RMSE
(dtr.model$results[1,2]-Tmaxmin.RMSE)/dtr.model$results[1,2]*100  #(orig-better)/orig  


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
Tavg.calc<-(Tmax.pred+Tmin.pred)/2
DTR.calc<-Tmax.pred-Tmin.pred

Tempstack<-stack(Tmax.pred, Tmin.pred, Tavg.calc, DTR.calc)
names(Tempstack)<-c("Tmax", "Tmin", "Tavg", "DTR")
writeRaster(Tempstack, filename=paste0("Analyses/Spatial_2006_2017_fin2019/Maps/MA", names(Tempstack), "_20190612.tif"),
            format="GTiff", bylayer=T, overwrite=T)

###################################################################################
## from AnalysesMonthlyValidation_20180525.R
## Notes: this script performs model fitting, evaluation, and mapping:
##    1) mean MONTHLY station values (2006-2017)
##       multiple linear regression using other predictor variables
##         for Tmax, Tmin, Tavg, and DTR, using variables selected through AIC for annual set
##    2) month-year station values (2006-2017)
##       multiple linear regression using other predictor variables
##         for Tmax, Tmin, Tavg, and DTR, using variables selected through AIC for annual set


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

par(mfrow=c(4,3))
for (i in 1:12){
  plot(Tmax~CorrElev_m, data=xyty.s[xyty.s$mo==unique(xyty.s$mo)[i],], col="red")
}


######### Monthly models #############  ### need to fit one for each mean month
mos<-sprintf("%02d", 1:12)

##### For each month, fit BEST multiple regressions selected earlier
names.mlr<-c("Tmax", "Tmin", "Tavg", "DTR")
mlr.mod<-list(Tmax=list(), Tmin=list(), Tavg=list(), DTR=list()) 
mlr.coefs<-list(Tmax=matrix(nrow=12, ncol=7), Tmin=matrix(nrow=12, ncol=7), 
                Tavg=matrix(nrow=12, ncol=7), DTR=matrix(nrow=12, ncol=7))

par(mfrow=c(4,3))

for (i in 1:length(mos)){
  dat<-xyty.s[xyty.s$mo==mos[i],] # subset data for a given month
  plot(dat$Tmin~dat$CorrElev_m, col="blue",
       ylim=range(c(dat$Tmin, dat$Tmax)), main=mos[i])
  points(dat$Tmax~dat$CorrElev_m, col="red")
  
  # fit best regression model
  mlr.mod$Tmax[[i]]<-lm(formula(mods.Tmax[[6]]), data=dat)
  mlr.mod$Tmin[[i]]<-lm(formula(mods.Tmin[[1]]), data=dat)
  mlr.mod$Tavg[[i]]<-lm(formula(mods.Tavg[[4]]), data=dat)
  mlr.mod$DTR[[i]]<-lm(formula(mods.DTR[[1]]), data=dat)
  
  pred.max<-predict(mlr.mod$Tmax[[i]],dat)
  points(pred.max~dat$CorrElev_m, pch="*")
  pred.min<-predict(mlr.mod$Tmin[[i]],dat)
  points(pred.min~dat$CorrElev_m, pch="*")
}

# extract coefficients
mlr.coefs$Tmax<-lapply(mlr.mod$Tmax, FUN= function(x) {c(coef(x), summary(x)$adj.r.squared)})
mlr.coefs$Tmin<-lapply(mlr.mod$Tmin, FUN= function(x) {c(coef(x), summary(x)$adj.r.squared)})
mlr.coefs$Tavg<-lapply(mlr.mod$Tavg, FUN= function(x) {c(coef(x), summary(x)$adj.r.squared)})
mlr.coefs$DTR<-lapply(mlr.mod$DTR, FUN= function(x) {c(coef(x), summary(x)$adj.r.squared)})

tmaxcoefs<-data.frame(mos, matrix(unlist(mlr.coefs$Tmax), byrow=T, ncol=6))
names(tmaxcoefs)<-c("Month", names(coef(mlr.mod$Tmax[[1]])), "adj.R.sq")

tmincoefs<-data.frame(mos, matrix(unlist(mlr.coefs$Tmin), byrow=T, ncol=7))
names(tmincoefs)<-c("Month", names(coef(mlr.mod$Tmin[[1]])), "adj.R.sq")

tavgcoefs<-data.frame(mos, matrix(unlist(mlr.coefs$Tavg), byrow=T, ncol=7))
names(tavgcoefs)<-c("Month", names(coef(mlr.mod$Tavg[[1]])), "adj.R.sq")

dtrcoefs<-data.frame(mos, matrix(unlist(mlr.coefs$DTR), byrow=T, ncol=7))
names(dtrcoefs)<-c("Month", names(coef(mlr.mod$DTR[[1]])), "adj.R.sq")

write.csv(tmaxcoefs, file="Analyses/Spatial_2006_2017_fin2019/coefsMO.Tmax.csv", row.names=F)
write.csv(tmincoefs, file="Analyses/Spatial_2006_2017_fin2019/coefsMO.Tmin.csv", row.names=F)
write.csv(tavgcoefs, file="Analyses/Spatial_2006_2017_fin2019/coefsMO.Tavg.csv", row.names=F)
write.csv(dtrcoefs, file="Analyses/Spatial_2006_2017_fin2019/coefsMO.DTR.csv", row.names=F)

par(mfrow=c(3,3), mar=c(3,4,3,2)) # plot fitted coefficients through time
for (j in 2:ncol(tmaxcoefs)){
  plot(tmaxcoefs[,j], main=names(tmaxcoefs)[j])
}
par(mfrow=c(3,3), mar=c(3,4,3,2))
for (j in 2:ncol(tmincoefs)){
  plot(tmincoefs[,j], main=names(tmincoefs)[j])
}
par(mfrow=c(3,3), mar=c(3,4,3,2))
for (j in 2:ncol(tavgcoefs)){
  plot(tavgcoefs[,j], main=names(tmincoefs)[j])
}
par(mfrow=c(3,3), mar=c(3,4,3,2))
for (j in 2:ncol(dtrcoefs)){
  plot(dtrcoefs[,j], main=names(tmincoefs)[j])
}

# look at summaries
mlr.summary<-list()
mlr.summary$Tmax<-lapply(mlr.mod$Tmax, FUN= summary)
mlr.summary$Tmin<-lapply(mlr.mod$Tmin, FUN= summary)
mlr.summary$Tavg<-lapply(mlr.mod$Tavg, FUN= summary)
mlr.summary$DTR<-lapply(mlr.mod$DTR, FUN= summary)

######## Map these back onto a raster!

# For each month,
Tmax.pred<-list()
Tmin.pred<-list()
Tavg.calc<-list()
DTR.calc<-list()

# predict raster
for (i in 1:12){   
  Tmax.pred[[i]]<-predict(covars, mlr.mod$Tmax[[i]], progress='text')
  Tmin.pred[[i]]<-predict(covars, mlr.mod$Tmin[[i]], progress='text')
  Tavg.calc[[i]]<-(Tmin.pred[[i]]+Tmax.pred[[i]])/2
  DTR.calc[[i]]<-Tmax.pred[[i]]-Tmin.pred[[i]]
}

# stack raster
Tmax.pred.stack<-stack(Tmax.pred)
Tmin.pred.stack<-stack(Tmin.pred)
Tavg.calc.stack<-stack(Tavg.calc)
DTR.calc.stack<-stack(DTR.calc)

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
plot(Tavg.calc.stack, breaks=fixbreaks, col=my.colors(15))

plot(DTR.calc.stack, breaks=fixstackcolors(DTR.calc.stack, DTR.calc.stack, 15),
     col=my.colors(15))

Temp<-list(Tmax=Tmax.pred.stack, 
           Tmin=Tmin.pred.stack,
           Tavg=Tavg.calc.stack, 
           DTR=DTR.calc.stack)

## Now save these files as geotiffs------------------------------------------------------
for (i in 1:length(Temp)){
  writeRaster(Temp[[i]],
              filename=paste0("Analyses/Spatial_2006_2017_fin2019/Maps/Monthly_", 
                              names(Temp)[i], "stack.tif"),
              format="GTiff", bylayer=F, overwrite=T)
}

## And print them as pngs for easy viewing:
for (i in 1:length(mos)){
  png(file=paste0("Analyses/Spatial_2006_2017_fin2019/Maps/Tmax_", mos[i], ".png"), bg="transparent")
  plot(Temp$Tmax[[i]], main=paste("Tmax, Month =", month.abb[i]),
       zlim=c(0,32))      # set color ramp
  dev.off()
  
  png(file=paste0("Analyses/Spatial_2006_2017_fin2019/Maps/Tmin_", mos[i], ".png"), bg="transparent")
  plot(Temp$Tmin[[i]], main=paste("Tmin, Month =", month.abb[i]),
       zlim=c(0,32))      # set color ramp
  dev.off()
  
  png(file=paste0("Analyses/Spatial_2006_2017_fin2019/Maps/Tavg_", mos[i], ".png"), bg="transparent")
  plot(Temp$Tavg[[i]], main=paste("Tavg, Month =", month.abb[i]),
       zlim=c(0,32))     # set color ramp
  dev.off()
  
  png(file=paste0("Analyses/Spatial_2006_2017_fin2019/Maps/DTR_", mos[i], ".png"), bg="transparent")
  plot(Temp$DTR[[i]], main=paste("DTR, Month =", month.abb[i]), zlim=c(2,16))
  dev.off()
}


# pred vs obs by month
for (i in 1:12){
  dat<-xyty.s[xyty.s$mo==mos[i],]
  pred.max<-predict(mlr.mod$Tmax[[i]],dat) 
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
      pred.max[j]<-predict(mlr.mod$Tmax[[j]],dat) 
      pred.min[j]<-predict(mlr.mod$Tmin[[j]],dat) 
      pred.avg[j]<-predict(mlr.mod$Tavg[[j]],dat) 
      pred.dtr[j]<-predict(mlr.mod$DTR[[j]],dat)
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
names.mlr.m06_17<-c("Tmax", "Tmin", "Tavg", "DTR")
mlr.mod.m06_17<-list(Tmax=vector("list", 12), 
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


par(mfrow=c(4,3))
for (i in 1:length(Year)){                     # for each year
  for (j in 1:length(Month)) {                  # and month
    dat<-m06_17x[m06_17x$Year==Year[i] & m06_17x$Month==Month[j],] # subset data (don't use subset?!)
    print(paste(Year[i], Month[j], dim(dat)[1]))
    plot(Tmin~CorrElev_m, data=dat, col="blue",
         ylim=range(dat[,c("Tmin", "Tmax")]),
         main=paste(Year[i], Month[j]))
    points(dat$Tmax~dat$CorrElev_m, col="red")
    
    # fit linear model for best form selected by annual AIC
    tmax.yr.mod[[j]]<-lm(formula(mods.Tmax[[6]]), data=dat)
    tmin.yr.mod[[j]]<-lm(formula(mods.Tmin[[1]]), data=dat)
    tavg.yr.mod[[j]]<-lm(formula(mods.Tavg[[4]]), data=dat)
    dtr.yr.mod[[j]]<-lm(formula(mods.DTR[[1]]), data=dat)
    
    pred.max<-predict(tmax.yr.mod[[j]],dat)
    points(pred.max~dat$CorrElev_m, pch="*")
    
    pred.min<-predict(tmin.yr.mod[[j]],dat)
    points(pred.min~dat$CorrElev_m, pch="*")
  }
  
  # list of regression models
  mlr.mod.m06_17$Tmax[[i]]<-tmax.yr.mod
  mlr.mod.m06_17$Tmin[[i]]<-tmin.yr.mod
  mlr.mod.m06_17$Tavg[[i]]<-tavg.yr.mod
  mlr.mod.m06_17$DTR[[i]]<-dtr.yr.mod
  
  # extract coefficients
  tmax.coefs.m06_17[[i]]<-cbind(Month, matrix(unlist(lapply(mlr.mod.m06_17$Tmax[[i]], FUN= coefmat)), 
                                              ncol=6, byrow=T))
  tmin.coefs.m06_17[[i]]<-cbind(Month, matrix(unlist(lapply(mlr.mod.m06_17$Tmin[[i]], FUN= coefmat)), 
                                              ncol=7, byrow=T))
  tavg.coefs.m06_17[[i]]<-cbind(Month, matrix(unlist(lapply(mlr.mod.m06_17$Tavg[[i]], FUN= coefmat)), 
                                              ncol=7, byrow=T))
  dtr.coefs.m06_17[[i]]<-cbind(Month, matrix(unlist(lapply(mlr.mod.m06_17$DTR[[i]], FUN= coefmat)), 
                                             ncol=7, byrow=T))
}

names(tmax.coefs.m06_17)<-names(tmin.coefs.m06_17)<-Year
names(tavg.coefs.m06_17)<-names(dtr.coefs.m06_17)<-Year

for (i in 1:length(Year)){
  tmax.dat<-data.frame(Year=Year[i], tmax.coefs.m06_17[[i]])
  names(tmax.dat)<-c("Year", "Month", names(coef(tmax.yr.mod[[1]])), "adj.R.sq")
  
  tmin.dat<-data.frame(Year=Year[i], tmin.coefs.m06_17[[i]])
  names(tmin.dat)<-c("Year", "Month", names(coef(tmin.yr.mod[[1]])), "adj.R.sq")
  
  tavg.dat<-data.frame(Year=Year[i], tavg.coefs.m06_17[[i]])
  names(tavg.dat)<-c("Year", "Month", names(coef(tavg.yr.mod[[1]])), "adj.R.sq")
  
  dtr.dat<-data.frame(Year=Year[i], dtr.coefs.m06_17[[i]])
  names(dtr.dat)<-c("Year", "Month", names(coef(dtr.yr.mod[[1]])), "adj.R.sq")
  
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

write.csv(tmax.coefs.fin, file="Analyses/Spatial_2006_2017_fin2019/Maps/coefs06_17.Tmax.csv", row.names=F)
write.csv(tmin.coefs.fin, file="Analyses/Spatial_2006_2017_fin2019/Maps/coefs06_17.Tmin.csv", row.names=F)
write.csv(tavg.coefs.fin, file="Analyses/Spatial_2006_2017_fin2019/Maps/coefs06_17.Tavg.csv", row.names=F)
write.csv(dtr.coefs.fin, file="Analyses/Spatial_2006_2017_fin2019/Maps/coefs06_17.DTR.csv", row.names=F)

# plot fitted coefficients through time; sanity check
par(mfrow=c(4,4))  
for (n in 3:ncol(tmax.coefs.fin)){  # for each coefficient
    plot(tmax.coefs.fin[,n], ylab=names(tmax.coefs.fin)[n], col="red")
}
par(mfrow=c(4,4))  
for (n in 3:ncol(tmin.coefs.fin)){  # for each coefficient
    plot(tmin.coefs.fin[,n], ylab=names(tmin.coefs.fin)[n],col="blue")
}
par(mfrow=c(4,4))  
for (n in 3:ncol(tavg.coefs.fin)){  # for each coefficient
    plot(tavg.coefs.fin[,n], ylab=names(tavg.coefs.fin)[n],col="black")
}
par(mfrow=c(4,4))  
for (n in 3:ncol(dtr.coefs.fin)){  # for each coefficient
  plot(dtr.coefs.fin[,n], ylab=names(dtr.coefs.fin)[n], col="dark gray", pch="*")
}

# look at summaries
tmax.m06_17.summary<-vector("list", length=length(Year))
tmin.m06_17.summary<-vector("list", length=length(Year))
tavg.m06_17.summary<-vector("list", length=length(Year))
dtr.m06_17.summary<-vector("list", length=length(Year))
for (i in 1:length(Year)){
  tmax.m06_17.summary[[i]]<-lapply(mlr.mod.m06_17$Tmax[[i]], summary) # 12 months of summaries in each year
  tmin.m06_17.summary[[i]]<-lapply(mlr.mod.m06_17$Tmin[[i]], summary)
  tavg.m06_17.summary[[i]]<-lapply(mlr.mod.m06_17$Tavg[[i]], summary)
  dtr.m06_17.summary[[i]]<-lapply(mlr.mod.m06_17$DTR[[i]], summary)
}

######## Map these back onto a raster!

# set color scale for plotting
fixstackcolors<-function(tmax, tmin,n){   
  rng<-range(cellStats(tmax, max), cellStats(tmin, min))
  round(seq(from=floor(rng[1]), to=ceiling(rng[2]), length.out=n))}
my.colors<-colorRampPalette(c("blue", "green", "light green", "yellow", "orange", "red"))


# predict raster
for (i in 1:length(Year)){    # For each year,  # from 2014
  
  # For each year, have a temporary vector
  Tmax.pred<-vector("list", length=12)  # length 12 for each month
  Tmin.pred<-vector("list", length=12)
  Tavg.calc<-vector("list", length=12)
  DTR.calc<-vector("list", length=12)
  
  for (j in 1:length(Month)){
    Tmax.pred[[j]]<-predict(covars, mlr.mod.m06_17$Tmax[[i]][[j]], progress='text')
    Tmin.pred[[j]]<-predict(covars, mlr.mod.m06_17$Tmin[[i]][[j]], progress='text')
    Tavg.calc[[j]]<-(Tmax.pred[[j]]+Tmin.pred[[j]])/2
    DTR.calc[[j]]<-predict(covars, mlr.mod.m06_17$DTR[[i]][[j]], progress='text')
  }
  
  # stack raster
  Tmax.pred.stack<-stack(Tmax.pred); 
  Tmin.pred.stack<-stack(Tmin.pred)
  Tavg.calc.stack<-stack(Tavg.calc)
  DTR.calc.stack<-stack(DTR.calc)
  
  ## Now save these raster stacks as 12-layer geotiffs------
  writeRaster(Tmax.pred.stack,
              filename=paste0("Analyses/Spatial_2006_2017_fin2019/Maps/Tmax_", Year[i], "stack.tif"),
              format="GTiff", bylayer=F, overwrite=T)
  writeRaster(Tmin.pred.stack,
              filename=paste0("Analyses/Spatial_2006_2017_fin2019/Maps/Tmin_", Year[i], "stack.tif"),
              format="GTiff", bylayer=F, overwrite=T)
  writeRaster(Tavg.calc.stack,
              filename=paste0("Analyses/Spatial_2006_2017_fin2019/Maps/Tavg_", Year[i], "stack.tif"),
              format="GTiff", bylayer=F, overwrite=T)
  writeRaster(DTR.calc.stack,
              filename=paste0("Analyses/Spatial_2006_2017_fin2019/Maps/DTR_", Year[i], "stack.tif"),
              format="GTiff", bylayer=F, overwrite=T)
  
  # ## Do some plotting
  # fixbreaks<-fixstackcolors(Tmax.pred.stack,    # fix color scale for monthly raster stacks
  #                           Tmin.pred.stack, 15) 
  # 
  # par(mfrow=c(4,3))
  # plot(Tmax.pred.stack, breaks=fixbreaks, col=my.colors(15))
  # plot(Tmin.pred.stack, breaks=fixbreaks, col=my.colors(15))
  # plot(Tavg.calc.stack, breaks=fixbreaks, col=my.colors(15))
  # 
  # plot(DTR.calc.stack, breaks=fixstackcolors(DTR.calc.stack, DTR.calc.stack, 15),
  #      col=my.colors(15))
  
  range(cellStats(Tmax.pred.stack, max), cellStats(Tmin.pred.stack, min))
  
  ## And print them as pngs for easy viewing:
  for (j in 1:length(Month)){
    png(file=paste0("Analyses/Spatial_2006_2017_fin2019/Maps/Tmax_", Year[i], sprintf("%02d", Month[j]), ".png"), bg="transparent")
    plot(Tmax.pred.stack[[j]], main=paste("Tmax", Year[i], month.abb[j]), zlim=c(0,32)) # fix col ramp
    dev.off()
    
    png(file=paste0("Analyses/Spatial_2006_2017_fin2019/Maps/Tmin_", Year[i], sprintf("%02d", Month[j]), ".png"), bg="transparent")
    plot(Tmin.pred.stack[[j]], main=paste("Tmin", Year[i], month.abb[j]), zlim=c(0,32)) # fix col ramp
    dev.off()
    
    png(file=paste0("Analyses/Spatial_2006_2017_fin2019/Maps/Tavg_", Year[i], sprintf("%02d", Month[j]), ".png"), bg="transparent")
    plot(Tavg.calc.stack[[j]], main=paste("Tavg", Year[i], month.abb[j]), zlim=c(0,32)) # fix col ramp   
    dev.off()
    
    png(file=paste0("Analyses/Spatial_2006_2017_fin2019/Maps/DTR_", Year[i], sprintf("%02d", Month[j]), ".png"), bg="transparent")
    plot(DTR.calc.stack[[j]], main=paste("DTR", Year[i], month.abb[j]), zlim=c(2,16))
    dev.off()
  }
}


# pred vs obs by monthyear: Tmax
par(mfrow=c(4,3))
for (i in 1:length(Year)){
  for (j in 1:length(Month)){
    dat<-m06_17x[m06_17x$Year==Year[i] & m06_17x$Month==Month[j],] 
    pred.max<-predict(mlr.mod.m06_17$Tmax[[i]][[j]], dat)
    # pred.min<-predict(mlr.mod.m06_17$Tmin[[i]][[j]], dat)
    # pred.avg<-predict(mlr.mod.m06_17$Tavg[[i]][[j]], dat)
    # pred.dtr<-predict(mlr.mod.m06_17$DTR[[i]][[j]], dat)
    plot(dat$Tmax~pred.max, col="red", main=paste(Year[i], sprintf("%02d", Month[j])))
    abline(0,1)
  }
}


