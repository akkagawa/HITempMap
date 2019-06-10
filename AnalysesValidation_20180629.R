## AnalysesValidation_20180525.R
## 6/17/2017; re-run 5/9/2018, 5/16/2018, 5/25/2018
## A. Kagawa-Viviani
## Notes: this script performs model fitting and evaluation:
##    1) mean annual station values
##        a) simple linear regression on elevation for Tmax, Tmin, Tavg, DTR
##        b) segmented regression for the above
##        c) multiple linear regression using other predictor variables
##            for Tmax, Tmin, Tavg, and DTR, using variables selected through AIC
##        d) segmented multiple linear regression using other predictor variables
##            for Tmax, Tmin, Tavg, and DTR, using variables selected through AIC (see spatial script)
##    2) Validation with jacknifing to look at MAE, RMSE
##    3) Mapping Mean Annual Tmax, Tmin, Tavg, DTR
## 
##  See AnalysesMonthlyValidation_201805xx script for below:
##    1) mean monthly station values
##        a) linear regression, b) segmented regression
##        c) multiple linear regression based on above, d) segmented MLR
##    2) Validation with jacknifing to look at MAE, RMSE
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
xyty$U1.CorrElev_m<-(xyty$CorrElev_m-2150)*(xyty$CorrElev_m >2150)

#########################################################################################
# Fit and save (mean) coefficients for:
#  a) Linear regression
lm.Tmax.full<-lm(Tmax~CorrElev_m, data=xyty)  
lm.Tmin.full<-lm(Tmin~CorrElev_m, data=xyty) 
lm.Tavg.full<-lm(Tavg~CorrElev_m, data=xyty) 
lm.DTR.full<-lm(DTR~CorrElev_m, data=xyty) # just for kicks

#  b) Segmented linear regression
slm.Tmax<-segmented(lm.Tmax.full, seg.Z=~CorrElev_m, 
                    psi=2150, control=seg.control(it.max=0))
slm.Tmin<-segmented(lm.Tmin.full, seg.Z=~CorrElev_m, 
                    psi=2150, control=seg.control(it.max=0))
slm.Tavg<-segmented(lm.Tavg.full, seg.Z=~CorrElev_m, 
                    psi=2150, control=seg.control(it.max=0))
slm.DTR<-segmented(lm.DTR.full, seg.Z=~CorrElev_m, 
                    psi=2150, control=seg.control(it.max=0))

## OK, for multiple regressions, use the AIC-selected model terms
#### see AnalysesSpatial_201805xx.R
#####################################################################################
## Examine output in Excel and decide best models for each temperature variable.
##  Tmax:     CorrElev_m(-), cpi_wgs84(+), staterf_mmann(-), cl_frq_ann(-)
##  Tmin:     CorrElev_m(-), cpi_wgs84(-), staterf_mmann(-), wind_sp_ms(+) 
##  Tmin_seg: CorrElev_m(-), cpi_wgs84(-),                   wind_sp_ms(+), lai_ann(-) 
##  Tavg:     CorrElev_m(-),               staterf_mmann(-), cl_frq_ann(-), wind_sp_ms(+)
##  DTR:      CorrElev_m(-), cpi_wgs84(+),                   cl_frq_ann(-), wind_sp_ms(-)
##
##  ** looking at AIC, segmented performs better than non-segmented except for Tmax
##  ** note coastal proximity is actually distance FROM the coast
#####################################################################################

#  c) Multiple linear regression (AICc-selected terms)
mlr.Tmax<-lm(Tmax~ CorrElev_m + cpi_wgs84 + staterf_mmann + cl_frq_ann, data=xyty) #*BEST
mlr.Tmin<-lm(Tmin ~ CorrElev_m + cpi_wgs84 + staterf_mmann + wind_sp_ms, data=xyty)
mlr.segTmin<-lm(Tmin ~ CorrElev_m + cpi_wgs84 + lai_ann + wind_sp_ms, data=xyty)  # best SMLR model
mlr.Tavg<-lm(Tavg ~ CorrElev_m + staterf_mmann + wind_sp_ms + cl_frq_ann, data=xyty)
mlr.DTR<-lm(DTR ~ CorrElev_m + cpi_wgs84 + cl_frq_ann + wind_sp_ms, data=xyty)

#  d) Multiple linear regression, segmented elevation term
smlr.mod.Tmax<-segmented(mlr.Tmax, seg.Z=~CorrElev_m, psi=2150, control=seg.control(it.max=0))
smlr.mod.Tmin<-segmented(mlr.segTmin, seg.Z=~CorrElev_m, psi=2150, control=seg.control(it.max=0)) # BEST
smlr.mod.Tavg<-segmented(mlr.Tavg, seg.Z=~CorrElev_m, psi=2150, control=seg.control(it.max=0))  # BEST
smlr.mod.DTR<-segmented(mlr.DTR, seg.Z=~CorrElev_m, psi=2150, control=seg.control(it.max=0))  # BEST

#########################################################################################
# Now set up and save these fitted coefs ########################################
# function to extract coefficient pvals
sig<-function(x){   # where x is a vector
  y<-character(length=length(x))
  y[which(x<0.001)]<-"***"
  y[which(x>=0.001 & x<0.01)]<-"**"
  y[which(x>=0.01 & x<0.05)]<-"*"
  return(y)
}

# Extract coefficients and save to file
#  a) Linear regression
tmax.lm.coefs<-data.frame(Coef=c(names(coef(lm.Tmax.full)), "adjR2"), 
                          TmaxValue=c(coef(lm.Tmax.full),summary(lm.Tmax.full)$adj.r.sq),
                          Tmax_pval=c(summary(lm.Tmax.full)$coef[,4],NA),
                          Tmax_pvalsig=c(sig(summary(lm.Tmax.full)$coef[,4]), NA))
tmin.lm.coefs<-data.frame(Coef=c(names(coef(lm.Tmin.full)), "adjR2"), 
                          TminValue=c(coef(lm.Tmin.full),summary(lm.Tmin.full)$adj.r.sq),
                          Tmin_pval=c(summary(lm.Tmin.full)$coef[,4],NA),
                          Tmin_pvalsig=c(sig(summary(lm.Tmin.full)$coef[,4]),NA))
tavg.lm.coefs<-data.frame(Coef=c(names(coef(lm.Tavg.full)), "adjR2"), 
                          TavgValue=c(coef(lm.Tavg.full),summary(lm.Tavg.full)$adj.r.sq),
                          Tavg_pval=c(summary(lm.Tavg.full)$coef[,4],NA),
                          Tavg_pvalsig=c(sig(summary(lm.Tavg.full)$coef[,4]), NA))
dtr.lm.coefs<-data.frame(Coef=c(names(coef(lm.DTR.full)), "adjR2"), 
                         DTRValue=c(coef(lm.DTR.full), summary(lm.DTR.full)$adj.r.sq),
                         DTR_pval=c(summary(lm.DTR.full)$coef[,4],NA),
                         DTR_pvalsig=c(sig(summary(lm.DTR.full)$coef[,4]), NA))
coefs.lm<-merge(merge(merge(tmax.lm.coefs, 
                            tmin.lm.coefs, all=T), 
                      tavg.lm.coefs, all=T), 
                dtr.lm.coefs, all=T) 
write.csv(coefs.lm, file="Analyses/coefs_ANNUAL_LM.csv", row.names=F)

#  b) SEGMENTED linear regression
tmax.slm.coefs<-data.frame(Coef=c(names(coef(slm.Tmax)), "adjR2"), 
                           TmaxValue=c(coef(slm.Tmax),summary(slm.Tmax)$adj.r.sq),
                           Tmax_pval=c(summary(slm.Tmax)$coef[,4],NA),
                           Tmax_pvalsig=c(sig(summary(slm.Tmax)$coef[,4]), NA))
tmin.slm.coefs<-data.frame(Coef=c(names(coef(slm.Tmin)), "adjR2"), 
                           TminValue=c(coef(slm.Tmin),summary(slm.Tmin)$adj.r.sq),
                           Tmin_pval=c(summary(slm.Tmin)$coef[,4],NA),
                           Tmin_pvalsig=c(sig(summary(slm.Tmin)$coef[,4]),NA))
tavg.slm.coefs<-data.frame(Coef=c(names(coef(slm.Tavg)), "adjR2"), 
                           TavgValue=c(coef(slm.Tavg),summary(slm.Tavg)$adj.r.sq),
                           Tavg_pval=c(summary(slm.Tavg)$coef[,4],NA),
                           Tavg_pvalsig=c(sig(summary(slm.Tavg)$coef[,4]), NA))
dtr.slm.coefs<-data.frame(Coef=c(names(coef(slm.DTR)), "adjR2"), 
                          DTRValue=c(coef(slm.DTR), summary(slm.DTR)$adj.r.sq),
                          DTR_pval=c(summary(slm.DTR)$coef[,4],NA),
                          DTR_pvalsig=c(sig(summary(slm.DTR)$coef[,4]), NA))
coefs.slm<-merge(merge(merge(tmax.slm.coefs, 
                             tmin.slm.coefs, all=T), 
                       tavg.slm.coefs, all=T), 
                 dtr.slm.coefs, all=T) 
write.csv(coefs.slm, file="Analyses/coefs_ANNUAL_SegLM.csv", row.names=F)

#  c) Multiple linear regression
tmax.mlr.coefs<-data.frame(Coef=c(names(coef(mlr.Tmax)), "adjR2"), 
                           TmaxValue=c(coef(mlr.Tmax),summary(mlr.Tmax)$adj.r.sq),
                           Tmax_pval=c(summary(mlr.Tmax)$coef[,4],NA),
                           Tmax_pvalsig=c(sig(summary(mlr.Tmax)$coef[,4]), NA))
tmin.mlr.coefs<-data.frame(Coef=c(names(coef(mlr.Tmin)), "adjR2"), 
                           TminValue=c(coef(mlr.Tmin),summary(mlr.Tmin)$adj.r.sq),
                           Tmin_pval=c(summary(mlr.Tmin)$coef[,4],NA),
                           Tmin_pvalsig=c(sig(summary(mlr.Tmin)$coef[,4]),NA))
tavg.mlr.coefs<-data.frame(Coef=c(names(coef(mlr.Tavg)), "adjR2"), 
                           TavgValue=c(coef(mlr.Tavg),summary(mlr.Tavg)$adj.r.sq),
                           Tavg_pval=c(summary(mlr.Tavg)$coef[,4],NA),
                           Tavg_pvalsig=c(sig(summary(mlr.Tavg)$coef[,4]), NA))
dtr.mlr.coefs<-data.frame(Coef=c(names(coef(mlr.DTR)), "adjR2"), 
                          DTRValue=c(coef(mlr.DTR), summary(mlr.DTR)$adj.r.sq),
                          DTR_pval=c(summary(mlr.DTR)$coef[,4],NA),
                          DTR_pvalsig=c(sig(summary(mlr.DTR)$coef[,4]), NA))

coefs.mlr<-merge(merge(merge(tmax.mlr.coefs, 
                             tmin.mlr.coefs, all=T), 
                       tavg.mlr.coefs, all=T), 
                 dtr.mlr.coefs, all=T) 
write.csv(coefs.mlr, file="Analyses/coefs_ANNUAL_MultReg.csv", row.names=F)

#  d) Segmented multiple linear regression
tmax.smlr.coefs<-data.frame(Coef=c(names(coef(smlr.mod.Tmax)), "adjR2"), 
                            TmaxValue=c(coef(smlr.mod.Tmax),summary(smlr.mod.Tmax)$adj.r.sq),
                            Tmax_pval=c(summary(smlr.mod.Tmax)$coef[,4],NA),
                            Tmax_pvalsig=c(sig(summary(smlr.mod.Tmax)$coef[,4]),NA))
tmin.smlr.coefs<-data.frame(Coef=c(names(coef(smlr.mod.Tmin)), "adjR2"), 
                            TminValue=c(coef(smlr.mod.Tmin),summary(smlr.mod.Tmin)$adj.r.sq),
                            Tmin_pval=c(summary(smlr.mod.Tmin)$coef[,4],NA),
                            Tmin_pvalsig=c(sig(summary(smlr.mod.Tmin)$coef[,4]),NA))
tavg.smlr.coefs<-data.frame(Coef=c(names(coef(smlr.mod.Tavg)), "adjR2"), 
                            TavgValue=c(coef(smlr.mod.Tavg),summary(smlr.mod.Tavg)$adj.r.sq),
                            Tavg_pval=c(summary(smlr.mod.Tavg)$coef[,4],NA),
                            Tavg_pvalsig=c(sig(summary(smlr.mod.Tavg)$coef[,4]),NA))
dtr.smlr.coefs<-data.frame(Coef=c(names(coef(smlr.mod.DTR)), "adjR2"), 
                           DTRValue=c(coef(smlr.mod.DTR), summary(smlr.mod.DTR)$adj.r.sq),
                           DTR_pval=c(summary(smlr.mod.DTR)$coef[,4],NA),
                           DTR_pvalsig=c(sig(summary(smlr.mod.DTR)$coef[,4]),NA))

coefs.smlr<-merge(merge(merge(tmax.smlr.coefs, 
                              tmin.smlr.coefs, all=T), 
                        tavg.smlr.coefs, all=T), 
                  dtr.smlr.coefs, all=T) 
write.csv(coefs.smlr, file="Analyses/coefs_ANNUAL_SMLR.csv", row.names=F)

#########################################################################################
#####  Use jackknifing to estimate model coefficients and error and MAE, RMSE
names.jk<-c("Tmax", "Tmin", "Tavg", "DTR")
n<-dim(xyty)[1]  # how many observations (stations)

##### a) jackknifing to evaluate LM predictions #####
lm.pred<-list(Tmax=list(), Tmin=list(), Tavg=list(), DTR=list())# list of lists
lm.coefs<-list(Tmax=matrix(nrow=n, ncol=2), Tmin=matrix(nrow=n, ncol=2), 
               Tavg=matrix(nrow=n, ncol=2), DTR=matrix(nrow=n, ncol=2))
pred<-data.frame(matrix(nrow=n, ncol=4)); names(pred)=names.jk

for (i in 1:n){
  # 1) drop station
  dat<-xyty[-i,]
  obs<-xyty[i,]
  
  # 2) fit model
  lm.pred$Tmax[[i]]<-lm(Tmax~CorrElev_m, data=dat)
  lm.pred$Tmin[[i]]<-lm(Tmin~CorrElev_m, data=dat)
  lm.pred$Tavg[[i]]<-lm(Tavg~CorrElev_m, data=dat)
  lm.pred$DTR[[i]]<-lm(Tavg~CorrElev_m, data=dat)
  
  # 3) extract coefficients
  lm.coefs$Tmax[i,]<-lm.pred$Tmax[[i]]$coefficients
  lm.coefs$Tmin[i,]<-lm.pred$Tmin[[i]]$coefficients
  lm.coefs$Tavg[i,]<-lm.pred$Tavg[[i]]$coefficients
  lm.coefs$DTR[i,]<-lm.pred$DTR[[i]]$coefficients
  
  # 4) predict value for dropped observation
  pred$Tmax[i]<-predict.lm(lm.pred$Tmax[[i]], obs) 
  pred$Tmin[i]<-predict.lm(lm.pred$Tmin[[i]], obs)   
  pred$Tavg[i]<-predict.lm(lm.pred$Tavg[[i]], obs)   
  pred$DTR[i]<-predict.lm(lm.pred$DTR[[i]], obs)   
} 

names(pred)<-paste(names.jk, "pred", sep=".")
err<-xyty[, names.jk]-pred; names(err)<-paste(names.jk, "err", sep=".")
sq.err<-data.frame(err^2); names(sq.err)<-paste(names.jk, "sqerr", sep=".")        # squared error: (pred-obs)^2
jk.results<-cbind(xyty, pred, err, sq.err)
write.csv(jk.results, file="Analyses/Spatial_2006_2017_fin2/LM_ANNUAL_jk_results.csv", row.names=F)

## Calculate MAE, RMSE
MAE<-colSums(abs(err))/(n-1); print(MAE)
RMSE<-sqrt(colSums(sq.err)/(n-1)); print(RMSE)

## Plot observed ~ predicted
plot(Tmax~Tmax.pred, data=jk.results, bg="tomato", pch=24, cex=0.75,
     xlim=c(-5,35), ylim=c(-5, 35),
     xlab=expression("Predicted Temperature ("*degree*C*")"), 
     ylab=expression("Observed Temperature ("*degree*C*")"),
     main="Simple linear regression on elevation")
points(Tmin~Tmin.pred, data=jk.results, bg="skyblue", pch=25, cex=0.75)
points(Tavg~Tavg.pred, data=jk.results, bg="forestgreen", pch=21)
abline(0,1)
legend("topleft",
       legend=c(expression(T[max]), expression(T[avg]), expression(T[min])),
       pt.bg=c("tomato", "forestgreen", "skyblue"), pch=c(24, 21, 25))

plot(jk.results$Tmax.err~jk.results$staterf_mmann)  # rainfall important term

##### b) jackknifing to evaluate SEGMENTED LM predictions #####
slm.coefs<-list(Tmax=matrix(nrow=n, ncol=3), Tmin=matrix(nrow=n, ncol=3), 
                Tavg=matrix(nrow=n, ncol=3), DTR=matrix(nrow=n, ncol=3))
slm.pred<-list(Tmax=list(), Tmin=list(), Tavg=list(), DTR=list()) # list of lists
slmpred<-data.frame(matrix(nrow=n, ncol=4)); names(slmpred)=names.jk

for (i in 1:n){
  # 1) drop station
  dat<-xyty[-i,]
  obs<-xyty[i,]
  
  # 2) fit model
  slm.pred$Tmax[[i]]<-segmented(lm(Tmax~CorrElev_m, data=dat), seg.Z=~CorrElev_m, psi=2150, 
                                control=seg.control(it.max=0))
  slm.pred$Tmin[[i]]<-segmented(lm(Tmin~CorrElev_m, data=dat), seg.Z=~CorrElev_m, psi=2150, 
                                control=seg.control(it.max=0))
  slm.pred$Tavg[[i]]<-segmented(lm(Tavg~CorrElev_m, data=dat), seg.Z=~CorrElev_m, psi=2150, 
                                control=seg.control(it.max=0))
  slm.pred$DTR[[i]]<-segmented(lm(Tavg~CorrElev_m, data=dat), seg.Z=~CorrElev_m, psi=2150, 
                               control=seg.control(it.max=0))
  # 3) extract coefficients
  slm.coefs$Tmax[i,]<-slm.pred$Tmax[[i]]$coefficients
  slm.coefs$Tmin[i,]<-slm.pred$Tmin[[i]]$coefficients
  slm.coefs$Tavg[i,]<-slm.pred$Tavg[[i]]$coefficients
  slm.coefs$DTR[i,]<-slm.pred$DTR[[i]]$coefficients
  
  # 4) predict value for dropped observation  ** U1.CorrElev_m must be specified
  slmpred$Tmax[i]<-predict(slm.pred$Tmax[[i]], obs)
  slmpred$Tmin[i]<-predict(slm.pred$Tmin[[i]], obs)
  slmpred$Tavg[i]<-predict(slm.pred$Tavg[[i]], obs)
  slmpred$DTR[i]<-predict(slm.pred$DTR[[i]], obs)
} 

names(slmpred)<-paste(names.jk, "pred", sep=".")
slmerr<-xyty[, names.jk]-slmpred; names(slmerr)<-paste(names.jk, "err", sep=".")
slmsq.err<-data.frame(slmerr^2); names(slmsq.err)<-paste(names.jk, "sqerr", sep=".")        # squared error: (pred-obs)^2
slm.jk.results<-cbind(xyty, slmpred, slmerr, slmsq.err)
write.csv(slm.jk.results, file="Analyses/Spatial_2006_2017_fin2/SegLM_ANNUAL_jk_results.csv", row.names=F)

## Calculate MAE, RMSE
slm.MAE<-colSums(abs(slmerr))/(n-1); print(slm.MAE)
slm.RMSE<-sqrt(colSums(slmsq.err)/(n-1)); print(slm.RMSE)

## Plot observed ~ predicted
plot(Tmax~Tmax.pred, data=slm.jk.results, bg="tomato", pch=24, cex=0.75,
     xlim=c(-5,35), ylim=c(-5, 35),
     xlab=expression("Predicted Temperature ("*degree*C*")"), 
     ylab=expression("Observed Temperature ("*degree*C*")"),
     main="Segmented linear regression on elevation")
points(Tmin~Tmin.pred, data=slm.jk.results, bg="skyblue", pch=25, cex=0.75)
points(Tavg~Tavg.pred, data=slm.jk.results, bg="forestgreen", pch=21)
abline(0,1)
legend("topleft",
       legend=c(expression(T[max]), expression(T[avg]), expression(T[min])),
       pt.bg=c("tomato", "forestgreen", "skyblue"), pch=c(24, 21, 25))

##### c) jackknifing to evaluate Multiple Regression predictions #####
names.mlr<-c("Tmax", "Tmin", "Tavg", "DTR")
mlr.coefs<-list(Tmax=matrix(nrow=n, ncol=5), Tmin=matrix(nrow=n, ncol=5), 
                Tavg=matrix(nrow=n, ncol=5), DTR=matrix(nrow=n, ncol=5))
mlr.pred<-list(Tmax=list(), Tmin=list(), Tavg=list(), DTR=list()) # list of lists
mlrpred<-data.frame(matrix(nrow=n, ncol=4)); names(mlrpred)=names.mlr

for (i in 1:n){   # for each station,
  # 1) drop station
  dat<-xyty[-i,]  
  obs<-xyty[i,]
  
  # 2) fit model
  mlr.pred$Tmax[[i]]<-lm(Tmax~ CorrElev_m + cpi_wgs84 + staterf_mmann + cl_frq_ann, data=dat)
  mlr.pred$Tmin[[i]]<-lm(Tmin ~ CorrElev_m + cpi_wgs84 + staterf_mmann + wind_sp_ms, data=dat)
  mlr.pred$Tavg[[i]]<-lm(Tavg ~ CorrElev_m + staterf_mmann + wind_sp_ms + cl_frq_ann, data=dat)
  mlr.pred$DTR[[i]]<-lm(DTR ~ CorrElev_m + cpi_wgs84 + cl_frq_ann + wind_sp_ms, data=dat)
  
  # 3) extract coefficients
  mlr.coefs$Tmax[i,]<-mlr.pred$Tmax[[i]]$coefficients
  mlr.coefs$Tmin[i,]<-mlr.pred$Tmin[[i]]$coefficients
  mlr.coefs$Tavg[i,]<-mlr.pred$Tavg[[i]]$coefficients
  mlr.coefs$DTR[i,]<-mlr.pred$DTR[[i]]$coefficients
  
  # 4) predict value for dropped observation
  mlrpred$Tmax[i]<-predict.lm(mlr.pred$Tmax[[i]], obs)  
  mlrpred$Tmin[i]<-predict.lm(mlr.pred$Tmin[[i]], obs)   
  mlrpred$Tavg[i]<-predict.lm(mlr.pred$Tavg[[i]], obs)   
  mlrpred$DTR[i]<-predict.lm(mlr.pred$DTR[[i]], obs)   
}

names(mlrpred)<-paste(names.mlr, "pred", sep=".")
mlr.err<-xyty[, names.mlr]-mlrpred; names(mlr.err)<-paste(names.mlr, "err", sep=".")
mlr.sq.serr<-data.frame(mlr.err^2); names(mlr.sq.serr)<-paste(names.mlr, "sqserr", sep=".")        # squared error: (pred-obs)^2

# 5) visualize spread of fitted coefficients
for (i in 1:4){      # for each temp variable
  par(mfrow=c(3,2))
  for (j in 1:5){    # plot hist of each of 5 coeffs
    hist(mlr.coefs[[i]][,j],
         main=paste(names.mlr[i], 
                    names(mlr.pred[[i]][[1]]$coefficients[j]), sep=", "))
  }
}

mlr.results<-cbind(xyty, mlrpred, mlr.err, mlr.sq.serr)
write.csv(mlr.results, file="Analyses/Spatial_2006_2017_fin2/MLR_ANNUAL_jk_results.csv", row.names=F)

## 6) Calculate MAE, RMSE
mlrMAE<-colSums(abs(mlr.err))/(n-1); print(mlrMAE)
mlrRMSE<-sqrt(colSums(mlr.sq.serr)/(n-1)); print(mlrRMSE)

## 7) Plot observed ~ predicted
par(mfrow=c(1,1))
plot(Tmax~Tmax.pred, data=mlr.results, bg="tomato", pch=24, cex=0.75,
     xlim=c(-5,35), ylim=c(-5, 35),
     xlab=expression("Predicted Temperature ("*degree*C*")"), 
     ylab=expression("Observed Temperature ("*degree*C*")"),
     main="Multiple linear regression")
points(Tmin~Tmin.pred, data=mlr.results, bg="skyblue", pch=25, cex=0.75)
points(Tavg~Tavg.pred, data=mlr.results, bg="forestgreen", pch=21)
abline(0,1)
legend("topleft",
       legend=c(expression(T[max]), expression(T[avg]), expression(T[min])),
       pt.bg=c("tomato", "forestgreen", "skyblue"), pch=c(24, 21, 25))

##### d) jackknifing to evaluate SEGMENTED Multiple Regression predictions #####
names.smlr<-c("Tmax", "Tmin", "Tavg", "DTR")
smlr.coefs<-list(Tmax=matrix(nrow=n, ncol=6), Tmin=matrix(nrow=n, ncol=6), 
                 Tavg=matrix(nrow=n, ncol=6), DTR=matrix(nrow=n, ncol=6))
smlr.pred<-list(Tmax=list(), Tmin=list(), Tavg=list(), DTR=list()) # list of lists
smlrpred<-data.frame(matrix(nrow=n, ncol=4)); names(smlrpred)=names.smlr

for (i in 1:n){  # for each station,
  # 1) drop station
  dat<-xyty[-i,]
  obs<-xyty[i,]
  
  # 2a) develop linear model for subsequent segmented() function
  lm.Tmax<-lm(Tmax~ CorrElev_m + cpi_wgs84 + staterf_mmann + cl_frq_ann, data=dat)
  lm.segTmin<-lm(Tmin ~ CorrElev_m + cpi_wgs84 + lai_ann + wind_sp_ms, data=dat)
  lm.Tavg<-lm(Tavg ~ CorrElev_m + staterf_mmann + wind_sp_ms + cl_frq_ann, data=dat)
  lm.DTR<-lm(DTR ~ CorrElev_m + cpi_wgs84 + cl_frq_ann + wind_sp_ms, data=dat)
  
  # 2b) fit model
  smlr.pred$Tmax[[i]]<-segmented(lm.Tmax, seg.Z=~CorrElev_m, psi=2150, 
                                 control=seg.control(it.max=0))
  smlr.pred$Tmin[[i]]<-segmented(lm.segTmin, seg.Z=~CorrElev_m, psi=2150, 
                                 control=seg.control(it.max=0))
  smlr.pred$Tavg[[i]]<-segmented(lm.Tavg, seg.Z=~CorrElev_m, psi=2150, 
                                 control=seg.control(it.max=0))
  smlr.pred$DTR[[i]]<-segmented(lm.DTR, seg.Z=~CorrElev_m, psi=2150, 
                                control=seg.control(it.max=0))
  
  # 3) extract coefficients
  smlr.coefs$Tmax[i,]<-smlr.pred$Tmax[[i]]$coefficients
  smlr.coefs$Tmin[i,]<-smlr.pred$Tmin[[i]]$coefficients
  smlr.coefs$Tavg[i,]<-smlr.pred$Tavg[[i]]$coefficients
  smlr.coefs$DTR[i,]<-smlr.pred$DTR[[i]]$coefficients
  
  # 4) predict value for dropped observation
  smlrpred$Tmax[i]<-predict(smlr.pred$Tmax[[i]], obs)
  smlrpred$Tmin[i]<-predict(smlr.pred$Tmin[[i]], obs)
  smlrpred$Tavg[i]<-predict(smlr.pred$Tavg[[i]], obs)
  smlrpred$DTR[i]<-predict(smlr.pred$DTR[[i]], obs)
}

# 5) visualize spread of fitted coefficients
for (i in 1:4){par(mfrow=c(3,2))
  for (j in 1:6){  # For each of 6 fitted coefficients
    hist(smlr.coefs[[i]][,j],main=paste(names.smlr[i], 
                                        names(smlr.pred[[i]][[1]]$coefficients[j]), sep=", "))
  }
}

names(smlrpred)<-paste(names.smlr, "pred", sep=".")
smlrerr<-xyty[, names.smlr]-smlrpred; names(smlrerr)<-paste(names.smlr, "err", sep=".")
smlrsq.serr<-data.frame(smlrerr^2); names(smlrsq.serr)<-paste(names.smlr, "sqserr", sep=".")        # squared error: (pred-obs)^2

smlr.results<-cbind(xyty, smlrpred, smlrerr, smlrsq.serr)
write.csv(smlr.results, file="Analyses/Spatial_2006_2017_fin2/SMLR_ANNUAL_jk_results.csv", row.names=F)

## 6) Calculate MAE, RMSE
smlrMAE<-colSums(abs(smlrerr))/(n-1); print(smlrMAE)
smlrRMSE<-sqrt(colSums(smlrsq.serr)/(n-1)); print(smlrRMSE)

## 7) Plot observed ~ predicted
par(mfrow=c(1,1))
plot(Tmax~Tmax.pred, data=smlr.results, bg="tomato", pch=24, cex=0.75,
     xlim=c(-5,35), ylim=c(-5, 35),
     xlab=expression("Predicted Temperature ("*degree*C*")"), 
     ylab=expression("Observed Temperature ("*degree*C*")"),
     main="Segmented multiple linear regression")
points(Tmin~Tmin.pred, data=smlr.results, bg="skyblue", pch=25, cex=0.75)
points(Tavg~Tavg.pred, data=smlr.results, bg="forestgreen", pch=21)
abline(0,1)
legend("topleft",
       legend=c(expression(T[max]), expression(T[avg]), expression(T[min])),
       pt.bg=c("tomato", "forestgreen", "skyblue"), pch=c(24, 21, 25))
########################################################################################
#################   MAKE FIGURE 2: Validation Stats, BEST MODELS #########################
## look at observed ~ predicted; 
##   Tmax: multiple linear regression
##   Tmin/Tavg/DTR: segmented multiple regression
pdf("FINAL_FiguresTables/Figure2_fin20180629.pdf")
par(par.default)
plot(Tmax~Tmax.pred, data=smlr.results, bg="tomato", pch=24, cex=0.75,   
     xlim=c(-5,35), ylim=c(-5, 35),
     xlab=expression("Predicted Temperature ("*degree*C*")"), 
     ylab=expression("Observed Temperature ("*degree*C*")"),
     main=expression(AIC[c]~"-selected regression models"),
     mgp=c(2.5,1,0))
points(Tmin~Tmin.pred, data=smlr.results, bg="skyblue", pch=25, cex=0.75)
points(Tavg~Tavg.pred, data=smlr.results, bg="forestgreen", pch=21)
abline(0,1)

Tmax.leg<-as.expression(bquote(T[max]==f(z[seg],CDI,RF,CF)*
                                 ", MAE="*.(round(smlrMAE[1], 2))*
                                 ", RMSE="*.(round(smlrRMSE[1], 2))))
Tmin.leg<-as.expression(bquote(T[min]== f (z[seg],CDI,WS,LAI)*
                                 ", MAE="*.(round(smlrMAE[2], 2))*
                                 ", RMSE="*.(round(smlrRMSE[2], 2)))) 
Tavg.leg<-as.expression(bquote(T[avg]== f (z[seg],RF,WS, CF)*
                                 ", MAE="*.(round(smlrMAE[3], 2))*
                                 ", RMSE="*.(round(smlrRMSE[3], 2))))
leg<-c(Tmax.leg, Tmin.leg, Tavg.leg)
legend("topleft", legend=leg, bty="n",
       pt.bg=c("tomato", "forestgreen", "skyblue"),
       pch=c(24, 21, 25))

# ## DTR inset
# DTR.leg2<-as.expression(bquote(DTR== f(z)))
# DTR.leg2b<-as.expression(bquote("MAE="*.(round(MAE[4], 2))*", RMSE="*.(round(RMSE[4], 2))))
# DTR.leg1<-as.expression(bquote(DTR== f(z[seg],CDI,CF, WS)))
# DTR.leg1b<-as.expression(bquote("MAE="*.(round(smlrMAE[4], 2))*", RMSE="*.(round(smlrRMSE[4], 2))))
# 
# leginset<-c(DTR.leg2, DTR.leg2b, DTR.leg1, DTR.leg1b)
# legend(x=20, y=18,
#        legend=leginset,
#        pch=c(8, NA, 21, NA), pt.cex=c(0.6,0, 0.8,0),
#        bty="n", cex=0.85, y.intersp=0.95)
# 
# par(fig = c(.53, 0.9, 0.16, 0.45), mar=c(2,2.2,0,0), oma=c(0,0,0,0), new=TRUE)
# plot(DTR~DTR.pred, data=smlr.results, pch=21, cex=0.8,
#      xlim=c(4,25), ylim=c(4, 25),
#      xlab=expression(DTR[pred]), 
#      ylab=expression(DTR[obs]),cex.axis=0.8,
#      cex.lab=0.9, mgp=c(1.2,0.2,0), tcl=-0.3)
# abline(0,1)
# points(DTR~DTR.pred, data=jk.results, pch=8, cex=0.5)  # see improvement from linear regression

## DTR inset2: modeled DTR and modelet Tmax-Tmin
Tmaxmin<-smlr.results$Tmax.pred-smlr.results$Tmin.pred
Tmaxmin.err<-smlr.results$DTR-Tmaxmin
Tmaxmin.MAE<-sum(abs(Tmaxmin.err))/(length(Tmaxmin.err)-1); print(Tmaxmin.MAE)
Tmaxmin.RMSE<-sqrt(sum(Tmaxmin.err^2)/(length(Tmaxmin.err)-1)); print(Tmaxmin.RMSE)

DTR.leg1<-as.expression(bquote(DTR== f(z[seg],CDI,CF, WS)))
DTR.legval<-as.expression(bquote("MAE="*.(round(smlrMAE[4], 2))*", RMSE="*.(round(smlrRMSE[4], 2))))

Tmaxmin.leg1<-as.expression(modeled~T[max]-T[min])
Tmaxmin.legval<-as.expression(bquote("MAE="*.(round(Tmaxmin.MAE, 2))*", RMSE="*.(round(Tmaxmin.RMSE, 2))))

leginset<-c(DTR.leg1, DTR.legval, Tmaxmin.leg1, Tmaxmin.legval)
legend(x=20, y=18,
       legend=leginset,
       pch=c(21, NA, 8, NA), pt.cex=c(0.8,0, 0.6,0),
       bty="n", cex=0.85, y.intersp=0.95)

par(fig = c(.53, 0.9, 0.16, 0.45), mar=c(2,2.2,0,0), oma=c(0,0,0,0), new=TRUE)
plot(DTR~DTR.pred, data=smlr.results, pch=21, cex=0.8,
     xlim=c(4,15), ylim=c(4, 15),
     xlab=expression(DTR[pred]),
     ylab=expression(DTR[obs]),cex.axis=0.8,
     cex.lab=0.9, mgp=c(1.2,0.2,0), tcl=-0.3)
abline(0,1)
points(smlr.results$DTR~Tmaxmin, pch=8, cex=0.5)  # see improvement from linear regression

dev.off()

MAE
slm.MAE
mlrMAE
smlrMAE

smlrMAE/MAE

RMSE
slm.RMSE
mlrRMSE
smlrRMSE

# Percent improvement in RMSE:
(RMSE-smlrRMSE)/RMSE*100  #(orig-better)/orig  
(mlrRMSE-smlrRMSE)/mlrRMSE*100  #(orig-better)/orig

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
Tmax.pred<-predict(covars, smlr.mod.Tmax, progress='text')
Tmin.pred<-predict(covars, smlr.mod.Tmin, progress='text')
Tavg.pred<-predict(covars, smlr.mod.Tavg, progress='text')
DTR.pred<-predict(covars, smlr.mod.DTR, progress='text')

Tempstack<-stack(Tmax.pred, Tmin.pred, Tavg.pred, DTR.pred)
names(Tempstack)<-c("Tmax", "Tmin", "Tavg", "DTR")
writeRaster(Tempstack, filename=paste0("Analyses/Maps/MA", names(Tempstack), "_20180525.tif"),
            format="GTiff", bylayer=T, overwrite=T)



