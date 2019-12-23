## TrendMethodComparison.R
## Author: Aurora Kagawa-Viviani
## Date: Feb 5 2018
## Notes: Script for calculating trends with annual time series

##################################################################
setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping")
coefsy<-read.csv("annual_lm.csv")    

library(nlme)
library(Kendall)
library(zyp)

ssets<-list(    # create subsets for the data 
  akv_1906_2016=subset(coefsy, X>=1916 & X<=2016),
  akv_1956_2016=subset(coefsy, X>=1956 & X<=2016),
  mm_1917_2016=subset(coefsy, X>=1917 & X<=2016),
  mm_1917_1939=subset(coefsy, X>=1917 & X<=1939),
  mm_1940_1957=subset(coefsy, X>=1940 & X<=1957),
  mm_1958_1999=subset(coefsy, X>=1958 & X<=1999),
  mm_2000_2015=subset(coefsy, X>=2000 & X<=2015)
)

# Calculate for Tmax, Tmin, Tavg
# Using gls(), lm(), and zyp.sen(0)
# For different periods (see ssets above)

df<-data.frame(matrix(NA, nrow=length(ssets), ncol=7*3))  # create empty df
names(df)<-paste0(rep(c("Tmax_","Tmin_", "Tavg_"), each=7), # name the columns
            c("gls", "gls.p", "lm", "lm.p", "sen", "sen05", "sen95"))

for (i in 1:length(ssets)){  # for each period
  dat<-ssets[[i]]            # indicate the data subset
  
  # Tmax
  gls.max<-gls(coefs_Tmax.b~coefs_Year, data=dat,
      correlation=corAR1(), na.action = na.exclude)
  lm.max<-lm(coefs_Tmax.b~coefs_Year, data=dat)
  sen.max<-zyp.sen(coefs_Tmax.b~coefs_Year, data=dat)
  ci.senmax<-confint(sen.max)
  
  # Tmin
  gls.min<-gls(coefs_Tmin.b~coefs_Year, data=dat,
               correlation=corAR1(), na.action = na.exclude)
  lm.min<-lm(coefs_Tmin.b~coefs_Year, data=dat)
  sen.min<-zyp.sen(coefs_Tmin.b~coefs_Year, data=dat)
  ci.senmin<-confint(sen.min)

# Tavg
  gls.avg<-gls(coefs_Tavg.b~coefs_Year, data=dat,
               correlation=corAR1(), na.action = na.exclude)
  lm.avg<-lm(coefs_Tavg.b~coefs_Year, data=dat)
  sen.avg<-zyp.sen(coefs_Tavg.b~coefs_Year, data=dat)
  ci.senavg<-confint(sen.avg)
  
## add them all to dataframe
  # Tmax
  df[i,1]<-coef(summary(gls.max))[2,1]  # gls trend
  df[i,2]<-coef(summary(gls.max))[2,4]  # gls p-value
  df[i,3]<-coef(summary(lm.max))[2,1]   # lm trend
  df[i,4]<-coef(summary(lm.max))[2,4]   # lm p-value
  df[i,5]<-coef(sen.max)[2]             # sen's slope
  df[i,6:7]<-ci.senmax[2,1:2]           # sen's slope 95% ci

  # Tmin
  df[i,8]<-coef(summary(gls.min))[2,1]
  df[i,9]<-coef(summary(gls.min))[2,4]
  df[i,10]<-coef(summary(lm.min))[2,1]
  df[i,11]<-coef(summary(lm.min))[2,4]
  df[i,12]<-coef(sen.min)[2]
  df[i,13:14]<-ci.senmin[2,1:2]
  
  # Tavg
  df[i,15]<-coef(summary(gls.avg))[2,1]
  df[i,16]<-coef(summary(gls.avg))[2,4]
  df[i,17]<-coef(summary(lm.avg))[2,1]
  df[i,18]<-coef(summary(lm.avg))[2,4]
  df[i,19]<-coef(sen.avg)[2]
  df[i,20:21]<-ci.senavg[2,1:2]
}

row.names(df)<-names(ssets)
write.csv(df, file="TrendMethodComparison.csv")

###############################################################################
### Consider station coverage through time.  Visualize as maps

setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping")
merged_yr<-read.csv("Merged_yr.csv")  ## 220 stations
merged_yr<-merged_yr[-which(merged_yr$CorrElev_m<0),]  # Remove -999.9 elevation
merged_yr<-merged_yr[-grep("WAIMEA 892", merged_yr$Name),]                    # Remove WAIMEA 892
regress_yr<-subset(merged_yr, CorrElev_m<=800)
years<-sort(unique(regress_yr$Year))


library(sp)
library(raster)
library(rgdal)

dem<-dir("C:/Users/Aurora/OneDrive/GIS/HITemp_rastercovariates/",
pattern="clipdem_wgs84", full.names=T)
dem.r<-raster(dem) # single layer
proj4string(dem.r)

ext<-extent(c(-160.5, -154.5, 18.8, 22.5))
dem.r.small<-crop(dem.r, ext)

for (i in years){
  dat<-subset(regress_yr, Year==i)
  png(paste0("CompareMarieM/StationYears/Stations", i, ".png"), 
      width=750, height=530)
  plot(dem.r.small, col=gray((1:20)/20),   # grayscale DEM
       main=i)  
  points(dat$CorrLong_DD, dat$CorrLat_DD, 
       col="green", pch=16, cex=.8) # solid circle
  dev.off()
}
  
library(magick)
library(dplyr)
list.files(path = "CompareMarieM/StationYears/", pattern = "Stations", full.names = T) %>%
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("HITempStations.gif") # write to current dir


###############################################################################
### Consider station coverage through time.  Visualize as scatterplots+regression
ylims<-trunc(range(range(regress_yr$Tmin, regress_yr$Tmax)))+c(-1,1)

for (i in years){
  dat<-subset(regress_yr, Year==i)
  png(paste0("CompareMarieM/StationYears/lapse800_WM", i, ".png"), 
      width=600, height=600)
  fit.min<-lm(Tmin~CorrElev_m, data=dat)
  fit.max<-lm(Tmax~CorrElev_m, data=dat)
  
  plot(dat$Tmin~dat$CorrElev_m, col="blue",
       ylim=ylim, xlim=c(0,1800),
       xlab="Elevation (m)", ylab="Temp (C)", main=i)
  abline(v=0, col="gray")
  if(!any(is.na(coef(fit.min)))){
    abline(coef(fit.min), col="blue")
  }
  points(dat$CorrElev_m, dat$Tmax, col="red")
  if(!any(is.na(coef(fit.max)))){
    abline(coef(fit.max), col="red")
  }
  dev.off()
}

library(magick)
library(dplyr)
list.files(path = "CompareMarieM/StationYears/", pattern = "lapse800_WM", full.names = T) %>%
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("HITemp0_800_WM.gif") # write to current dir

###########################################################
### Try bootstrapping 
###   to estimate confidence around regression coefficients

# library(bootstrap)  # by Tibshirani for S-Plus 
# xdata<- dat[,c("Tmin", "CorrElev_m")]
# theta<-function(x, xdata) {
#   fit<-lm(xdata$Tmin[x]~xdata$CorrElev_m[x])
#   fit$coefficients[1]
# } 
# n<-dim(xdata)[1]
# results<-bootstrap(x=1:n, 
#                    nboot=1000,
#                    theta=theta, 
#                    xdata)

library(boot)       # by Ripley
# function to obtain fitted coefficients from the data
lm.boot<- function(formula, data, indices){
  d<-data[indices,]      # allows boot to select sample 
  fit<-lm(formula, data=d)
  return(coef(fit))
}


# # bootstrapping with 1000 replications 
# results <- boot(data=xdata, statistic=lm.boot, 
#                 R=1000, formula=Tmin~CorrElev_m)
# 
# # view results
# results 
# plot(results, index=1)   # intercept 
# plot(results, index=2, add=T)   # slope
# 
# # get 95% confidence intervals 
# boot.ci(results, type="bca", index=1) # intercept 
# s.ci<-boot.ci(results, type="bca", index=2) # slope

n<-1
boot.all<-data.frame(matrix(NA, ncol=21, nrow=length(years)))

for (i in years){                    # for each year
  dat<-subset(regress_yr, Year==i)   # take the appropriate subset
  
  if(dim(dat)[1]>2){
    # Bootstrap CI for Tmin intercept and slope
    boot.min <- boot(data=dat, statistic=lm.boot, 
                     R=1000, formula=Tmin~CorrElev_m)
    boot.min0.ci<-boot.ci(boot.min, type="bca", index=1)
    boot.minL.ci<-boot.ci(boot.min, type="bca", index=2)
    
    # Bootstrap CI for Tmax intercept and slope
    boot.max <- boot(data=dat, statistic=lm.boot, 
                     R=1000, formula=Tmax~CorrElev_m)
    boot.max0.ci<-boot.ci(boot.max, type="bca", index=1)
    boot.maxL.ci<-boot.ci(boot.max, type="bca", index=2)
    
    # Bootstrap CI for Tavg intercept and slope
    boot.avg <- boot(data=dat, statistic=lm.boot, 
                     R=1000, formula=Tavg~CorrElev_m)
    boot.avg0.ci<-boot.ci(boot.avg, type="bca", index=1)
    boot.avgL.ci<-boot.ci(boot.avg, type="bca", index=2)
    
    boot.yr<-c(i, dim(dat)[1], max(dat$CorrElev_m),
               boot.min$t0[1], boot.min0.ci$bca[4:5],
               boot.min$t0[2], boot.minL.ci$bca[4:5],
               boot.max$t0[1], boot.max0.ci$bca[4:5],
               boot.max$t0[2], boot.maxL.ci$bca[4:5],
               boot.avg$t0[1], boot.avg0.ci$bca[4:5],
               boot.avg$t0[2], boot.avgL.ci$bca[4:5])

    boot.all[n,]<-boot.yr
    print(paste(i, dim(dat)[1]>2, n))
    n<-n+1
  }
}

names(boot.all)<-c("Year", "NumStations", "MaxElev",
                   "Tmin.0", "Tmin.0.low", "Tmin.0.up",
                   "Tmin.lapse", "Tmin.lapse.low", "Tmin.lapse.up",
                   "Tmax.0", "Tmax.0.low", "Tmax.0.up",
                   "Tmax.lapse", "Tmax.lapse.low", "Tmax.lapse.up",
                   "Tavg.0", "Tavg.0.low", "Tavg.0.up",
                   "Tavg.lapse", "Tavg.lapse.low", "Tavg.lapse.up")

write.csv(boot.all, "bootstrap800_WM.csv", row.names=F)

##########################################################
### Read in and compare 1600m lapse and 800m lapse
boot.1600<-read.csv("bootstrap1600_WM.csv")
boot.800<-read.csv("bootstrap800_WM.csv")

pdf("Bootstrapped_WM.pdf")
par(mfcol=c(3,2), mar=c(2,4,4,4))
# Plot Tmin 
Tmin<-boot.1600[,c(1:3, grep("Tmin", names(boot.1600)))]
# sea level with bootstrap 95% intervals
plot(Tmin.0~Year, data=Tmin, pch=19,
     main="Tmin at sea level, 0-1600",
     ylim=range(Tmin[,4:6], na.rm=T))
arrows(x0=Tmin$Year,
       y0=Tmin$Tmin.0.low, 
       y1=Tmin$Tmin.0.up, 
       length=0.0, code=3, col="grey")
# lapse rate with bootstrap 95% intervals
plot(Tmin.lapse~Year, data=Tmin, pch=19,
     main="Tmin lapse rate, 0-1600",
     ylim=range(Tmin[,7:9], na.rm=T))
arrows(x0=Tmin$Year,
       y0=Tmin$Tmin.lapse.low, 
       y1=Tmin$Tmin.lapse.up, 
       length=0.0, code=3, col="orange")
# number of stations, max elevation
plot(NumStations~Year, data=Tmin, pch=19,
     main="# Stations, Max Elev",
     ylim=c(0, max(Tmin$NumStations, na.rm=T)))
par(new = TRUE)
plot(MaxElev~Year, data=Tmin, pch=1, axes=F, type="l",
     bty = "n", xlab = "", ylab = "", ylim=c(0,1600))
axis(side=4)
mtext("Max Elev", side=4, line=2.5, cex=0.7)

Tmin<-boot.800[,c(1:3, grep("Tmin", names(boot.800)))]
plot(Tmin.0~Year, data=Tmin, pch=19,
     main="Tmin at sea level, 0-800",
     ylim=range(Tmin[,4:6], na.rm=T))
arrows(x0=Tmin$Year,
       y0=Tmin$Tmin.0.low, 
       y1=Tmin$Tmin.0.up, 
       length=0.0, code=3, col="grey")
plot(Tmin.lapse~Year, data=Tmin, pch=19,
     main="Tmin lapse rate, 0-800",
     ylim=range(Tmin[,7:9], na.rm=T))
arrows(x0=Tmin$Year,
       y0=Tmin$Tmin.lapse.low, 
       y1=Tmin$Tmin.lapse.up, 
       length=0.0, code=3, col="orange")
plot(NumStations~Year, data=Tmin, pch=19,
     main="# Stations, Max Elev",
     ylim=c(0, max(Tmin$NumStations, na.rm=T)))
par(new = TRUE)
plot(MaxElev~Year, data=Tmin, pch=1, axes=F, type="l",
     bty = "n", xlab = "", ylab = "", ylim=c(0,800))
axis(side=4)
mtext("Max Elev", side=4, line=2.5, cex=0.7)


par(mfcol=c(3,2), mar=c(2,4,4,4))
# Tmax
Tmax<-boot.1600[,c(1:3, grep("Tmax", names(boot.1600)))]
# sea level with bootstrap 95% intervals
plot(Tmax.0~Year, data=Tmax, pch=19,
     ylim=range(Tmax[,4:6], na.rm=T),
     main="Tmax at sea level, 0-1600")
arrows(x0=Tmax$Year,
       y0=Tmax$Tmax.0.low, 
       y1=Tmax$Tmax.0.up, 
       length=0.0, code=3, col="grey")
# lapse rate with bootstrap 95% intervals
plot(Tmax.lapse~Year, data=Tmax, pch=19,
     main="Tmax lapse rate, 0-1600",
     ylim=range(Tmax[,7:9], na.rm=T))
arrows(x0=Tmax$Year,
       y0=Tmax$Tmax.lapse.low, 
       y1=Tmax$Tmax.lapse.up, 
       length=0.0, code=3, col="orange")
# number of stations, max elevation
plot(NumStations~Year, data=Tmax, pch=19,
     main="# Stations, Max Elev",
     ylim=c(0, max(Tmax$NumStations, na.rm=T)))
par(new = TRUE)
plot(MaxElev~Year, data=Tmax, pch=1, axes=F, type="l",
     bty = "n", xlab = "", ylab = "", ylim=c(0,1600))
axis(side=4)
mtext("Max Elev", side=4, line=2.5, cex=0.7)


Tmax<-boot.800[,c(1:3, grep("Tmax", names(boot.800)))]
plot(Tmax.0~Year, data=Tmax, pch=19,
     ylim=range(Tmax[,4:6], na.rm=T),
     main="Tmax at sea level, 0-800")
arrows(x0=Tmax$Year,
       y0=Tmax$Tmax.0.low, 
       y1=Tmax$Tmax.0.up, 
       length=0.0, code=3, col="grey")
plot(Tmax.lapse~Year, data=Tmax, pch=19,
     main="Tmax lapse rate, 0-800",
     ylim=range(Tmax[,7:9], na.rm=T))
arrows(x0=Tmax$Year,
       y0=Tmax$Tmax.lapse.low, 
       y1=Tmax$Tmax.lapse.up, 
       length=0.0, code=3, col="orange")
# number of stations, max elevation
plot(NumStations~Year, data=Tmax, pch=19,
     main="# Stations, Max Elev",
     ylim=c(0, max(Tmax$NumStations, na.rm=T)))
par(new = TRUE)
plot(MaxElev~Year, data=Tmax, pch=1, axes=F, type="l",
     bty = "n", xlab = "", ylab = "", ylim=c(0,800))
axis(side=4)
mtext("Max Elev", side=4,line=2.5, cex=0.7)



par(mfcol=c(3,2), mar=c(2,4,4,4))
# Tavg
Tavg<-boot.1600[,c(1:3, grep("Tavg", names(boot.1600)))]
# sea level with bootstrap 95% intervals
plot(Tavg.0~Year, data=Tavg, pch=19,
     ylim=range(Tavg[,4:6], na.rm=T),
     main="Tavg at sea level, 0-1600")
arrows(x0=Tavg$Year,
       y0=Tavg$Tavg.0.low, 
       y1=Tavg$Tavg.0.up, 
       length=0.0, code=3, col="grey")
# lapse rate with bootstrap 95% intervals
plot(Tavg.lapse~Year, data=Tavg, pch=19,
     main="Tavg lapse rate, 0-1600",
     ylim=range(Tavg[,7:9], na.rm=T))
arrows(x0=Tavg$Year,
       y0=Tavg$Tavg.lapse.low, 
       y1=Tavg$Tavg.lapse.up, 
       length=0.0, code=3, col="orange")
# number of stations, max elevation
plot(NumStations~Year, data=Tavg, pch=19,
     main="# Stations, Max Elev",
     ylim=c(0, max(Tavg$NumStations, na.rm=T)))
par(new = TRUE)
plot(MaxElev~Year, data=Tavg, pch=1, axes=F, type="l",
     bty = "n", xlab = "", ylab = "", ylim=c(0,1600))
axis(side=4)
mtext("Max Elev", side=4,line=2.5, cex=0.7)

Tavg<-boot.800[,c(1:3, grep("Tavg", names(boot.800)))]
plot(Tavg.0~Year, data=Tavg, pch=19,
     ylim=range(Tavg[,4:6], na.rm=T),
     main="Tavg at sea level, 0-800")
arrows(x0=Tavg$Year,
       y0=Tavg$Tavg.0.low, 
       y1=Tavg$Tavg.0.up, 
       length=0.0, code=3, col="grey")
plot(Tavg.lapse~Year, data=Tavg, pch=19,
     main="Tavg lapse rate, 0-800",
     ylim=range(Tavg[,7:9], na.rm=T))
arrows(x0=Tavg$Year,
       y0=Tavg$Tavg.lapse.low, 
       y1=Tavg$Tavg.lapse.up, 
       length=0.0, code=3, col="orange")
plot(NumStations~Year, data=Tavg, pch=19,
     main="# Stations, Max Elev",
     ylim=c(0, max(Tavg$NumStations, na.rm=T)))
par(new = TRUE)
plot(MaxElev~Year, data=Tavg, pch=1, axes=F, type="l",
     bty = "n", xlab = "", ylab = "", ylim=c(0,800))
axis(side=4)
mtext("Max Elev", side=4,line=2.5, cex=0.7)
dev.off()
