## AnalysesTemporal_p0_20190613.R (formerly AnalysesTemporal_p1_20180220.R, AnalysesJul2017Temporal.RAnalysesJun2017Temporal.R, AnalysesApr2016v4, Analyses.R)
## Author: Aurora Kagawa-Viviani
## Date: Feb 23, 2018, rearranged 6/13/2019 
## Notes: Script for calculating regression time series from station data
##     Generates Figure 1
## Feb 2018 notes: removed Midway (-999 elevation)
## 7/20/2017 notes: created new script for interannual/interdecadal variability, revised for cloud/wind 7/20/2017
## 6/23/2017 notes: added TWI criteria, simplified Fig 1 (stations)
## Apr 18-20 2016 notes: major overhaul
## DESCRIPTION: This script addresses only station coverage (see p1 for trends)

setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping")

## Read in annual data file (12 station months/observation)
merged_yr<-read.csv("Merged_yr.csv", stringsAsFactors = F)  
merged_mo<-read.csv("Merged_mo.csv", stringsAsFactors = F)  

length(unique(merged_yr$Station))   ## 219 stations
length(unique(merged_mo$Station))   ## 289 stations

offshore_yr<-c(grep("USC00516271", merged_yr$Station), # remove Midway (USC00516271) 
               grep("USC00510350", merged_yr$Station), # remove Moku o Loe (USC00510350)
               grep("USR0000HMOL", merged_yr$Station)) # remove Moloaa Dairy (USR0000HMOL) QA/QC fail
offshore_mo<-c(grep("USC00516271", merged_mo$Station), # remove Midway (USC00516271) 
               grep("USC00510350", merged_mo$Station), # remove Moku o Loe (USC00510350)
               grep("USR0000HMOL", merged_mo$Station)) # remove Moloaa Dairy (USR0000HMOL) QA/QC fail

merged_yr<-merged_yr[-offshore_yr,] ## 217 stations
merged_mo<-merged_mo[-offshore_mo,] ## 287 stations

# RESTRICT YEARS to 1905-2017 
merged_yr<-merged_yr[merged_yr$Year>=1905 & merged_yr$Year<=2017,]  
merged_mo<-merged_mo[merged_mo$Year>=1905 & merged_mo$Year<=2017,]  

############ plotting number of stations over time 
## From Analyses0_1600andALL.R
numhi<-function(x) length(which(x>1600)) # "high elevation" spanning TWI
numTWI<-function(x) length(which(x>=2150)) # above TWI

## All complete years: #obs, #>1600, #>=2150, max elevation
yr_num<-tapply(merged_yr$Station, INDEX=merged_yr$Year, length)       # num stations
yr_numhi<-tapply(merged_yr$CorrElev_m, INDEX=merged_yr$Year, numhi)   # num hi
yr_numTWI<-tapply(merged_yr$CorrElev_m, INDEX=merged_yr$Year, numTWI) # num hi
yr_maxelev<-tapply(merged_yr$CorrElev_m, INDEX=merged_yr$Year, max)   # max elev

## All complete months: #obs, #>1600, #>=2150, max elevation
mo_num<-tapply(merged_mo$Station, INDEX=merged_mo$MonthYear, length)       # num stations
mo_numhi<-tapply(merged_mo$CorrElev_m, INDEX=merged_mo$MonthYear, numhi)   # num hi
mo_numTWI<-tapply(merged_mo$CorrElev_m, INDEX=merged_mo$MonthYear, numTWI) # num hi
mo_maxelev<-tapply(merged_mo$CorrElev_m, INDEX=merged_mo$MonthYear, max)   # max elev

## Calculate mean station month-years; compare later to station-years
mo_num.mean<-tapply(mo_num, INDEX=substr(names(mo_num), start=1, stop=4), mean)
mo_max.median<-tapply(mo_maxelev, INDEX=substr(names(mo_num), start=1, stop=4), median)

###############################################################################
## Generate plot of stations (Figure 1)
png(file = "Fig1_Stations2017scr.png", bg = "transparent",
    width=1200, height=400,
    antialias = "cleartype")
op <- par(mar = c(5,4,4,4) + 0.2)
x<-barplot(yr_num, col="white", ylim=c(0,65), axes=F, cex.names=1.5)
axis(2, at=seq(0,65,10), cex.axis=1.2)
mtext("Number of Stations", side = 2, line = 2.5, cex = 1.5)
#barplot(yr_numhi, col="gray",   add=T, axes=F)
barplot(yr_numTWI, col="black",   add=T, axes=F,  cex.names=1.5)
#points(x, as.numeric(mo_num.mean), pch="*")
par(new=T)
plot(x, yr_maxelev, type="l", lwd=2, axes=F,ann=F, 
     ylim=c(400,4200))
axis(4, at=seq(0,4200,500), cex.axis=1.2)
mtext("Max Elevation (m)", side = 4, line = 3, cex = 1.5)
#lines(x, as.numeric(mo_max.median), lty=2)
box()
legend(x="topleft", bty="n",  
       title="Annual series stations", title.adj=0,
       inset=0.02,
       legend=c("total stations", "stations >2150m",
                "maximum elevation"),
       lwd=c(NA, NA, 2), pch=c(0, 15, NA), 
       bg=c("white","black", NA),
       cex=1.3, xjust=0)
dev.off()

##################################################################
## Analysis 1: Temporal Trends : 
##    -subset all with CorrElev_m below 1600
##    -drop Stations beginning with "USR" (RAWS)
##  Calculate for ** Monthly** AND **Annual**
##    - simple linear regression of temperature variable on elevation
##    - keep track of diagnostics
##  Analyze:
##    - non-parametric trend in Tmax(z=0), Tmin(z=0), DTR(z=0), Tavg(z=0)
##          from 1918-2017, 1968-2017
##################################################################
st.ts<-1905
nd.ts<-2017

## Take low-elevation subset; remove RAWS
## Monthly series:
temporal_mo<-subset(merged_mo, merged_mo$CorrElev_m<=1600)
tempNoRAWS_mo<-subset(temporal_mo, !grepl("USR", temporal_mo$Station))
a1_mo<-subset(tempNoRAWS_mo, 
              tempNoRAWS_mo$Year>=st.ts & tempNoRAWS_mo$Year<=nd.ts)               
## Annual series:
temporal_yr<-subset(merged_yr, merged_yr$CorrElev_m<=1600)
tempNoRAWS_yr<-subset(temporal_yr, !grepl("USR", temporal_yr$Station))
a1_yr<-subset(tempNoRAWS_yr, 
              tempNoRAWS_yr$Year>=st.ts & tempNoRAWS_yr$Year<=nd.ts) 

## Summary: look at mean data vs time for strange patterns
## Monthly series
mo_Tmax<-tapply(a1_mo$Tmax, INDEX=a1_mo$MonthYear, max)
mo_Tmin<-tapply(a1_mo$Tmin, INDEX=a1_mo$MonthYear, min)
mo_DTR<-tapply(a1_mo$DTR, INDEX=a1_mo$MonthYear, mean)
mo_Tavg<-tapply(a1_mo$Tavg, INDEX=a1_mo$MonthYear, mean)

par(mfrow=c(2,2))
plot(mo_Tmax~names(mo_Tmin), col="red", pch=2) #triangle up
plot(mo_Tmin~names(mo_Tmin), col="blue", pch=6) #triangle dn
plot(mo_DTR)
plot(mo_Tavg)

## Annual series Tavg, all stations over time
par(mfrow=c(1,1))
plot(Tavg~Year, data=a1_yr)

## Calculate simple linear regression
require(stats)
## Monthly series
Tmax<-by(a1_mo, a1_mo$MonthYear, 
         function(x) lm(Tmax ~ CorrElev_m, data = x))
Tmin<-by(a1_mo, a1_mo$MonthYear, 
         function(x) lm(Tmin ~ CorrElev_m, data = x))
DTR<-by(a1_mo, a1_mo$MonthYear, 
        function(x) lm(DTR ~ CorrElev_m, data = x))
Tavg<-by(a1_mo, a1_mo$MonthYear, 
         function(x) lm(Tavg ~ CorrElev_m, data = x))
moyrs<-names(Tmax)
coefs<-data.frame(MonthYear=moyrs,
                  Tmax=matrix(unlist(lapply(Tmax, FUN=coef)), ncol=2, byrow=T),
                  Tmin=matrix(unlist(lapply(Tmin, FUN=coef)), ncol=2, byrow=T),
                  Tavg=matrix(unlist(lapply(Tavg, FUN=coef)), ncol=2, byrow=T),
                  DTR=matrix(unlist(lapply(DTR, FUN=coef)), ncol=2, byrow=T))
names(coefs)<-c("MonthYear", 
                "Tmax.b", "Tmax.m",
                "Tmin.b", "Tmin.m",
                "Tavg.b", "Tavg.m",
                "DTR.b", "DTR.m")
# head(coefs)
# coefs$DTR.b2<-coefs$Tmax.b-coefs$Tmin.b  ## identical
# summary(lm(coefs$DTR.b2~coefs$DTR.b))

R2<-data.frame(MonthYear=moyrs,
               Tmax=unlist(lapply(Tmax, FUN=function(x) summary(x)$adj.r.squared)),
               Tmin=unlist(lapply(Tmin, FUN=function(x) summary(x)$adj.r.squared)),
               Tavg=unlist(lapply(Tavg, FUN=function(x) summary(x)$adj.r.squared)),
               DTR=unlist(lapply(DTR, FUN=function(x) summary(x)$adj.r.squared)))
# DTR slope is not significant but intercept is 
names(R2)<-c("MonthYear", "Tmax", "Tmin", "Tavg", "DTR")

out<-data.frame(coefs, R2)
names(out)<-c(paste("coefs", names(coefs), sep="_"), paste("R2", names(R2), sep="_"))
write.csv(out, "monthly_lm_2017.csv")

## Annual series
Tmaxy<-by(a1_yr, a1_yr$Year, 
          function(x) lm(Tmax ~ CorrElev_m, data = x))
Tminy<-by(a1_yr, a1_yr$Year, 
          function(x) lm(Tmin ~ CorrElev_m, data = x))
DTRy<-by(a1_yr, a1_yr$Year, 
         function(x) lm(DTR ~ CorrElev_m, data = x))
Tavgy<-by(a1_yr, a1_yr$Year, 
          function(x) lm(Tavg ~ CorrElev_m, data = x))
yrs<-as.numeric(names(Tmaxy))
coefsy<-data.frame(Year=yrs,
                   Tmax=matrix(unlist(lapply(Tmaxy, FUN=coef)), ncol=2, byrow=T),
                   Tmin=matrix(unlist(lapply(Tminy, FUN=coef)), ncol=2, byrow=T),
                   Tavg=matrix(unlist(lapply(Tavgy, FUN=coef)), ncol=2, byrow=T),
                   DTR=matrix(unlist(lapply(DTRy, FUN=coef)), ncol=2, byrow=T))
names(coefsy)<-c("Year", 
                 "Tmax.b", "Tmax.m",
                 "Tmin.b", "Tmin.m",
                 "Tavg.b", "Tavg.m",
                 "DTR.b", "DTR.m")  

R2y<-data.frame(Year=yrs,
                Tmax=unlist(lapply(Tmaxy, FUN=function(x) summary(x)$adj.r.squared)),
                Tmin=unlist(lapply(Tminy, FUN=function(x) summary(x)$adj.r.squared)),
                Tavg=unlist(lapply(Tavgy, FUN=function(x) summary(x)$adj.r.squared)),
                DTR=unlist(lapply(DTRy, FUN=function(x) summary(x)$adj.r.squared)))
# DTR slope is not significant but intercept is.

names(R2y)<-c("Year", "Tmax", "Tmin", "Tavg", "DTR")
outy<-data.frame(coefsy, R2y)
names(outy)<-c(paste("coefs", names(coefsy), sep="_"), paste("R2", names(R2y), sep="_"))
write.csv(outy, "annual_lm_2017.csv")  # Time series of fitted coefficients

