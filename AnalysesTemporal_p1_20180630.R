## AnalysesTemporal_p1_20180220.R (formerly AnalysesJul2017Temporal.RAnalysesJun2017Temporal.R, AnalysesApr2016v4, Analyses.R)
## Author: Aurora Kagawa-Viviani
## Date: Feb 23, 2018
## Notes: Script for running analyses on time series
## Feb 2018 notes: removed Midway (-999 elevation)
## 7/20/2017 notes: created new script for interannual/interdecadal variability, revised for cloud/wind 7/20/2017
## 6/23/2017 notes: added TWI criteria, simplified Fig 1 (stations)
## Apr 18-20 2016 notes: major overhaul
## DESCRIPTION: This script addresses only station coverage, trends

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
  
  
######## Examine fits ** look at TrendMethodComparison.R
  
###############################################################################
###### A1.1: Analyze trends over different periods ############################
library(Kendall)
library(zyp)  # or library(trend)

## Convert slope m to lapse rate by changing sign:
# but note d(DTR/dz)dt is not very interpretable
coefsy[,grep(".m", names(coefsy), fixed=T)]<-(coefsy[,grep(".m", names(coefsy), fixed=T)])*(-1)

######## Trends over 1905-2017: ######
## But note I am not propagating my error here..
## T(z=0): b
senmax0.100<-zyp.sen(Tmax.b~Year, coefsy); ci.senmax0.100<-confint(senmax0.100) 
senmin0.100<-zyp.sen(Tmin.b~Year, coefsy); ci.senmin0.100<-confint(senmin0.100)
senavg0.100<-zyp.sen(Tavg.b~Year, coefsy); ci.senavg0.100<-confint(senavg0.100)
sendtr0.100<-zyp.sen(DTR.b~Year, coefsy); ci.sendtr0.100<-confint(sendtr0.100)

## Lapse rates: -m, -dT/dz  (should be opposite in sign)
senmaxlapse.100<-zyp.sen(Tmax.m~Year, coefsy); ci.senmaxlapse.100<-confint(senmaxlapse.100) 
senminlapse.100<-zyp.sen(Tmin.m~Year, coefsy); ci.senminlapse.100<-confint(senminlapse.100)
senavglapse.100<-zyp.sen(Tavg.m~Year, coefsy); ci.senavglapse.100<-confint(senavglapse.100)
sendtrlapse.100<-zyp.sen(DTR.m~Year, coefsy); ci.sendtrlapse.100<-confint(sendtrlapse.100)

sentrends.100<-list(senmax0.100, senmin0.100, senavg0.100, sendtr0.100,
                    senmaxlapse.100, senminlapse.100, senavglapse.100, sendtrlapse.100)
senCI.100<-list(ci.senmax0.100, ci.senmin0.100, ci.senavg0.100, ci.sendtr0.100,
                ci.senmaxlapse.100, ci.senminlapse.100, ci.senavglapse.100, ci.sendtrlapse.100)

trend.100<-data.frame(trend=unlist(lapply(sentrends.100, function(x) coef(x)["Year"])),
                     CI=matrix(unlist(lapply(senCI.100, function(x) x["Year",])), 
                               ncol=2, byrow=T),
                     metric=rep(c("T(z=0)", "-dT/dz"), each=4),
                     start="1905",
                     measure=rep(c("Tmax", "Tmin", "Tavg", "DTR"), times=2))
names(trend.100)[2:3]<-c("CI.025", "CI.975")

###### Trends over 1958-2017: ###### 
coefsy50<-subset(coefsy, coefsy$Year>=1958)
## T(z=0): b
senmax0.50<-zyp.sen(Tmax.b~Year, coefsy50); ci.senmax0.50<-confint(senmax0.50) 
senmin0.50<-zyp.sen(Tmin.b~Year, coefsy50); ci.senmin0.50<-confint(senmin0.50)
senavg0.50<-zyp.sen(Tavg.b~Year, coefsy50); ci.senavg0.50<-confint(senavg0.50)
sendtr0.50<-zyp.sen(DTR.b~Year, coefsy50); ci.sendtr0.50<-confint(sendtr0.50)

## Lapse rates: -m, -dT/dz (should be opposite in sign)
senmaxlapse.50<-zyp.sen(Tmax.m~Year, coefsy50); ci.senmaxlapse.50<-confint(senmaxlapse.50) 
senminlapse.50<-zyp.sen(Tmin.m~Year, coefsy50); ci.senminlapse.50<-confint(senminlapse.50)
senavglapse.50<-zyp.sen(Tavg.m~Year, coefsy50); ci.senavglapse.50<-confint(senavglapse.50)
sendtrlapse.50<-zyp.sen(DTR.m~Year, coefsy50); ci.sendtrlapse.50<-confint(sendtrlapse.50)

sentrends.50<-list(senmax0.50, senmin0.50, senavg0.50, sendtr0.50,
                    senmaxlapse.50, senminlapse.50, senavglapse.50, sendtrlapse.50)
senCI.50<-list(ci.senmax0.50, ci.senmin0.50, ci.senavg0.50, ci.sendtr0.50,
                ci.senmaxlapse.50, ci.senminlapse.50, ci.senavglapse.50, ci.sendtrlapse.50)

trend.50<-data.frame(trend=unlist(lapply(sentrends.50, function(x) coef(x)["Year"])),
                    CI=matrix(unlist(lapply(senCI.50, function(x) x["Year",])), 
                              ncol=2, byrow=T),
                    metric=rep(c("T(z=0)", "-dT/dz"), each=4),
                    start="1958",
                    measure=rep(c("Tmax", "Tmin", "Tavg", "DTR"), times=2))
names(trend.50)[2:3]<-c("CI.025", "CI.975")
sentrend<-rbind(trend.100, trend.50)

write.csv(sentrend, file="SenTrends_2017_1905st_1958st.csv", row.names=F)
## SEE SenTrends.xlsx

###############################################################################
############### A1.2 Plotting time series (ANNUAL) #######################################
Tmax.exp<-expression("T"["max, z=0"])
Tmin.exp<-expression("T"["min, z=0"])
Tavg.exp<-expression("T"["avg, z=0"])
DTR.exp<-expression("DTR"[" z=0"])

Tmax.tr<-coef(senmax0.100)[2]*10 #100-year trend, convert to per decade
Tmin.tr<-coef(senmin0.100)[2]*10
Tavg.tr<-coef(senavg0.100)[2]*10
DTR.tr<-coef(sendtr0.100)[2]*10

Tmax.tr100.leg<-as.expression(bquote(+.(round(Tmax.tr, 2))~degree*C~dec^-1))
Tmin.tr100.leg<-as.expression(bquote(+.(round(Tmin.tr, 2))~degree*C~dec^-1))
Tavg.tr100.leg<-as.expression(bquote(+.(round(Tavg.tr, 2))~degree*C~dec^-1))
DTR.tr100.leg<-as.expression(bquote(.(round(DTR.tr, 2))~degree*C~dec^-1))

Tmax.tr50<-coef(senmax0.50)[2]*10 #50-year trend, convert to per decade
Tmin.tr50<-coef(senmin0.50)[2]*10
Tavg.tr50<-coef(senavg0.50)[2]*10
DTR.tr50<-coef(sendtr0.50)[2]*10

Tmax.tr50.leg<-as.expression(bquote(+.(round(Tmax.tr50, 2))~degree*C~dec^-1))
Tmin.tr50.leg<-as.expression(bquote(+.(round(Tmin.tr50, 2))~degree*C~dec^-1))
Tavg.tr50.leg<-as.expression(bquote(+.(round(Tavg.tr50, 2))~degree*C~dec^-1))
DTR.tr50.leg<-as.expression(bquote(.(round(DTR.tr50, 2))~degree*C~dec^-1))

par(op)
par(mfrow=c(2,2), mar=c(2,5,2,1))
# Tmax
with(coefsy, plot(Tmax.b~Year, type="l", col="red",lwd=2, 
                  ylim=c(mean(coefsy$Tmax.b)-1.5, mean(coefsy$Tmax.b)+1.5),
                  ylab=Tmax.exp, cex.lab=1.5),
     axis(1, at=seq(1900, 2020,5)))
abline(coef(senmax0.100), col="red", lty=2, lwd=2)
legend("topleft", bty="n", legend=Tmax.tr100.leg, lty=2, lwd=2,col="red")

# Tmin  
with(coefsy, plot(Tmin.b~Year, type="l", col="blue",lwd=2,
                  ylim=c(mean(coefsy$Tmin.b)-1.5, mean(coefsy$Tmin.b)+1.5),
                  ylab=Tmin.exp, cex.lab=1.5))
abline(coef(senmin0.100), col="blue", lty=2, lwd=2)
legend("topleft", bty="n", legend=Tmin.tr100.leg, lty=2, lwd=2,col="blue")

# Tavg
with(coefsy, plot(Tavg.b~Year, type="l", col="black",lwd=2,
                  ylim=c(mean(coefsy$Tavg.b)-1.5, mean(coefsy$Tavg.b)+1.5),
                  ylab=Tavg.exp, cex.lab=1.5))
abline(coef(senavg0.100), col="black", lty=2, lwd=2)
legend("topleft", bty="n", legend=Tavg.tr100.leg, lty=2, lwd=2,col="black")
  
# DTR
with(coefsy, plot(DTR.b~Year, type="l", col="darkgray",lwd=2,
                  ylab=DTR.exp, cex.lab=1.5))
abline(coef(sendtr0.100), col="darkgray", lty=2, lwd=2)
legend("bottomleft", bty="n", legend=DTR.tr100.leg, lty=2, lwd=2,col="darkgray")
  
###############################################################################
############### A1.3 Plotting time series (by MONTH) #######################################

# USE MONTHLY SERIES
monthly<-ts(coefs[,-1], frequency=12, start=c(st.ts,1), end=c(nd.ts,12))

# Extract month-specific time series: Jans, Febs, etc
months.l<-list()
cyc<-cycle(monthly)
for (i in 1:12){
  months.l[[i]]<-monthly[cyc==i,c("Tmax.b", "Tmin.b", "Tavg.b",  "DTR.b")]
}
names(months.l)<-month.abb
months.ts<-lapply(months.l, FUN=ts, start=st.ts, end=nd.ts, frequency=1)

# Make a series of graphs of the raw annual time series and trends
pdf("TimeSeriesbyMonth_lm_2017_1905.pdf")
par(mfrow=c(6,2), mar=c(2,4,2,2))

sen.mo.max<-list()
sen.mo.min<-list()
for (i in 1:12){
  x<-as.numeric(time(months.ts[[i]]))
  
  ymax<-as.numeric(months.ts[[i]][,"Tmax.b"])
  sen.mo.max[[i]]<-zyp.sen(ymax~x)
  plot(months.ts[[i]][,"Tmax.b"],
       main=paste0(names(months.ts)[i], ", ", 
                   round((sen.mo.max[[i]]$coefficients[2])*10, 3),
                   " C/decade"),
       ylab=Tmax.exp, col="red", cex.lab=1.2)
  abline(sen.mo.max[[i]]$coefficients, lty=2)
  
  ymin<-as.numeric(months.ts[[i]][,"Tmin.b"])
  sen.mo.min[[i]]<-zyp.sen(ymin~x)
  plot(months.ts[[i]][,"Tmin.b"],
       main=paste0(names(months.ts)[i], ", ", 
                   round((sen.mo.min[[i]]$coefficients[2])*10, 3),
                   " C/decade"),
       ylab=Tmin.exp,col="blue", cex.lab=1.2)
  abline(sen.mo.min[[i]]$coefficients, lty=2)
}
dev.off()

######### develop figure for manuscript: monthly long term trends
# Calculate sen slope
mos<-time(months.ts)  # for the whole 100-year period
xtime<-st.ts:nd.ts
tmax0<-lapply(months.ts, function(x){x[,"Tmax.b"]}) # extract Tmax.b by month
tmin0<-lapply(months.ts, function(x){x[,"Tmin.b"]}) # extract Tmin.b by month
tavg0<-lapply(months.ts, function(x){x[,"Tavg.b"]}) # extract Tavg.b by month
#dtr0<-lapply(months.ts, function(x){x[,"DTR.b"]}) # extract DTR.b by month

mo.trend<-function(y, xtime){zyp.sen(y~xtime)}

Tmax0.zyp<-lapply(tmax0, mo.trend, xtime=xtime)
Tmax0.tr<-lapply(Tmax0.zyp, coef)
Tmax0.95ci<-lapply(Tmax0.zyp, confint.zyp)

Tmin0.zyp<-lapply(tmin0, mo.trend, xtime=xtime)
Tmin0.tr<-lapply(Tmin0.zyp, coef)
Tmin0.95ci<-lapply(Tmin0.zyp, confint.zyp)

# Combine into dataframe and convert to degrees per decade
MinMaxTrend<-data.frame(
  month=month.abb,
  Tmax0.tr=matrix(unlist(Tmax0.tr), byrow=T, ncol=2)[,2]*10,
  Tmax0.lo=matrix(unlist(Tmax0.95ci), byrow=T, ncol=4)[,2]*10,
  Tmax0.up=matrix(unlist(Tmax0.95ci), byrow=T, ncol=4)[,4]*10,
  Tmin0.tr=matrix(unlist(Tmin0.tr), byrow=T, ncol=2)[,2]*10,
  Tmin0.lo=matrix(unlist(Tmin0.95ci), byrow=T, ncol=4)[,2]*10,
  Tmin0.up=matrix(unlist(Tmin0.95ci), byrow=T, ncol=4)[,4]*10)

pdf("TrendsbyMonth_Sen_1905_2017.pdf")
par(mfrow=c(1,1), mar=c(4,5,4,2))
plot(MinMaxTrend$Tmax0.tr, pch=0, cex=2.5,
     ylim=c(-0.05, max(MinMaxTrend$Tmin0.up)),
     ylab=expression(Trend~(degree*C~decade^-1)), 
     xlab="Month", xaxt = "n",
     main="Temperature trends near sea level, 1905-2017")
abline(0,0)
segments(1:12, MinMaxTrend$Tmax0.lo, 
         1:12, MinMaxTrend$Tmax0.up,
         col="red",lwd=3) 
points(1:12+0.1, MinMaxTrend$Tmin0.tr, pch=0, cex=2.5)
segments(1:12+0.1, MinMaxTrend$Tmin0.lo, 
         1:12+0.1, MinMaxTrend$Tmin0.up,
         col="blue",lwd=3) 
axis(side=1, at=1:12, labels=month.abb)
legend(x="bottom", ncol=2, bty="n",
       legend=c(Tmax.exp, Tmin.exp),
       lwd=3, col=c("red", "blue"))
dev.off()

##################################################################
####### Making a master figure
pdf("Fig3_Sen_1905_2017e.pdf")
mat<-matrix(data=c(1:4,5,5), nrow=3, ncol=2, byrow=T) 
layout(mat)

# Tmax
par(fig=c(0,0.5,0.7,1), mar=c(0,5,0,0), oma=c(1,1,3,1))
with(coefsy, plot(Tmax.b~Year, type="l", col="red",lwd=2, 
                  ylim=c(mean(Tmax.b)-1.5, mean(Tmax.b)+1.5),
                  ylab=Tmax.exp, cex.lab=2, cex.axis=1.3, xaxt='n'))
axis(side=1, at=seq(1910, 2020, by=10), labels=F, tick=T)
mtext(text="Temperature trends near sea level, 1905-2017, 1958-2017", outer=T,
      side=3, line=0.6)

abline(coef(senmax0.100), col="red", lty=2, lwd=2)
legend("topleft", bty="n", 
       legend=Tmax.tr100.leg, # non-signif: Tmax.tr50.leg
       lty="33", lwd=2,
       col="red", cex=1.2)
usr <- par("usr")
clip(x1=1958, usr[2], usr[3], usr[4])
abline(coef(senmax0.50), col="black", lty="12", lwd=2)


# Tmin  
par(fig=c(0.5,1,0.7,1), new=T, mar=c(0,0,0,5), oma=c(1,1,3,1))
with(coefsy, 
     plot(Tmin.b~Year, type="l", col="blue",lwd=2,
                  ylim=c(mean(Tmin.b)-1.5, mean(Tmin.b)+1.5),
                  xaxt='n', yaxt='n'))
axis(side=1, at=seq(1910, 2020, by=10), labels=F, tick=T)
axis(side=4, at=seq(18.5, 21.5, by=0.5), labels=T, tick=T, cex.axis=1.3)
mtext(side=4, text=Tmin.exp, cex=1.3, line=4)

abline(coef(senmin0.100), col="blue", lty="33", lwd=2)
legend("topleft", bty="n", 
       legend=c(Tmin.tr100.leg, Tmin.tr50.leg), 
       lty=c("33","12"), lwd=2,
       col=c("blue", "black"), cex=1.2)
usr <- par("usr")
clip(x1=1958, usr[2], usr[3], usr[4])
abline(coef(senmin0.50), col="black", lty="12", lwd=2)


# Tavg
par(fig=c(0,0.5,0.4,0.7), new=T, mar=c(0,5,0,0), oma=c(1,1,3,1))
with(coefsy, plot(Tavg.b~Year, type="l", col="black",lwd=2,
                  ylim=c(mean(Tavg.b)-1.5, mean(Tavg.b)+1.5),
                  ylab=Tavg.exp, cex.lab=2, cex.axis=1.3, xaxt='n'))
axis(side=1, at=seq(1910, 2020, by=10), labels=F, tick=T)
axis(side=1, at=seq(1920, 2020, by=20), labels=T, tick=T)

abline(coef(senavg0.100), col="black", lty="33", lwd=2)
legend("topleft", bty="n", 
       legend=c(Tavg.tr100.leg, Tavg.tr50.leg), 
       lty=c("33","12"), lwd=2,
       col=c("black", "gray"), cex=1.2)
usr <- par("usr")
clip(x1=1958, usr[2], usr[3], usr[4])
abline(coef(senavg0.50), col="gray", lty="12", lwd=2)

# DTR
par(fig=c(0.5,1,0.4,0.7), new=T, mar=c(0,0,0,5), oma=c(1,1,3,1))
with(coefsy, plot(DTR.b~Year, type="l", col="darkgray",lwd=2,
                  xaxt='n', yaxt='n'))
axis(side=1, at=seq(1910, 2020, by=10), labels=F, tick=T)
axis(side=1, at=seq(1920, 2020, by=20), labels=T, tick=T)
axis(side=4, at=seq(6.5, 9.5, by=0.5), labels=T, tick=T, cex.axis=1.3)
mtext(side=4, text=DTR.exp, cex=1.3, line=4)

abline(coef(sendtr0.100), col="darkgray", lty="33", lwd=2)
legend("bottomleft", bty="n", 
       legend=c(DTR.tr100.leg, DTR.tr50.leg), 
       lty=c("33","12"), lwd=2,
       col=c("gray", "black"), cex=1.2)

usr <- par("usr")  # recent trend
clip(x1=1958, usr[2], usr[3], usr[4])
abline(coef(sendtr0.50), col="black", lty="12", lwd=2)

# Monthly trends
par(fig=c(0,0.5,0,0.4), new=T, mar=c(2,5,3,0.1), oma=c(1,1,3,1))
month.A<-substr(month.abb, start=1, stop=1)

plot(MinMaxTrend$Tmax0.tr, pch=0, cex=1.5,
     ylim=c(-0.05, max(MinMaxTrend$Tmin0.up)),
     ylab=expression(Trend[1905-2017]~(degree*C~dec^-1)), 
     xlab="Month", xaxt = "n", cex.lab=1.5, cex.axis=1.3)
abline(0,0)
segments(1:12, MinMaxTrend$Tmax0.lo, 
         1:12, MinMaxTrend$Tmax0.up,
         col="red",lwd=3) 
points(1:12+0.1, MinMaxTrend$Tmin0.tr, pch=0, cex=1.5)
segments(1:12+0.1, MinMaxTrend$Tmin0.lo, 
         1:12+0.1, MinMaxTrend$Tmin0.up,
         col="blue",lwd=3) 
axis(side=1, at=1:12, labels=month.A)
legend(x="bottom", ncol=2, bty="n",
       legend=c(Tmax.exp, Tmin.exp), cex=1.2,
       lwd=3, col=c("red", "blue"))


# Lapse rate trend, Tmax 1958-2017
par(fig=c(0.5,1,0,0.4), new=T, mar=c(2,0.1,3,5), oma=c(1,1,3,1), new=T)

with(coefsy[coefsy$Year>=1958,], 
     plot(Tmax.m*1000~Year, type="l", col="red",lwd=2, 
          xaxt='n', yaxt='n', xlim=c(1905, 2017), ylim=c(5.6, 9.6))) #

with(coefsy[coefsy$Year>=1958,], 
     lines(Tavg.m*1000~Year, col="black",lwd=2, xlim=c(1905, 2017)), new=T) #

with(coefsy[coefsy$Year>=1958,], 
     lines(Tmin.m*1000~Year, col="blue",lwd=2, xlim=c(1905, 2017)), new=T) #

axis(side=1)
axis(side=4, labels=T, tick=T, cex.axis=1.3)
mtext(side=4, text=expression(-dT/dz~(degree*C~km^-1)), 
      cex=1.2, line=4)
# Set up for legend
tmax.lapse<-coef(senmaxlapse.50)[2]*1000*10
tmax.lapse.leg<-as.expression(bquote(T[max]:~+.(round(tmax.lapse, 2))~degree*C~km^-1~dec^-1))
tavg.lapse<-coef(senavglapse.50)[2]*1000*10
tavg.lapse.leg<-as.expression(bquote(T[avg]:~+.(round(tavg.lapse, 2))~degree*C~km^-1~dec^-1))

legend("topleft", bty="n", 
       legend=c(tmax.lapse.leg, tavg.lapse.leg), 
       lty=3, lwd=2, col=c("red", "black"), cex=1.2)

usr <- par("usr")  # recent trend
clip(x1=1958, usr[2], usr[3], usr[4])
abline(coef(senmaxlapse.50)*1000, col="red", lty="12", lwd=2)
abline(coef(senavglapse.50)*1000, col="black", lty="12", lwd=2)
abline(coef(senminlapse.50)*1000, col="blue", lty="12", lwd=2)

dev.off()