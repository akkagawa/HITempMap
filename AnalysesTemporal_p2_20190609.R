## AnalysesTemporal_p2_20190609.R (formerly AnalysesJul2017Temporal.R, AnalysesJun2017Temporal.R, AnalysesApr2016v4, Analyses.R)
## Author: Aurora Kagawa-Viviani
## Date: Apr 18-20 2016, modified and rerun 6/23/2017, 6/9/2019
##   revised for cloud/wind 7/20/2017, finalizing ts analyses 9/2017
## Notes: Script for running analyses on time series

##################################################################
## Analysis 2: Time Series comparisons
##  Compare to MEI, PDO, IPO, RFI from RF atlas
setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping")
op<-par() # save default

Tmax.exp<-expression("T"["max"])
Tmin.exp<-expression("T"["min"])
Tavg.exp<-expression("T"["avg"])

deg.exp<-expression(~degree*C)
######## Read in fitted temperature coeffs
coefs<-read.csv("monthly_lm_2017.csv")
coefsy<-read.csv("annual_lm_2017.csv")

######## Read and create climate indices time series ##############
# Revised 7/20/2017, 9/2017
# index<-read.csv("ENSO/indices2016.csv")  #monthly regional climate indices through 2016
# hnl<-read.csv("NCDC_DailyTemp/CloudWind/HNLCloudWind_narmMonthly.csv")

index<-read.csv("WindCloud/MonthlyTimeSeries_2017.csv") # new master time series

####### Merge into a master monthly time series
head(coefs, 1)
tail(coefs,1)
coefs.st<-c(1905, 1)
coefs.nd<-c(2017, 12)
coefs.ts<-ts(coefs, start=coefs.st, end=coefs.nd, frequency=12)

head(index, 1)
tail(index,1)
index.st<-c(1905, 1)
index.nd<-c(2017, 12)
index.ts<-ts(index, start=index.st, end=index.nd, frequency=12)

master<-ts.union(coefs.ts, index.ts)
colnames(master)<-c(colnames(coefs.ts), colnames(index.ts))

# identify and remove extraneous timestamp columns
# we can extract years, months after using cycle()
xtracol<- grepl(".YEAR", colnames(master), fixed=T)|    
  grepl("Month", colnames(master), fixed=T)| 
  grepl("MO", colnames(master), fixed=T)|
  colnames(master)=="X"
master<-master[,!xtracol]  

#### PLOT all time series (raw series)
pdf("TimeSeriesMonthlyRaw_TS.pdf")
par(mfrow=c(7, 4), mar=c(2,2,2,2))
for (i in 1:ncol(master)){
  plot(master[,i], main=colnames(master)[i])
}
dev.off()

par(mfcol=c(3,3), mar=c(2,3.5,2,1), mgp=c(2.2,1,0))
  plot(master[,"coefs_Tmax.b"], col="red", main=Tmax.exp, ylab="intercept")
  plot(master[,"coefs_Tmax.m"], ylab="slope")
  plot(master[,"R2_Tmax"], ylab=expression("R"^2))
  
  plot(master[,"coefs_Tmin.b"], col="blue", main=Tmin.exp, ylab="intercept")
  plot(master[,"coefs_Tmin.m"], ylab="slope")
  plot(master[,"R2_Tmin"], ylab=expression("R"^2))
  
  plot(master[,"coefs_Tavg.b"], main=Tavg.exp, ylab="intercept")
  plot(master[,"coefs_Tavg.m"], ylab="slope")
  plot(master[,"R2_Tavg"], ylab=expression("R"^2))
  
#### PLOT histograms (raw series)
pdf("TimeSeriesMonthlyRaw_Hist.pdf")
par(mfrow=c(7, 4), mar=c(2,2,2,2))
for (i in 1:ncol(master)){
  hist(master[,i], main=colnames(master)[i])
}
dev.off()
  ## ASSESSMENT:
  ### RFI should be transformed
  ### Cloud proportion/percents should be transformed, sqrt() appears best

###### Clean up various time series
## Merge mei and meiX (is this legit?) since MEI starts 1950
## which MEI are NA? replace with MEIX
meiNAl<-(is.na(master[,"indices.mei"])) 
MEI<-c(master[meiNAl,"indices.meiX"], master[!meiNAl,"indices.mei"])

## Log-transform RFI
lnrfi<-log(master[,"indices.rfi"])
par(mfrow=c(2,1))
plot(master[,"indices.rfi"], main="RFI")
plot(lnrfi, main="ln(RFI)")

## Square root transform cloud cover
sqtcc<-sqrt(master[,"windcloud.tcc"])
sqlcc<-sqrt(master[,"windcloud.lcc"])
sqmcc<-sqrt(master[,"windcloud.mcc"])
sqhcc<-sqrt(master[,"windcloud.hcc"])
sqacsh<-sqrt(master[,"HNLcw.ACSH.pct"]/100)
sqacmh<-sqrt(master[,"HNLcw.ACMH.pct"]/100)

## Convert wind to m/s
awnd<-master[,"HNLcw.AWND.tenthms"]/10

## Combine into a master TRANSFORMED dataset
mastertr<-cbind(master, MEI, lnrfi, 
              sqtcc, sqlcc, sqmcc, sqhcc, 
              sqacsh, sqacmh, awnd)
colnames(mastertr)<-c(colnames(master), colnames(mastertr)[29:37])

## Subset out only the time series for further analyses
all<-list(Tmax0=mastertr[,"coefs_Tmax.b"],
          Tmin0=mastertr[,"coefs_Tmin.b"],
          Tavg0=mastertr[,"coefs_Tavg.b"], #disregard DTR
          sst=mastertr[,"indices.sst"],
          lnrfi=mastertr[,"lnrfi"],
          waswind=mastertr[,"windcloud.waswind"],
          awnd=mastertr[,"awnd"],
          si10=mastertr[,"windcloud.si10"],
          sqtcc=mastertr[,"sqtcc"],
          sqhcc=mastertr[,"sqhcc"],
          sqmcc=mastertr[,"sqmcc"],
          sqlcc=mastertr[,"sqlcc"],
          sqacsh=mastertr[,"sqacsh"],
          sqacmh=mastertr[,"sqacmh"],
          MEI=mastertr[,"MEI"],
          PDO=mastertr[,"indices.pdo"],
          IPO=mastertr[,"indices.ipo"])
#### PLOT scatterplot matrix (raw series, NOT DETRENDED or iid)
pdf("TimeSeriesMonthlyRaw_ScPlMat.pdf")
all.df<-as.data.frame(all) 
plot(all.df, pch=".", main="Raw Series")
dev.off()

#################################################################
########### MAKE STATIONARY BY DETRENDING #######################
## remove seasonality, detrend, THEN compare to climate indices #############

all.ts<-ts(all.df,                 # make mts
           start=start(mastertr), 
           end=end(mastertr), 
           frequency=frequency(mastertr))

# VISUALIZE SEASONAL CYCLE and variability in this
pdf("MeanSeasonalCycles.pdf")
par(mfrow=c(6,3), mar=c(2,2,2,2))
for (i in 1:length(all)){
  cyc<-tapply(all.ts[,i], INDEX=cycle(all.ts), FUN=mean, na.rm=T)
  sds<-tapply(all.ts[,i], INDEX=cycle(all.ts), FUN=sd, na.rm=T)
  plot(cyc, main=names(all)[i], type="l", 
       ylim=c(min(cyc-sds), max(cyc+sds)))
  lines(cyc+sds, col="blue", lty=2)
  lines(cyc-sds, col="blue", lty=2)
}
dev.off()

###### REMOVE SEASONALITY
## Try stl(), Seasonal Decomposition of Time Series by Loess (more sophisticated)
# stl() returns objects including a "time.series" matrix with 
#  seasonal, trend, remainder
stl4list<-function(x){
  stlx<-stl(x, s.window="periodic", t.window=37)  # filter 3 year = 37 mos
  plot(stlx, main=x$names)
  return(stlx)}

# To decompose using stl() and decompose(), we need to properly window
#  the time series so there are no NAs

tsperiods<-read.csv("TimeSeriesPeriods2017.csv", stringsAsFactors = T)
all.windowed<-list()
for (i in 1:length(all)){
  pd<-grep(names(all)[i], tsperiods$Time.series, fixed=TRUE)
  all.windowed[[i]]<-window(all[[i]], 
                            start=c(tsperiods$Start.YEAR[pd], tsperiods$Start.MONTH[pd]),
                            end=c(tsperiods$End.YEAR[pd], tsperiods$End.MONTH[pd]),
                            frequency=frequency(all[[i]]))
  print(paste(names(all)[i],
              tsperiods$Time.series[pd], 
              any(is.na(all.windowed[[i]])),
              sep=","))
}
names(all.windowed)<-names(all)
#stls<-lapply(all.windowed, stl4list) 
## missing values for awnd!
ms<-which(is.na(all.windowed$awnd))
for (i in i:length(ms)){
  interp<-mean(all.windowed$awnd[c(ms[i]-1, ms[i]+1)])
  all.windowed$awnd[ms]<-interp
}

## Now apply stl() to all windowed time series in the list
#stls<-lapply(all.windowed, stl4list)  #to have titles, convert this to a loop
pdf("TimeSeriesMonthly_STL2017.pdf")
stls<-list()
for (i in 1:length(all.windowed)){
  stls[[i]]<-stl(all.windowed[[i]], 
                 s.window="periodic",
                 t.window=37)  # filter 3 year = 37 mos
  plot(stls[[i]], main=names(all.windowed)[i])
}
dev.off()
names(stls)<-names(all.windowed)


## Now try another method, decompose(), 
## Classical Seasonal Decomposition by Moving Averages (less sophisticated)
decomp<-lapply(all.windowed, decompose, type="additive")

## Compare two decomposition outputs
par(mfcol=c(3,2), mar=c(0,2,3,2))
stls$Tmin0$time.series[1:12,"seasonal"] 
plot(stls$Tmin0$time.series[,1], main="stl(Tmin0)") # seasonal
plot(stls$Tmin0$time.series[,2]) # trend
plot(stls$Tmin0$time.series[,3]) # remainder

decomp$Tmin0$seasonal[1:12]
plot(decomp$Tmin0$seasonal, main="decompose(Tmin0)")
plot(decomp$Tmin0$trend)
plot(decomp$Tmin0$random)

##--> They are slightly different!
##--> They both calculate a trend by smoothing/averaging

### Remove seasonality in all time series
# Sum stl object "trend" + remainder and analyze in time domain 
all.ns<-list()
for (i in 1:length(all.windowed)){
  all.ns[[i]]<-ts(rowSums(stls[[i]]$time.series[,2:3]), #"trend" + "remainder"
                  start=start(stls[[i]]$time.series),
                  end=end(stls[[i]]$time.series),
                  frequency=12)
}
names(all.ns)<-names(stls)

### Look at linear trends in all time series
lms<-list()
fits<-list()
resids<-list()

library(Kendall)  # Calculate Sen's slope
library(zyp)
zyp.trend<-list()

for (i in 1:length(all.ns)){
  lms[[i]]<-lm(all.ns[[i]]~time(all.ns[[i]]))
  print(names(all.ns)[i])
  print(summary(lms[[i]]))
  
  fits[[i]]<-ts(lms[[i]]$fitted.values, 
                start=start(all.ns[[i]]),
                end=end(all.ns[[i]]),
                frequency=12)
  resids[[i]]<-ts(lms[[i]]$residuals, 
                  start=start(all.ns[[i]]),
                  end=end(all.ns[[i]]),
                  frequency=12)
  zyp.trend[[i]]<-zyp.trend.vector(all.ns[[i]], time(all.ns[[i]]))
  print(zyp.trend[[i]])
}
names(lms)<-names(all.ns)
names(fits)<-names(all.ns)
names(resids)<-names(all.ns)
names(zyp.trend)<-names(all.ns)


  ## ASSESSMENT:
  ##  Increasing: Tmax0***, Tmin0***, Tavg0***, sst***, sqhcc**, (MEI***)
  ##  Decreasing: lnrfi***, waswind*, awnd*, sqmcc***, sqacsh***,sqacmh***
  ##  No trend: si10, sqtcc, sqlcc, PDO, IPO

############### LINEAR DETREND Temp, ln(RFI) and SST and others####
alldt.ls<-list(Tmax0=resids$Tmax0, # recall residuals = data-fitted.values
                Tmin0=resids$Tmin0,                    
                Tavg0=resids$Tavg0,
                sst=resids$sst,
                lnrfi=resids$lnrfi,
                waswind=resids$waswind,
                awnd=resids$awnd,
                sqacsh=resids$sqacsh,
                sqacmh=resids$sqacmh,
                sqhcc=resids$sqhcc,
                sqmcc=resids$sqmcc,
                si10=all.ns$si10-mean(all.ns$si10),   # no trend removal from here on
                sqtcc=all.ns$sqtcc-mean(all.ns$sqtcc), 
                sqlcc=all.ns$sqlcc-mean(all.ns$sqlcc), 
                MEI=all.ns$MEI, #??? should we use original series? using de-seasoned
                PDO=all.ns$PDO, 
                IPO=all.ns$IPO)

alldt.ts<-ts.union(alldt.ls[[1]], alldt.ls[[2]])
for (i in 3:length(alldt.ls)){
  alldt.ts<-ts.union(alldt.ts, alldt.ls[[i]], dframe=F)
}
colnames(alldt.ts)<-names(alldt.ls)

alldt.df<-data.frame(alldt.ts)
pdf("TimeSeriesMonthlyDetrend_ScPlMat.pdf")
plot(alldt.df, pch=".", main="Detrended")
dev.off()

imax<-grep("Tmax", colnames(alldt.ts))
imin<-grep("Tmin", colnames(alldt.ts))

for (i in 4:ncol(alldt.ts)){
  print(paste(colnames(alldt.ts)[imax], "~", colnames(alldt.ts)[i]))
  print(cor(alldt.ts[,i], alldt.ts[,imax], method="kendall", use="pairwise.complete.obs"))
  print(summary(lm(alldt.ts[,imax]~alldt.ts[,i])))
}

for (i in 4:ncol(alldt.ts)){
  print(paste(colnames(alldt.ts)[imin], "~", colnames(alldt.ts)[i]))
  print(cor(alldt.ts[,i], alldt.ts[,imin], method="kendall", use="pairwise.complete.obs"))
  print(summary(lm(alldt.ts[,imin]~alldt.ts[,i])))
}

########### CCFs
## Cross-correlation of monthly data series
ccfplot<-function(x, y, ylab="ccf", title=NA)
{if(is.na(title)==T){
  label<-c(deparse(substitute(x)), deparse(substitute(y)))
  title<-paste(label[1], "&", label[2])}
  ccfout<-ccf(x, y, ylab=ylab, main=title)
  # mtext(paste0("lag", round(ccfout$lag[which.max(abs(ccfout$acf))], 2),
  #              ", cor=", round(ccfout$acf[which.max(abs(ccfout$acf))], 2)))
  
  mtext(paste0("lag", round(ccfout$lag[which.max(abs(ccfout$acf))], 2),
               ", cor=", round(ccfout$acf[which.max(abs(ccfout$acf))], 2),
               ", lag0, cor=", round(ccfout$acf[ccfout$lag==0], 2)))
}


names(tsperiods)<-c("series", "st.y", "st.m", "end.y", "end.m")
alldt.2017<-window(alldt.ts, end=c(2017,12)) # temp series ends 2017

pdf("TimeSeriesMonthlyDetrend_CCF_0.pdf")
par(mfrow=c(5,2), mar=c(2,3,3,2))
for (i in 4:ncol(alldt.2017)){
  r<-grep(colnames(alldt.2017)[i], tsperiods$series, fixed=TRUE)
  x<-window(alldt.2017, start=c(tsperiods$st.y[r], tsperiods$st.m[r]),
            end=c(tsperiods$end.y[r], tsperiods$end.m[r]), frequency=12)
  ccfplot(x[,imax], x[,i], title=paste(colnames(alldt.2017)[i], "Tmax", sep="&"))
  ccfplot(x[,imin], x[,i], title=paste(colnames(alldt.2017)[i], "Tmin", sep="&"))
}
dev.off()

# examine this output! Need some help with this




####### MAKE STATIONARY by DIFFERENCING (seasonal differencing) ##################
# This does not assume a particular seasonal pattern

all.tsf<-ts.union(all.ts, all.windowed$awnd) #fixed for missing values of awnd
all.tsf[,"all.ts.awnd"]<-all.tsf[,"all.windowed$awnd"]
all.tsf<-all.tsf[,-ncol(all.tsf)]
colnames(all.tsf)<-colnames(all.ts)

all.sdiff<-diff(all.tsf, lag=12, differences=1)    # seasonal difference
all.dsdiff<-diff(all.sdiff, lag=1, differences=1) # first diff of sdiff

alldsd.df<-as.data.frame(all.dsdiff) # make into dataframe
pdf("TimeSeriesMonthlyDiff_ScPlMat.pdf")
plot(alldsd.df, pch=".", main="Differenced")                      # plot all combinations
dev.off()

for (i in 4:ncol(all.dsdiff)){
  print(paste(colnames(all.dsdiff)[imax], "~", colnames(all.dsdiff)[i]))
  print(cor(all.dsdiff[,i], all.dsdiff[,imax], method="kendall", use="pairwise.complete.obs"))
  print(summary(lm(all.dsdiff[,imax]~all.dsdiff[,i])))
}

for (i in 4:ncol(all.dsdiff)){
  print(paste(colnames(all.dsdiff)[imin], "~", colnames(all.dsdiff)[i]))
  print(cor(all.dsdiff[,i], all.dsdiff[,imin], method="kendall", use="pairwise.complete.obs"))
  print(summary(lm(all.dsdiff[,imin]~all.dsdiff[,i])))
}

all.dsdiff.2017<-window(all.dsdiff, end=c(2017,12)) # temp series ends 2017

pdf("TimeSeriesMonthlyDiff_CCF_0.pdf")
par(mfrow=c(5,2), mar=c(2,3,3,2))
for (i in 4:ncol(all.dsdiff.2017)){
  r<-grep(colnames(all.dsdiff.2017)[i], tsperiods$series, fixed=TRUE)
  x<-window(all.dsdiff.2017, start=c(tsperiods$st.y[r]+1, tsperiods$st.m[r]+1),
            end=c(tsperiods$end.y[r], tsperiods$end.m[r]), frequency=12)
  ccfplot(x[,imax], x[,i], title=paste(colnames(all.dsdiff.2017)[i], "Tmax", sep="&"))
  ccfplot(x[,imin], x[,i], title=paste(colnames(all.dsdiff.2017)[i], "Tmin", sep="&"))
}
dev.off()

# examine this output and compare with detrended patterns

# ######################################################
# # USE ANNUAL SERIES
# head(coefsy, 1)
# tail(coefsy, 1)
# annual<-ts(coefsy[,-c(1:2)], start=1905, end=2017)
# plot(annual[,"coefs_Tmax.b"]); acf(annual[,"coefs_Tmax.b"])
# plot(diff(annual[,"coefs_Tmax.b"])); acf(diff(annual[,"coefs_Tmax.b"]))
# 
# # Calculate an annual series for every variable
# annual.all<-data.frame(matrix(ncol=length(all)+1, nrow=nrow(annual)))
# names(annual.all)<-c("Year", names(all))
# annual.all$Year<-time(annual)   # Year column
# annual.all[,2:4]<-annual[,c("coefs_Tmax.b", "coefs_Tmin.b", "coefs_Tavg.b")]
# all.tsf.2017<-window(all.tsf, end=c(2017,12)) #set up to calculate annual values from monthly
# 
# years<-rep(1905:2017, each=12)
# for (i in 5:(ncol(annual.all))){
#   annual.all[,i]<-tapply(all.tsf.2017[,i-1], years, mean)
# }
# 
# # Make a scatterplot matrix
# pdf("TimeSeriesAnnualRaw_ScPlMat.pdf")
# plot(annual.all, pch="*", main="Annual raw series") # just for fun (not iid)
# dev.off()
# 
# # Extract month-specific time series: Jans, Febs, etc
# months.l<-list()
# cyc<-cycle(all.tsf.2017)
# for (i in 1:12){
#   months.l[[i]]<-all.tsf.2017[cyc==i,c("Tmax0", "Tmin0", "Tavg0")]
# }
# names(months.l)<-month.abb
# months.ts<-lapply(months.l, FUN=ts, start=1905, end=2017, frequency=1)
# 
# # Make a series of graphs of the raw annual time series and trends
# pdf("TimeSeriesbyMonth_lm.pdf")
# par(mfrow=c(6,2), mar=c(2,4,2,2))
# 
# lm.max<-list()
# lm.min<-list()
# for (i in 1:12){
#   lm.max[[i]]<-lm(months.ts[[i]][,"Tmax0"]~time(months.ts[[i]]))
#   plot(months.ts[[i]][,"Tmax0"],
#        main=paste0(names(months.ts)[i], ", ", 
#                   round((lm.max[[i]]$coefficients[2])*10, 3),
#                   " C/decade"),
#        ylab=Tmax.exp, col="red")
#   abline(lm.max[[i]], lty=2)
#   
#   lm.min[[i]]<-lm(months.ts[[i]][,"Tmin0"]~time(months.ts[[i]]))
#   plot(months.ts[[i]][,"Tmin0"],
#        main=paste0(names(months.ts)[i], ", ", 
#                   round((lm.min[[i]]$coefficients[2])*10, 3),
#                   " C/decade"),
#        ylab=Tmin.exp,col="blue")
#   abline(lm.min[[i]], lty=2)
# }
# dev.off()

# # Calculate sen slope
# library(zyp)
# 
# Tmax0.tr<-zyp.sen(annual.all$Tmax0~as.numeric(annual.all$Year))
# Tmax0.tr<-zyp.sen(annual.all$Tmin0~annual.all$Year)
# 
# 
# 
# trend<-data.frame(matrix(ncol=6, nrow=12))
# names(trend)<-c("senmax0", "ci.senmax0.lower", "ci.senmax0.upper",
#                     "senmin0", "ci.senmin0.lower", "ci.senmin0.upper")
# for (i in 1:12){
#   Tmax.y<-as.numeric(months.ts[[i]][,"Tmax0"])
#   Tmax.t<-as.numeric(time(months.ts[[i]]))
#   senmax0<-zyp.sen(Tmax.y~Tmax.t)
#   ci.senmax0<-confint(senmax0)
#   
#   Tmin.y<-as.numeric(months.ts[[i]][,"Tmin0"])
#   Tmin.t<-as.numeric(time(months.ts[[i]]))
#   senmin0<-zyp.sen(Tmin.y~Tmin.t)
#   ci.senmin0<-confint(senmin0)  
#   
#   trend[i,]<-c(senmax0$coefficients[2], ci.senmax0[2,],
#                senmin0$coefficients[2], ci.senmin0[2,])
# }
# 
# print(trend)
# trend<-trend*10  #convert so we have change in degrees per decade
# 
# min(trend$ci.senmax0.lower)
# min(trend$ci.senmin0.lower)
# max(trend$ci.senmax0.upper)
# max(trend$ci.senmin0.upper)
# 
# pdf("TrendsbyMonth_Sen.pdf")
# par(mfrow=c(1,1), mar=c(4,4,4,2))
# plot(trend$senmax0, pch=0, cex=3,
#      ylim=c(-0.05, max(trend$ci.senmin0.upper)),
#      ylab="Trend, C/decade", xlab="Month",
#      main="Annual trends for each month (Sen's slope)")
# abline(0,0)
# segments(1:12, trend$ci.senmax0.lower, 
#          1:12, trend$ci.senmax0.upper,
#          col="red",lwd=3) 
# points(1:12+0.2, trend$senmin0, pch=0, cex=3)
# segments(1:12+0.2, trend$ci.senmin0.lower, 
#          1:12+0.2, trend$ci.senmin0.upper,
#          col="blue",lwd=3) 
# dev.off()


#################################################################
#################################################################
######## NOW do some Graphing! ###
## Do some filtering
f1 <-rep (1/13, 13)              # create boxcar, 1 year, centered
fs<-c(1/24, rep(1/12, 11), 1/24) # remove seasonality
pdo.f1<- stats::filter(all$PDO, f1, sides=2) 
Tmax.bf1<- stats::filter(all$Tmax0, fs, sides=2) 
Tmin.bf1<- stats::filter(all$Tmin0, fs, sides=2) 

## Graph and examine the data: PDO and temperature
par(mfrow=c(2,1))
plot(pdo.f1); abline(0,0) 
par(new=T); plot(Tmax.bf1, col="red",axes=F, ann=F)
plot(pdo.f1); abline(0,0)
par(new=T); plot(Tmin.bf1, col="blue",axes=F, ann=F, main=Tmin.exp)

###### Make better-looking graphs by filtering with a wider window! #####
## To examine decadal variability, smooth data (low-pass filtering)
f3 <-rep (1/37, 37) # create boxcar, 3 year, centered

# Smooth data using this filter
all.f3<-stats::filter(all.ts, filter=f3, sides=2)
colnames(all.f3)<-colnames(all.ts)

# Now do this for series with seasonality and trend removed
all.dt.f3<-stats::filter(alldt.ts, filter=f3, sides=2)
colnames(all.dt.f3)<-colnames(alldt.ts)

# Try another method, kernal smoothing with gaussian filter 
all.dt.g3<-lapply(X=alldt.ls, FUN=function(x){     # different lengths for each series
  ksmooth(time(x), x, bandwidth = 3, kernel="normal")})
# all.dt.g3l<-apply(X=alldt.ts, MARGIN=2, FUN=function(x){
#   ksmooth(time(x), x, bandwidth = 3, kernel="normal")})
all.dt.g3.tsl<-lapply(all.dt.g3, FUN=function(x){
  g3.ts<-ts(x$y, start=c(x$x[1],1), end=c(x$x[length(x$x)],12), frequency=12)
  return(g3.ts)})

# Thinking about DTR  
plot(all.dt.g3$Tmax0, type="l", lwd=2, col="red", ylab="Temp series minus trend")
lines(all.dt.g3$Tmin0, lwd=2, col="blue")

DTR<-all.ts[,"Tmax0"]-all.ts[,"Tmin0"]
DTR.f3<-stats::filter(DTR,filter=f3, sides=2)
DTR.g3<-ksmooth(time(DTR), DTR, bandwidth = 3, kernel="normal"); plot(DTR.g3)

DTR.dt<-alldt.ts[,"Tmax0"]-alldt.ts[,"Tmin0"]
DTR.dt.f3<-stats::filter(DTR.dt,filter=f3, sides=2)
plot(DTR.dt.f3); abline(0,0)
all.dt.f3<-ts.union(all.dt.f3,DTR.dt.f3)
colnames(all.dt.f3)<-c(colnames(alldt.ts), "dtr")

DTR.dt.g3<-ksmooth(time(DTR.dt), DTR.dt, bandwidth = 3, kernel="normal")
plot(DTR.dt.g3); abline(0,0)

all.dt.g3.tsl$dtr<-ts(DTR.dt.g3$y, 
                     start=c(DTR.dt.g3$x[1],1), 
                     end=c(DTR.dt.g3$x[length(DTR.dt.g3$x)],12), 
                     frequency=12)

########## Making pretty graphs, time domain #############
## Function: make MEI plot
MEIplot<-function(xMEI){  # provide a time series of MEI
  plot(xMEI, ylab="MEI", ylim=c(-3,3)); abline(0,0)
  fill<-as.numeric(xMEI); fill[c(1, length(xMEI))]<-0  #start and end at 0
  fillblue<-fill; fillred<-fill
  fillblue[fill>0]<-0; fillred[fill<0]<-0
  polygon(time(xMEI), fillblue, col='skyblue', border="gray30")
  polygon(time(xMEI), fillred, col='pink', border="gray30")
}

## Plot Regional Climate Indices
# ## Plot MEI AND PDO, 3 year running mean
# png(file=paste("MEI_PDO_IPO_f3y.png", sep=""), bg="transparent")
# par(mfrow=c(2,1), mar=c(2,4,2,2))
# MEIplot(all$MEI)
# lines(all.f3[,"PDO"], col="black", lwd=2)
# legend("topleft", legend=c("MEI>0", "MEI<0", "PDO"), bty="n", ncol=2,
#        lwd=c(NA,NA, 2), fill=c("pink", "skyblue", NA), border=c("gray30", "gray30", NA),
#        cex=.9, xjust=1)
# ## Plot MEI AND IPO, 3 year runing mean
# MEIplot(all$MEI)
# lines(all.f3[,"IPO"], col="red4", lwd=2)
# legend("topleft", legend=c("MEI>0", "MEI<0", "IPO"), bty="n", ncol=2,
#        lwd=c(NA,NA, 2), col="red4", 
#        fill=c("pink", "skyblue", NA), border=c("gray30", "gray30", NA),
#        cex=.9, xjust=1)
# dev.off()

## Plot Raw Tmin and Tmax series, smoothed
## Add Tmin and Tmax, detrended, 3 year running mean
dtTmax.exp<-expression(Detrended~T[max])
dtTmin.exp<-expression(Detrended~T[min])
dtDTR.exp<-expression(Detrended~DTR)

png(file=paste("MEI_Tminmax.png", sep=""), bg="transparent",
    width=1000, height=1200)
op<-par(mfrow=c(2,1), mar = c(2,4,1,4) + 0.2)
MEIplot(all$MEI)
par(new=T)
plot(all.dt.f3[,"Tmin0"], col="blue", lwd=2, 
     ylim=c(-0.8, 0.8), axes=F, ann=F)
lines(all.dt.f3[,"Tmax0"], col="red", lwd=2)
axis(4); mtext(side=4, line=2.5, 
               text=expression(Detrended~Temp~(degree*C)))
legend("bottom", legend=c("MEI>0", "MEI<0", dtTmax.exp, dtTmin.exp), bty="n",
       ncol=2, lwd=c(NA,NA, 2,2), col=c(NA, NA,"red", "blue"), 
       fill=c("pink", "skyblue", NA,NA), border=c("gray30", "gray30", NA,NA),
       xjust=0, x.intersp=0.5)


# MEIplot(all$MEI)
# par(new=T)
# plot(all.dt.f3[,"Tmax0"], col="red", lwd=2, axes=F, ann=F)
# axis(4); mtext(side=4, line=2.5, "Temp (C, stationary)", cex=0.7)
# legend("topleft", legend=c("MEI>0", "MEI<0", Tmax.exp), bty="n",
#        ncol=2, lwd=c(NA,NA, 2), col=c(NA, NA, "red"), 
#        fill=c("pink", "skyblue", NA), border=c("gray30", "gray30", NA),
#        xjust=1)
dev.off()

## Add plot for ln(RFI) (NOT detrended, just filtered)
plot(all$lnrfi, col="green", lty=2, lwd=1, ylab="ln(RFI)")
lines(all.f3[,"lnrfi"], col="darkgreen", lty=1, lwd=2)
legend("topleft", legend=c("ln(RFI) monthly", "ln(RFI) 3 year moving avg"), bty="n",
       lwd=1, col=c("green", "darkgreen"), cex=0.8, xjust=1)
plot(all.f3[,"PDO"], col="black", lwd=2)

## Add plot for SST (NOT detrended, just filtered)
plot(all$sst, col="skyblue", lty=2, lwd=1, ylab="SST")
lines(all.f3[,"sst"], col="blue", lty=1, lwd=2)
legend("topleft", legend=c("SST monthly", "SST 3 year moving avg"), bty="n",
       lwd=2, col=c("skyblue", "blue"), cex=0.8, xjust=1)

## Add a separate plot for WIND data (NOT detrended, just filtered)
plot(all$waswind, col="gray80", lty=2, lwd=1, ylab="Wind (m/s)", ylim=c(2,10))
lines(all.f3[,"waswind"], col="gray50", lty=1, lwd=2) # WASWind, 3mo moving average

lines(all$si10, col="gray40", lty=2, lwd=1)
lines(all.f3[,"si10"], col="black", lty=1, lwd=2) # ERA-Int si10, 3mo moving average
lines(all$awnd, col="skyblue", lty=2, lwd=1)
lines(all.f3[,"awnd"], col="blue", lty=1, lwd=2) # HNL AWND, 3mo moving average
legend("topleft", 
       legend=c("WASWind monthly, 3yr MA", 
                "ERA-Int si10 monthly, 3yr MA",
                "HNL AWND monthly, 3yr MA"), bty="n",
       lwd=c(2,2,2), 
       col=c("gray50", "black", "blue"), cex=0.8, xjust=1)

## Add a separate plot for CLOUD data (NOT detrended, just filtered)
plot(all.f3[,"sqtcc"], col="black", lty=1, lwd=2,    # TOTAL Cloud Cover, 3mo moving average
     ylim=c(0.25,0.65), ylab="Mean Cloud Cover Fraction") 
lines(all.f3[,"sqhcc"], col="gray80", lty=1, lwd=2)  # HIGH Cloud Cover, 3mo moving average
lines(all.f3[,"sqmcc"], col="gray50", lty=1, lwd=2)  # MEDIUM Cloud Cover, 3mo moving average
lines(all.f3[,"sqlcc"], col="gray30", lty=1, lwd=2)  # LOW Cloud Cover, 3mo moving average
par(new=T)
#plot(all$sqacmh"], col="skyblue", lty=2, lwd=1, ylim=c(0.55,0.9)
plot(all.f3[,"sqacmh"], col="darkblue", lty=1, lwd=2, 
     axes=F, ann=F, ylim=c(0.55,0.80))   # HNL ACMH, 3mo moving average
#lines(all$sqacsh, col="orange", lty=2, lwd=1)
lines(all.f3[,"sqacsh"], col="skyblue", lty=1, lwd=2)   # HNL ACMH, 3mo moving average
axis(4); mtext(side=4, line=2.5, "HNL Cloud Cover Fraction", cex=0.8)
legend("bottomleft", lwd=2, cex=0.8, xjust=1,bty="n",
       legend=c("ERA-Int sqrt(TCC) monthly, 3yr MA", 
                "ERA-Int sqrt(LCC) monthly, 3yr MA",
                "ERA-Int sqrt(MCC) monthly, 3yr MA",
                "ERA-Int sqrt(HCC) monthly, 3yr MA",
                "HNL sqrt(ACMH) monthly, 3yr MA",
                "HNL sqrt(ACSH) monthly, 3yr MA"), 
       col=c("black","gray30", "gray50", "gray80",
             "darkblue", "skyblue"))

##############################################################################
# Plot series with each other:
# RFI, SST, Wind, Cloud all with Tmax and Tmin (seasonality removed but trend present)

###### Take smaller window for wind, cloud FIX AXES
## Zoom into 1956-2017:
# all.f3short<-lapply(all.f3, FUN=window, start=c(1956,1), end=c(2017,12), frequency=12)
# all.dt.f3short<-lapply(all.dt.f3, FUN=window, start=c(1956,1), end=c(2017,12), frequency=12)

### Function to plot two series together
plt.2<-function(ts1, ts2, col1="black", col2, ylab1, ylab2){
  plot(ts1, col=col1, ylab=ylab1, cex=0.8)
  abline(0,0)
  par(new=T)
  plot(ts2, col=col2, axes=F, ann=F)
  axis(4); mtext(ylab2, side=4, line=2.5, cex=0.8)
}

# png(file = "InterannualVar.png", bg = "transparent",
#     width=1000, height=400,
#     antialias = "cleartype")

###############################################################################
##### Produce Figure 4: Tz0 and SST, RFT, WS, and CC

pdf("Fig4_InterannualVar.pdf", width=7, height=8)
mat<-matrix(1:8, nrow = 4, ncol = 2, byrow=T)
layout(mat)
par(oma=c(2,5,2,5), mar=c(0,0,0,0), cex.axis=0.9, 
    tcl=-0.3, mgp=c(2.5,0.8,0))

###### SST ######################
### SST + Tmax
plot(all.dt.f3[,"sst"], lwd=2, ylim=c(-0.9, 0.9), xaxt='n', las=2)
abline(0,0)
axis(side=1, labels=F, tick=T, tcl=0.3)

par(new=T)
plot(all.dt.f3[,"Tmax0"], col="red", lwd=2, ylim=c(-0.9, 0.9), axes=F, ann=F)
axis(side=4, labels=F, tick=T, tcl=0.3)
legend("bottom", bty="n", horiz=T,
       legend=c(expression(Detrended~SST), dtTmax.exp),
       lwd=2, col=c("black", "red"), cex=0.9)

mtext(text=expression(Detrended~T[max~z0]), outer=F,
      side=3, line=0.4, cex=1)
mtext(text="Upwind SST", side=2, line=3, cex=0.9)

### SST + Tmin
plot(all.dt.f3[,"sst"], lwd=2, ylim=c(-0.9, 0.9), xaxt='n', yaxt="n")
abline(0,0)
axis(side=1, labels=F, tick=T, tcl=0.3)
axis(side=2, labels=F, tick=T, tcl=0.3)

par(new=T)
plot(all.dt.f3[,"Tmin0"], col="blue", lwd=2, ylim=c(-0.9, 0.9), axes=F, ann=F)
axis(side=4, labels=T, tick=T, las=2)
legend("bottom", bty="n", horiz=T,
       legend=c(expression(Detrended~SST), dtTmin.exp),
       lwd=2, col=c("black", "blue"), cex=0.9)

mtext(text=expression(Detrended~T[min~z0]), outer=F,
      side=3, line=0.4, cex=1)

# ### SST + DTR
# plot(all.dt.f3[,"sst"], lwd=2, ylim=c(-0.9, 0.9), xaxt='n', yaxt="n")
# abline(0,0)
# axis(side=1, labels=F, tick=T, tcl=0.3)
# axis(side=2, labels=F, tick=T, tcl=0.3)
# 
# par(new=T)
# plot(all.dt.f3[,"dtr"], col="gray", lwd=2, ylim=c(-0.9, 0.9), axes=F, ann=F)
# axis(side=4, labels=T, tick=T, las=2, cex=0.9)
# legend("bottom", bty="n", horiz=T,
#        legend=c(expression(Detrended~SST), dtDTR.exp),
#        lwd=2, col=c("black", "gray"), cex=0.9)
# mtext(text=expression(Detrended~DTR[z0]), outer=F,
#       side=3, line=0.4, cex=1)
mtext(text=expression(Detrended~T[z0]~(degree*C)), outer=T, side=4, line=3, cex=1)

###### ln(RFI) ######################
### ln(RFI) + Tmax
plot(all.dt.f3[,"lnrfi"], lwd=2, ylim=c(-0.4, 0.4), yaxt='n', xaxt='n')
abline(0,0)
axis(side=1, labels=F, tick=T, tcl=0.3)
axis(side=2, labels=T, tick=T, las=2, at=c(-0.3, 0, 0.3))

par(new=T)
plot(all.dt.f3[,"Tmax0"], col="red", lwd=2, ylim=c(-0.9, 0.9), ann=F, axes=F)
axis(side=4, labels=F, tick=T, tcl=0.3)
legend("bottom", bty="n", horiz=T,
       legend=c(expression(Detrended~ln(RFI)), dtTmax.exp),
       lwd=2, col=c("black", "red"), cex=0.9)

mtext(text="Rainfall Index", side=2, line=3, cex=0.9)

### ln(RFI) + Tmin
plot(all.dt.f3[,"lnrfi"], lwd=2, ylim=c(-0.4, 0.4), yaxt='n', xaxt='n')
abline(0,0)
axis(side=1, labels=F, tick=T, tcl=0.3)
axis(side=2, labels=F, tick=T, tcl=0.3, at=c(-0.3, 0, 0.3))

par(new=T)
plot(all.dt.f3[,"Tmin0"], col="blue", lwd=2, ylim=c(-0.9, 0.9), ann=F, axes=F)
axis(side=4, labels=T, tick=T, las=2)
legend("bottom", bty="n", horiz=T,
       legend=c(expression(Detrended~ln(RFI)), dtTmin.exp),
       lwd=2, col=c("black", "blue"), cex=0.9)

# ### ln(RFI) + DTR
# plot(all.dt.f3[,"lnrfi"], lwd=2, ylim=c(-0.4, 0.4), yaxt='n', xaxt='n')
# abline(0,0)
# axis(side=1, labels=F, tick=T, tcl=0.3)
# axis(side=2, labels=F, tick=T, tcl=0.3, at=c(-0.3, 0, 0.3))
# 
# par(new=T)
# plot(all.dt.f3[,"dtr"], col="gray", lwd=2,ylim=c(-0.9, 0.9), axes=F, ann=F)
# axis(side=4, labels=T, tick=T, las=2)
# legend("bottom", bty="n", horiz=T,
#        legend=c(dtDTR.exp, expression(Detrended~ln(RFI))),
#        lwd=2, col=c("gray", "black"), cex=0.9)

###### WASWIND ######################
### Wind Speed + Tmax
plot(all.dt.f3[,"waswind"], lwd=2, ylim=c(-0.5, 0.5), yaxt="n", xaxt='n')
abline(0,0)
axis(side=1, labels=F, tick=T, tcl=0.3)
axis(side=2, labels=T, tick=T, las=2, at=c(-0.3, 0, 0.3))

par(new=T)
plot(all.dt.f3[,"Tmax0"], col="red", lwd=2, ylim=c(-0.9, 0.9), axes=F, ann=F)
axis(side=4, labels=F, tick=T, tcl=0.3)
legend("bottom", bty="n", horiz=T,
       legend=c(expression(Detrended~WS), dtTmax.exp),
       lwd=2, col=c("black", "red"), cex=0.9)

mtext(text="Wind Speed", side=2, line=3, cex=0.9)

### Wind Speed + Tmin
plot(all.dt.f3[,"waswind"], lwd=2, ylim=c(-0.5, 0.5), xaxt='n', yaxt="n")
abline(0,0)
axis(side=1, labels=F, tick=T, tcl=0.3)
axis(side=2, labels=F, tick=T, tcl=0.3, at=c(-0.3, 0, 0.3))

par(new=T)
plot(all.dt.f3[,"Tmin0"], col="blue", lwd=2, ylim=c(-0.9, 0.9), axes=F, ann=F)
axis(side=4, labels=T, tick=T, las=2)
legend("bottom", bty="n", horiz=T,
       legend=c(expression(Detrended~WS), dtTmin.exp),
       lwd=2, col=c("black", "blue"), cex=0.9)

# ### Wind Speed + DTR
# plot(all.dt.f3[,"waswind"], lwd=2, ylim=c(-0.5, 0.5), xaxt='n', yaxt="n")
# abline(0,0)
# axis(side=1, labels=F, tick=T, tcl=0.3)
# axis(side=2, labels=F, tick=T, tcl=0.3, at=c(-0.3, 0, 0.3))
# 
# par(new=T)
# plot(all.dt.f3[,"dtr"], col="gray", lwd=2, ylim=c(-0.9, 0.9), axes=F, ann=F)
# axis(side=4, labels=T, tick=T, las=2, cex=0.9)
# legend("bottom", bty="n", horiz=T,
#        legend=c(expression(Detrended~WS), dtDTR.exp),
#        lwd=2, col=c("black", "gray"), cex=0.9)

# ###### ERA-Interim Total Cloud Cover ######################
# ### Cloud Cover + Tmax
# plot(all.dt.f3[,"sqtcc"], lwd=2, ylim=c(-0.035, 0.035), yaxt='n')
# abline(0,0)
# axis(side=2, labels=T, tick=T, las=2, cex.lab=0.9, at=c(-0.02,0,0.02))
# 
# par(new=T)
# plot(all.dt.f3[,"Tmax0"], col="red", lwd=2, ylim=c(-0.9, 0.9), ann=F, axes=F)
# axis(side=4, labels=F, tick=T, tcl=0.3)
# legend("bottom", bty="n", horiz=T,
#        legend=c(dtTmax.exp, expression(Detrended~sqrt(TCC))),
#        lwd=2, col=c("red", "black"), cex=0.9)
# mtext(text="Detrended TCC", side=2, line=3, cex=0.9)
# 
# ### Cloud Cover + Tmin
# plot(all.dt.f3[,"sqtcc"], lwd=2, ylim=c(-0.035, 0.035), yaxt='n')
# abline(0,0)
# axis(side=2, labels=F, tick=T, at=c(-0.02,0,0.02), tcl=0.3)
# 
# par(new=T)
# plot(all.dt.f3[,"Tmin0"], col="blue", lwd=2, ylim=c(-0.9, 0.9), ann=F, axes=F)
# axis(side=4, labels=F, tick=T, tcl=0.3)
# legend("bottom", bty="n", horiz=T,
#        legend=c(dtTmin.exp, expression(Detrended~sqrt(TCC))),
#        lwd=2, col=c("blue", "black"), cex=0.9)
# 
# ### Cloud Cover + DTR
# plot(all.dt.f3[,"sqtcc"], lwd=2, ylim=c(-0.035, 0.035), yaxt='n')
# abline(0,0)
# axis(side=2, labels=F, tick=T, at=c(-0.02,0,0.02), tcl=0.3)
# 
# par(new=T)
# plot(all.dt.f3[,"dtr"], col="gray", lwd=2, ylim=c(-0.9, 0.9), axes=F, ann=F)
# axis(side=4, labels=T, tick=T, las=2)
# legend("bottom", bty="n", horiz=T,
#        legend=c(dtDTR.exp, expression(Detrended~sqrt(TCC))),
#        lwd=2, col=c("gray", "black"), cex=0.9)

###### Pacific Decadal Oscillation ######################
### PDO + Tmax
plot(all.dt.f3[,"PDO"], lwd=2, ylim=c(-1.5, 1.5), yaxt='n')
abline(0,0)
axis(side=2, labels=T, tick=T, las=2, cex.lab=0.9, at=c(-1.0,0,1.0))

par(new=T)
plot(all.dt.f3[,"Tmax0"], col="red", lwd=2, ylim=c(-0.9, 0.9), ann=F, axes=F)
axis(side=4, labels=F, tick=T, tcl=0.3)
legend("bottom", bty="n", horiz=T,
       legend=c("PDO", dtTmax.exp),
       lwd=2, col=c("black", "red"), cex=0.9)
mtext(text="PDO", side=2, line=3, cex=0.9)

### PDO + Tmin
plot(all.dt.f3[,"PDO"], lwd=2, ylim=c(-1.5,1.5), yaxt='n')
abline(0,0)
axis(side=2, labels=F, tick=T, at=c(-1.0,0,1.0), tcl=0.3)

par(new=T)
plot(all.dt.f3[,"Tmin0"], col="blue", lwd=2, ylim=c(-0.9, 0.9), ann=F, axes=F)
axis(side=4, labels=T, tick=T, las=2)
legend("bottom", bty="n", horiz=T,
       legend=c("PDO", dtTmin.exp),
       lwd=2, col=c("black","blue"), cex=0.9)

# ### PDO + DTR
# plot(all.dt.f3[,"PDO"], lwd=2, ylim=c(-1.5, 1.5), yaxt='n')
# abline(0,0)
# axis(side=2, labels=F, tick=T, at=c(-0.02,0,0.02), tcl=0.3)
# 
# par(new=T)
# plot(all.dt.f3[,"dtr"], col="gray", lwd=2, ylim=c(-0.9, 0.9), axes=F, ann=F)
# axis(side=4, labels=T, tick=T, las=2)
# legend("bottom", bty="n", horiz=T,
#        legend=c(dtDTR.exp, "PDO"),
#        lwd=2, col=c("gray", "black"), cex=0.9)

dev.off()
###############################################################################

###### Wind and Clouds
# ###### Wind ###################### 
# #WASWind
# plt.2(ts1=all.dt.f3[,"waswind"], ylab1="WASWind smoothed",
#       ts2=all.dt.f3[,"Tmin0"], ylab2="Tmin smoothed",
#       col2="blue")
# plt.2(ts1=all.dt.f3[,"waswind"], ylab1="WASWind smoothed",
#       ts2=all.dt.f3[,"Tmax0"], ylab2="Tmax smoothed",
#       col2="red")
# plt.2(ts1=all.dt.f3[,"waswind"], ylab1="WASWind smoothed",
#       ts2=all.dt.f3[,"dtr"], ylab2="DTR smoothed",
#       col2="gray")
# 
# #si10
# plt.2(ts1=all.dt.f3[,"si10"], ylab1="ERA-Int 10m wind smoothed",
#       ts2=all.dt.f3[,"Tmin0"], ylab2="Tmin smoothed",
#       col2="blue")
# plt.2(ts1=all.dt.f3[,"si10"], ylab1="ERA-Int 10m wind smoothed",
#       ts2=all.dt.f3[,"Tmax0"], ylab2="Tmax smoothed",
#       col2="red")
# plt.2(ts1=all.dt.f3[,"si10"], ylab1="ERA-Int 10m wind smoothed",
#       ts2=all.dt.f3[,"dtr"], ylab2="DTR smoothed",
#       col2="gray")
# 
# #HNL AWND
# plt.2(ts1=all.dt.f3[,"awnd"], ylab1="HNL AWND wind smoothed",
#       ts2=all.dt.f3[,"Tmin0"], ylab2="Tmin smoothed",
#       col2="blue")
# plt.2(ts1=all.dt.f3[,"awnd"], ylab1="HNL AWND wind smoothed",
#       ts2=all.dt.f3[,"Tmax0"], ylab2="Tmax smoothed",
#       col2="red")
# plt.2(ts1=all.dt.f3[,"awnd"], ylab1="HNL AWND wind smoothed",
#       ts2=all.dt.f3[,"dtr"], ylab2="DTR smoothed",
#       col2="gray")

# ###### Cloud ###################### 
# par(mfcol=c(3,2), mar=c(2,4,1,4))
# # Total CC
# plt.2(ts1=all.dt.f3[,"sqtcc"], ylab1="sqrt(Total CC) smoothed",
#       ts2=all.dt.f3[,"Tmin0"], ylab2="Tmin smoothed",
#       col2="blue")
# plt.2(ts1=all.dt.f3[,"sqtcc"], ylab1="sqrt(Total CC) smoothed",
#       ts2=all.dt.f3[,"Tmax0"], ylab2="Tmax smoothed",
#       col2="red")
# plt.2(ts1=all.dt.f3[,"sqtcc"], ylab1="sqrt(Total CC) smoothed",
#       ts2=all.dt.f3[,"dtr"], ylab2="DTR smoothed",
#       col2="gray")
# # High Cloud Cover
# plt.2(ts1=all.dt.f3[,"sqhcc"], ylab1="sqrt(High CC) smoothed",
#       ts2=all.dt.f3[,"Tmin0"], ylab2="Tmin smoothed",
#       col2="blue")
# plt.2(ts1=all.dt.f3[,"sqhcc"], ylab1="sqrt(High CC) smoothed",
#       ts2=all.dt.f3[,"Tmax0"], ylab2="Tmax smoothed",
#       col2="red")
# plt.2(ts1=all.dt.f3[,"sqhcc"], ylab1="sqrt(High CC) smoothed",
#       ts2=all.dt.f3[,"dtr"], ylab2="DTR smoothed",
#       col2="gray")
# 
# # Medium Cloud Cover
# plt.2(ts1=all.dt.f3[,"sqmcc"], ylab1="sqrt(Medium CC) smoothed",
#       ts2=all.dt.f3[,"Tmin0"], ylab2="Tmin smoothed",
#       col2="blue")
# plt.2(ts1=all.dt.f3[,"sqmcc"], ylab1="sqrt(Medium CC) smoothed",
#       ts2=all.dt.f3[,"Tmax0"], ylab2="Tmax smoothed",
#       col2="red")
# plt.2(ts1=all.dt.f3[,"sqmcc"], ylab1="sqrt(Medium CC) smoothed",
#       ts2=all.dt.f3[,"dtr"], ylab2="DTR smoothed",
#       col2="gray")
# 
# # Low Cloud Cover
# plt.2(ts1=all.dt.f3[,"sqlcc"], ylab1="sqrt(Low CC) smoothed",
#       ts2=all.dt.f3[,"Tmin0"], ylab2="Tmin smoothed",
#       col2="blue")
# plt.2(ts1=all.dt.f3[,"sqlcc"], ylab1="sqrt(Low CC) smoothed",
#       ts2=all.dt.f3[,"Tmax0"], ylab2="Tmax smoothed",
#       col2="red")
# plt.2(ts1=all.dt.f3[,"sqlcc"], ylab1="sqrt(Low CC) smoothed",
#       ts2=all.dt.f3[,"dtr"], ylab2="DTR smoothed",
#       col2="gray")
# 
# # HNL ACSH
# plt.2(ts1=all.dt.f3[,"sqacsh"], ylab1="sqrt(ACSH) smoothed",
#       ts2=all.dt.f3[,"Tmin0"], ylab2="Tmin smoothed",
#       col2="blue")
# plt.2(ts1=all.dt.f3[,"sqacsh"], ylab1="sqrt(ACSH) smoothed",
#       ts2=all.dt.f3[,"Tmax0"], ylab2="Tmax smoothed",
#       col2="red")
# plt.2(ts1=all.dt.f3[,"sqacsh"], ylab1="sqrt(ACSH) smoothed",
#       ts2=all.dt.f3[,"dtr"], ylab2="DTR smoothed",
#       col2="gray")
# 
# # HNL ACMH
# plt.2(ts1=all.dt.f3[,"sqacmh"], ylab1="sqrt(ACMH) smoothed",
#       ts2=all.dt.f3[,"Tmin0"], ylab2="Tmin smoothed",
#       col2="blue")
# plt.2(ts1=all.dt.f3[,"sqacmh"], ylab1="sqrt(ACMH) smoothed",
#       ts2=all.dt.f3[,"Tmax0"], ylab2="Tmax smoothed",
#       col2="red")
# plt.2(ts1=all.dt.f3[,"sqacmh"], ylab1="sqrt(ACMH) smoothed",
#       ts2=all.dt.f3[,"dtr"], ylab2="DTR smoothed",
#       col2="gray")

# Plot PDO
plt.2(ts1=all.dt.f3[,"PDO"], ylab1="PDO smoothed",
      ts2=all.dt.f3[,"Tmin0"], ylab2="Tmin smoothed",
      col2="blue")
plt.2(ts1=all.dt.f3[,"PDO"], ylab1="PDO smoothed",
      ts2=all.dt.f3[,"Tmax0"], ylab2="Tmax smoothed",
      col2="red")
plt.2(ts1=all.dt.f3[,"PDO"], ylab1="PDO smoothed",
      ts2=all.dt.f3[,"dtr"], ylab2="DTR smoothed",
      col2="gray")

# Plot PDO
plt.2(ts1=all.dt.f3[,"PDO"], ylab1="PDO smoothed",
      ts2=all.dt.f3[,"sst"], ylab2="SST smoothed",
      col2="blue")
plt.2(ts1=all.dt.f3[,"PDO"], ylab1="PDO smoothed",
      ts2=all.dt.f3[,"waswind"], ylab2="WASWind smoothed",
      col2="skyblue")
plt.2(ts1=all.dt.f3[,"PDO"], ylab1="PDO smoothed",
      ts2=all.dt.f3[,"sqtcc"], ylab2="sqtcc smoothed",
      col2="gray")

# MEI
MEIplot(all$MEI)
par(new=T)
plot(all.dt.f3[,"sst"], col="black", lwd=2, axes=F, ann=F)
axis(4); mtext("SST, smoothed", side=4, line=2.5, cex=0.8)
legend("topleft", legend=c("MEI>0", "MEI<0", "SST"), bty="n", ncol=2,
       lwd=c(NA,NA, 2), fill=c("pink", "skyblue", NA), 
       border=c("gray30", "gray30", NA), col=c(NA, NA, "black"),
       cex=.9, xjust=1)

MEIplot(all$MEI)
par(new=T)
plot(all.dt.f3[,"Tmin0"], col="blue", lwd=2, axes=F, ann=F)
axis(4); mtext("Tmin, smoothed", side=4, line=2.5, cex=0.8)
legend("topleft", legend=c("MEI>0", "MEI<0", Tmin.exp), bty="n", ncol=2,
       lwd=c(NA,NA, 2), fill=c("pink", "skyblue", NA), 
       border=c("gray30", "gray30", NA), col=c(NA, NA, "blue"),
       cex=.9, xjust=1)

MEIplot(all$MEI)
par(new=T)
plot(all.dt.f3[,"Tmax0"], col="red", lwd=2, axes=F, ann=F)
axis(4); mtext("Tmax, smoothed", side=4, line=2.5, cex=0.8)
legend("topleft", legend=c("MEI>0", "MEI<0", Tmax.exp), bty="n", ncol=2,
       lwd=c(NA,NA, 2), fill=c("pink", "skyblue", NA), 
       border=c("gray30", "gray30", NA), col=c(NA, NA, "red"),
       cex=.9, xjust=1)
dev.off()
#############
#line up DTR and waswind, acsh to examine 1980s
par(mfrow=c(2,1), mar=c(2,4,2,4))
# Plot DTR with Windspeed
plot(all.dt.f3[,"dtr"], ylab="DTR (C)", lwd=2, main="Detrended")
par(new=T)
plot(all.dt.f3[,"waswind"], col="gray50", axes=F, ann=F)
lines(all.dt.f3[,"si10"], col="black")
lines(all.dt.f3[,"awnd"], col="blue")
axis(4); mtext(side=4, line=2.5, "Windspeed", cex=0.8)
legend("topleft", bty="n",
       legend=c("DTR",
                "WASWind", 
                "ERA-Int si10",
                "HNL AWND"), 
       lwd=c(2,1,1,1), cex=0.8, xjust=1, 
       col=c("black", "gray50", "black", "blue"))

# Plot DTR with Clouds
plot(all.dt.f3[,"dtr"], ylab="DTR (C)", lwd=2, main="Detrended")
par(new=T)
plot(all.dt.f3[,"sqtcc"], col="black", axes=F, ann=F)
lines(all.dt.f3[,"sqlcc"], col="gray30")
lines(all.dt.f3[,"sqmcc"], col="gray50")
lines(all.dt.f3[,"sqhcc"], col="gray80")
lines(all.dt.f3[,"sqacmh"], col="darkblue")
lines(all.dt.f3[,"sqacsh"], col="skyblue")
axis(4); mtext(side=4, line=2.5, "sqrt(Cloud Cover)", cex=0.8)
legend("bottomleft", lwd=c(2, rep(1, 6)), cex=0.8, xjust=1,bty="n",
       legend=c("DTR",
                "sqrt(TCC)", 
                "sqrt(LCC)",
                "sqrt(MCC)",
                "sqrt(HCC)",
                "sqrt(ACMH)",
                "sqrt(ACSH)"), 
       col=c("black", "black","gray30", "gray50", "gray80",
             "darkblue", "skyblue"))



# #################################################################
# ########### VISUALIZE ALL THE MONTHLY TIME SERIES ###############
# ## Compare climate index time series, esp MEI, MEIext
# plot(indices.meiX.ts, lty=2, ylab="Climate Index",
#      ylim=c(min(indices.mei.ts, indices.pdo.ts, na.rm=T), 
#             max(indices.mei.ts, indices.pdo.ts, na.rm=T))) 
# lines(indices.pdo.ts, col="red") 
# lines(indices.ipo.ts, col="purple")
# lines(indices.mei.ts, lty=1, col="blue")
# legend("bottom", horiz=T, legend=c("MEIext", "MEI", "PDO", "IPO"), 
#        lty=c(2,1,1,1), col=c("black", "blue", "red", "purple"), bty="n")

####### Plot the indices
## Function: make MEI plot

# MEI
pdf("FINAL_FiguresTables/Fig6_Indices.pdf")
par(fig=c(0,1,0.66,1), mar = c(0,4,2,5), 
    cex.axis=1, tcl=-0.3, mgp=c(2.5,0.8,0))

plot(all$MEI, ylim=c(-3,3), yaxt='n', xaxt='n', ylab=NULL, col="gray30")
abline(0,0)
axis(2, las=2, at=c(-2,0,2))
axis(1, tick=T, labels=F, at=seq(1910, 2020, by=10))
fill<-as.numeric(all$MEI); fill[c(1, length(all$MEI))]<-0  #start and end at 0
fillblue<-fill; fillred<-fill
fillblue[fill>0]<-0; fillred[fill<0]<-0
polygon(time(all$MEI), fillblue, col='skyblue', border="gray30")
polygon(time(all$MEI), fillred, col='pink', border="gray30")

par(new=T)
plot(all.dt.f3[,"Tmin0"], col="blue", lwd=2, 
     ylim=c(-0.8, 0.8), axes=F, ann=F)
lines(all.dt.f3[,"Tmax0"], col="red", lwd=2)
axis(4, las=2)
legend("topleft", legend=c("MEI>0", "MEI<0"), 
       bty="n", fill=c("pink", "skyblue"), horiz=T,
       border=c("gray30", "gray30"),
       xjust=0, cex=1.2)
legend("bottom", inset=-0.025,legend=c(dtTmax.exp, dtTmin.exp), 
       bty="n", horiz=T, lwd=2, col=c("red","blue"), xjust=0, cex=1.2)
mtext(side=2, line=2, las=0, cex=1.2, text="MEI")

# PDO
par(fig=c(0,1,0.33,0.66), new=T,
    mar = c(0,4,0,5), cex.axis=1, tcl=-0.3, mgp=c(2.5,0.8,0))
plot(all.dt.f3[,"PDO"], col="black", lwd=2, ylim=c(-1.6, 1.6), 
     xaxt='n', yaxt='n', ylab=NULL)
axis(2, las=2, at=c(-1,0,1))
axis(1, tick=T, labels=F, at=seq(1910, 2020, by=10))
abline(0,0)
par(new=T)
plot(all.dt.f3[,"Tmin0"], col="blue", lwd=2, 
     ylim=c(-0.8, 0.8), axes=F, ann=F)
lines(all.dt.f3[,"Tmax0"], col="red", lwd=2)
axis(4, las=2)
mtext(side=4, line=-2, las=0, outer=T, cex=1.2,
      text=expression(Detrended~Temperature~Series~(degree*C)))
legend("topleft", legend="PDO", 
       bty="n", lwd=2, col="black", xjust=0, cex=1.2)
legend("bottom", inset=-0.025,legend=c(dtTmax.exp, dtTmin.exp), 
       bty="n", horiz=T, lwd=2, col=c("red","blue"), xjust=0, cex=1.2)
mtext(side=2, line=2, las=0, cex=1.2, text="PDO")

# IPO
par(fig=c(0,1,0,0.33), new=T,
    mar = c(2,4,0,5), cex.axis=1, tcl=-0.3, mgp=c(2.5,0.8,0))
plot(all.dt.f3[,"IPO"], col="black", lwd=2, ylim=c(-1.3, 1.3),
     yaxt='n', ylab=NULL)
axis(2, las=2, at=c(-1,0,1))
axis(1, labels=F, tick=T, at=seq(1910, 2020, by=10))
abline(0,0)
par(new=T)
plot(all.dt.f3[,"Tmin0"], col="blue", lwd=2, 
     ylim=c(-0.8, 0.8), axes=F, ann=F)
lines(all.dt.f3[,"Tmax0"], col="red", lwd=2)
axis(4, las=2)
legend("topleft", legend="IPO", bty="n", lwd=2, 
       col="black", xjust=0, cex=1.2)
legend("bottom", inset=-0.025,legend=c(dtTmax.exp, dtTmin.exp), 
       bty="n", horiz=T, lwd=2, col=c("red","blue"), xjust=0, cex=1.2)
mtext(side=2, line=2, las=0, cex=1.2, text="IPO")
dev.off()
