## Indices_processing.R
## Author: Aurora Kagawa-Viviani
## Date: Apr 19 2016, updated 7/20/2017, 9/2017
## Notes: Script to organize various climate indices

setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping/ENSO")

#######################################################################
## Download and aggregate into a single file 
## IN NOTEPAD: cut table from html and paste into notepad

#################### Multivariate ENSO INDEX ##########################
## http://www.esrl.noaa.gov/psd/enso/mei/table.html
## Wolter, K., and M.S. Timlin, 1993: Monitoring ENSO in COADS with a seasonally 
##     adjusted principal component index. Proc. of the 17th Climate Diagnostics 
##     Workshop, Norman, OK, NOAA/NMC/CAC, NSSL, Oklahoma Clim. Survey, CIMMS and 
##     the School of Meteor., Univ. of Oklahoma, 52-57. 
## Wolter, K., and M. S. Timlin, 1998: Measuring the strength of ENSO events - 
##     how does 1997/98 rank? Weather, 53, 315-324.

  mei<- read.table("MEI1950_2018May_short.txt", skip=9, header=T)
  names(mei)[-1]<-sprintf("%02d", 1:12)
  mos<- names(mei)[-1]
  meiLong<- reshape(mei, times= mos, timevar="Month",
        varying=list(mos), v.names="mei", direction="long")
  
  ####################### Extended MEI ################################
  ## http://www.esrl.noaa.gov/psd/enso/mei.ext/table.ext.html
  ## Wolter, K., and M. S. Timlin, 2011: El NiÃ±o/Southern Oscillation behaviour 
  ##    since 1871 as diagnosed in an extended multivariate ENSO index (MEI.ext). 
  ##    Intl. J. Climatology, 31, 14pp., 1074-1087. 
  
  meiX<- read.table("MEIext.txt", skip=8, header=T)
  names(meiX)<-names(mei)
  meiXLong<- reshape(meiX, times= mos, timevar="Month",
                  varying=list(mos), v.names="meiX", direction="long")

###################### Pacific Decadal Oscillation #####################
## http://research.jisao.washington.edu/pdo/PDO.latest
## Zhang, Y., J.M. Wallace, D.S. Battisti, 1997: ENSO-like interdecadal 
##    variability: 1900-93. J. Climate, 10, 1004-1020. 
## Mantua, N.J. and S.R. Hare, Y. Zhang, J.M. Wallace, and R.C. Francis,1997: 
##    A Pacific interdecadal climate oscillation with impacts on salmon 
##    production. Bulletin of the American Meteorological Society, 78, 
##    pp. 1069-1079.

pdo<- read.table("PDO_2018May_short.txt", skip=32, header=T)
pdo$YEAR<-as.numeric(substr(pdo$YEAR, start=1, stop=4))
names(pdo)[-1]<-sprintf("%02d", 1:12)
mos<-names(pdo)[-1]
pdoLong<- reshape(pdo, times= mos, timevar="Month",
                  varying=list(mos), v.names="pdo", direction="long")

#################### Interdecadal Pacific Oscillation ###################
## https://www.esrl.noaa.gov/psd/data/timeseries/IPOTPI/tpi.timeseries.ersstv5.data
## Henley, B.J., Gergis, J., Karoly, D.J., Power, S.B., Kennedy, J., Folland, C.K., 
##    (2015). A Tripole Index for the Interdecadal Pacific Oscillation. 
##    Clim. Dyn. http://dx.doi.org/10.1007/s00382-015-2525-1.


ipo<- read.table("ipotpi.ersst5.data_head.txt", skip=11, header=F)
names(ipo)<-names(pdo)
ipoLong<- reshape(ipo, times= mos, timevar="Month",
                  varying=list(mos), v.names="ipo", direction="long")

################ Hawaii State Monthly Rainfall Index #####################
## http://rainfall.geography.hawaii.edu/downloads.html
## Frazier, A. G., Giambelluca, T. W., Diaz, H. F. and Needham, H. L. (2016), 
##    Comparison of geostatistical approaches to spatially interpolate month-year 
##    rainfall for the Hawaiian Islands. Int. J. Climatol., 36(3), 1459-1470. 
##    doi: 10.1002/joc.4437

rfi<-read.csv("State_mm_MonthlyRainfallIndex_1920_2012_CSV.csv")
names(rfi)[-14]<-names(pdo)
mos<-names(rfi)[-1]
rfiLong<- reshape(rfi, times= mos, timevar="Month",
                  varying=list(mos), v.names="rfi", direction="long")

## note that there is an annual value here
anns<-rfiLong$Month=="Ann"
rfiLongmo<-rfiLong[!anns,]

############### ERSST v5 #################################################
##
library('raster')
library('rgdal')
library('ncdf4')

#Set Working Directory
setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping")

urls<-paste("http://www1.ncdc.noaa.gov/pub/data/cmb/ersst/v5/netcdf/",
            "ersst.v5.", 
            rep(1905:2017, each=12),   #
            sprintf("%02d", rep(1:12, times=112)),
            ".nc", sep="")
dest<-paste("../SST/ersst.v5.",
            rep(1905:2017, each=12),
            sprintf("%02d", rep(1:12, times=112)),
            ".nc", sep="")

for(i in 1:length(urls)){
  download.file(urls[i], destfile=dest[i], mode = "wb")}

# set location same as Giambelluca et al 2008 SST: 22N, 156W
xyHIsst<-data.frame(x=360-156,y=22)  

sst<-vector(mode="numeric", length=length(dest))
rasters<-list()
for(i in 1:length(dest)){
  r<-raster(dest[i], varname="sst")
  sst[i]<-extract(r, xyHIsst)
  rasters[[i]]<-r
  print(i)
}

###########################################################################
################ Compile all of the above into a single df ################

yr<-data.frame(Year=1905:2017)  # for 1905-2017, 113 years

poi<-function(x, start, end) {x>=start & x<=end} # returns logical

meiLt<-subset(meiLong, poi(meiLong$YEAR, 1905, 2017))
meiXLt<-subset(meiXLong, poi(meiXLong$YEAR, 1905, 2017))
pdoLt<-subset(pdoLong, poi(pdoLong$YEAR, 1905, 2017))
ipoLt<-subset(ipoLong, poi(ipoLong$YEAR, 1905, 2017))
rfiLt<-subset(rfiLongmo, poi(rfiLongmo$YEAR, 1905, 2017))

full1<-merge(meiLt[,-4], meiXLt[,-4], by=c("YEAR","Month"), all=T)
full2<-merge(full1, pdoLt[,-4], by=c("YEAR","Month"), all=T)
full3<-merge(full2, ipoLt[,-4], by=c("YEAR","Month"), all=T)
full4<-merge(full3, rfiLt[,-4], by=c("YEAR","Month"), all=T)
full5<-cbind(full4, sst)
write.csv(full5, "ENSO/indices2017.csv", row.names=F)


############### WASWIND 1.01 #################################################
## http://www.dpac.dpri.kyoto-u.ac.jp/tokinaga/waswind.html (Tokinaga and Xie)
## also http://apdrc.soest.hawaii.edu/datadoc/waswind.php
##
library('raster')
library('rgdal')
library('ncdf4')

#Set Working Directory
setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping/WindCloud")

ncf_wind<-"waswind_v1_0_1.monthly.nc"
ncin <- nc_open(ncf_wind)
print(ncin)

nctime <- ncvar_get(ncin, "time")  # days since 1950-01-01
tunits <- ncatt_get(ncin, "time", "units")

nclon <- ncvar_get(ncin, "lon")
nclat <- ncvar_get(ncin, "lat")

# set location same as Giambelluca et al 2008 SST: 22N, 156W
# xyHIsst<-data.frame(x=360-156,y=22)  

# Hawaiian Islands span Kauai (22N, 160W) to (18N, 154W)
xr<-360-152 #slightly more to the east
xl<-360-160

ix<-which(nclon>=xl & nclon<=xr)
iy<-which(nclat>=18 & nclat<=22)

waswind<-ncvar_get(nc = ncin,
                varid = "sp",         # scalar speed
                start=c(ix[1], 28, 1),
                count=c(2, 1, -1))

waswindt<-t(waswind)
plot(waswindt[,1])
points(waswindt[,2], col="red", pch="*")

waswindmean<-rowMeans(waswindt)

windmean.ts<-ts(waswindmean, start=c(1950,1), frequency=12)


############### ERA-Interim #################################################
## http://apps.ecmwf.int/datasets/data/interim-full-moda/levtype=sfc/requests/netcdf/59b0dee3f952b1269c7e5bc8/
## need to create account and make request

# Stream:Monthly means of Daily means
# Type:Analysis
# Dataset:interim_moda
# Version:1
# Grid:0.75° x 0.75°
# Date:19790101 - 20170601
# Type of level:Surface
# Parameter:10 metre wind speed, High cloud cover, Low cloud cover, Medium cloud cover, Total cloud cover
# Class:ERA Interim

## attempted new 5/18/2018 download, python syntax: 
## need to split into smaller jobs (by year); 
## could not figure out how to do this efficiently

# #!/usr/bin/env python
# from ecmwfapi import ECMWFDataServer
# 
# server = ECMWFDataServer()
# server.retrieve({
#   "class": "ei",
#   "dataset": "interim",
#   "expver": "1",
#   "stream": "moda",
#   "type": "an",
#   "levtype": "sfc",
#   "param": "164.128/186.128/187.128/188.128/207.128",
#   "date": "1979-01-01/to/2018-01-01",
#   "grid": "0.75/0.75",
#   "area": "24/-160/16/-154",
#   "format":"netcdf",
#   "target": "C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping/WindCloud/eraint_windcloudmonthly_2017.nc"
# })


#Set Working Directory
library('raster')
library('rgdal')
library('ncdf4')

setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping/WindCloud")

ncf_eraint_1979_2017<-"eraint_windcloudmonthly.nc" #"eraint_windcloudmonthly.nc"  
ncf_eraint_2017<-"windcloud_2017_ecmwf.nc" #"eraint_windcloudmonthly.nc"  

ncin <- nc_open(ncf_eraint_1979_2017)
ncin2 <- nc_open(ncf_eraint_2017)
print(ncin)

nctime <- ncvar_get(ncin, "time")  # hours since 1900-01-01 00:00:0.0
tunits <- ncatt_get(ncin, "time", "units")

nclon <- ncvar_get(ncin, "longitude")  # 0.75degree
nclat <- ncvar_get(ncin, "latitude")   # 0.75degree

# Hawaiian Islands span Kauai (22N, 160W) to (18N, 154W)
xr<-360-154 
xl<-360-160

ix<-which(nclon>=xl & nclon<=xr)
iy<-which(nclat>=18 & nclat<=22)

era_int.si10<-ncvar_get(nc = ncin,              # returns an array
                        varid = "si10",         # 10m wind speed
                        start=c(ix[1], iy[1], 1),
                        count=c(length(ix), length(iy), -1))

era_int.lcc<-ncvar_get(nc = ncin,              # returns an array
                       varid = "lcc",         # low cloud cover
                       start=c(ix[1], iy[1], 1),
                       count=c(length(ix), length(iy), -1))

era_int.mcc<-ncvar_get(nc = ncin,              # returns an array
                       varid = "mcc",         # medium cloud cover
                       start=c(ix[1], iy[1], 1),
                       count=c(length(ix), length(iy), -1))

era_int.hcc<-ncvar_get(nc = ncin,              # returns an array
                       varid = "hcc",         # high cloud cover
                       start=c(ix[1], iy[1], 1),
                       count=c(length(ix), length(iy), -1))

era_int.tcc<-ncvar_get(nc = ncin,              # returns an array
                       varid = "tcc",         # total cloud cover
                       start=c(ix[1], iy[1], 1),
                       count=c(length(ix), length(iy), -1))

si10mean<-apply(era_int.si10, 3, mean)
lccmean<-apply(era_int.lcc, 3, mean)
mccmean<-apply(era_int.mcc, 3, mean)
hccmean<-apply(era_int.hcc, 3, mean)
tccmean<-apply(era_int.tcc, 3, mean)

###!!!  change the year below, depending on file
styear<-1979  # 2017 or 1979 
si10.ts<-ts(si10mean, start=c(styear,1), frequency=12)
lcc.ts<-ts(lccmean, start=c(styear,1), frequency=12)
mcc.ts<-ts(mccmean, start=c(styear,1), frequency=12)
hcc.ts<-ts(hccmean, start=c(styear,1), frequency=12)
tcc.ts<-ts(tccmean, start=c(styear,1), frequency=12)

plot(lcc.ts, ylim=c(0,0.5))
lines(mcc.ts, col="blue")
lines(hcc.ts, col="gray")
lines(tcc.ts, col="red")

# need to align dates
plot(tcc.ts, ylab="Total Cloud Cover")
par(new=T, mar=c(4,4,2,4))
plot(windmean.ts, col="blue", axes=F, ann=F)  
axis(4); mtext(side=4, line=2.5, "Waswind", cex=1)

#####################################################################
### Add new 2017 data
ncf_eraint_2017<-"windcloud_2017_ecmwf.nc" #"eraint_windcloudmonthly.nc"  
ncin2 <- nc_open(ncf_eraint_2017)

nctime2 <- ncvar_get(ncin2, "time")  # hours since 1900-01-01 00:00:0.0
tunits2 <- ncatt_get(ncin2, "time", "units")

nclon2 <- ncvar_get(ncin2, "longitude")  # 0.75degree
nclat2 <- ncvar_get(ncin2, "latitude")   # 0.75degree

ix<-which(nclon2>=xl & nclon2<=xr)
iy<-which(nclat2>=18 & nclat2<=22)

era_int.si102<-ncvar_get(nc = ncin2,              # returns an array
                        varid = "si10",         # 10m wind speed
                        start=c(ix[1], iy[1], 1),
                        count=c(length(ix), length(iy), -1))

era_int.lcc2<-ncvar_get(nc = ncin2,              # returns an array
                       varid = "lcc",         # low cloud cover
                       start=c(ix[1], iy[1], 1),
                       count=c(length(ix), length(iy), -1))

era_int.mcc2<-ncvar_get(nc = ncin2,              # returns an array
                       varid = "mcc",         # medium cloud cover
                       start=c(ix[1], iy[1], 1),
                       count=c(length(ix), length(iy), -1))

era_int.hcc2<-ncvar_get(nc = ncin2,              # returns an array
                       varid = "hcc",         # high cloud cover
                       start=c(ix[1], iy[1], 1),
                       count=c(length(ix), length(iy), -1))

era_int.tcc2<-ncvar_get(nc = ncin2,              # returns an array
                       varid = "tcc",         # total cloud cover
                       start=c(ix[1], iy[1], 1),
                       count=c(length(ix), length(iy), -1))

si10mean2<-apply(era_int.si102, 3, mean)
lccmean2<-apply(era_int.lcc2, 3, mean)
mccmean2<-apply(era_int.mcc2, 3, mean)
hccmean2<-apply(era_int.hcc2, 3, mean)
tccmean2<-apply(era_int.tcc2, 3, mean)

###!!!  change the year below, depending on file
styear<-2017  # 2017 or 1979 
si10.ts2<-ts(si10mean2, start=c(styear,1), frequency=12)
lcc.ts2<-ts(lccmean2, start=c(styear,1), frequency=12)
mcc.ts2<-ts(mccmean2, start=c(styear,1), frequency=12)
hcc.ts2<-ts(hccmean2, start=c(styear,1), frequency=12)
tcc.ts2<-ts(tccmean2, start=c(styear,1), frequency=12)

plot(lcc.ts2, ylim=c(0,0.5))
lines(mcc.ts2, col="blue")
lines(hcc.ts2, col="gray")
lines(tcc.ts2, col="red")

# need to align dates
plot(tcc.ts, ylab="Total Cloud Cover")
par(new=T, mar=c(4,4,2,4))
plot(windmean.ts, col="blue", axes=F, ann=F)  
axis(4); mtext(side=4, line=2.5, "Waswind", cex=1)

#########################################
# Merge WASWind (windmean.ts) and ERA-Int (lcc, tcc.ts etc) and HNL airport

windcloud<-ts.union(waswind=windmean.ts,   # ts.union() is merge() for time series
                    si10=si10.ts,
                    tcc=tcc.ts,
                    lcc=lcc.ts,
                    mcc=mcc.ts,
                    hcc=hcc.ts)
windcloud.df<-data.frame(windcloud)
windcloud.df$YEAR<-as.integer(time(windcloud))
windcloud.df$Month<-rep_len(1:12, length(windcloud.df$YEAR))
write.csv(windcloud.df, "windcloud.csv", row.names=F)

ts.intersect(window(si10.ts), window(si10.ts2)) # ts.intersect is the overlap
             
# create separate file for new ERA-Interim data
windcloud2<-ts.union(windcloud,   # ts.union() is merge() for time series
                    si10=si10.ts2,
                    tcc=tcc.ts2,
                    lcc=lcc.ts2,
                    mcc=mcc.ts2,
                    hcc=hcc.ts2)
windcloud2.df<-data.frame(windcloud2)
windcloud2.df$YEAR<-as.integer(time(windcloud2))
windcloud2.df$Month<-rep_len(1:12, length(windcloud2.df$YEAR))

write.csv(windcloud2.df, "windcloud_2017new.csv", row.names=F)

### combine these later in Excel and save as windcloud_2017merged.csv

windcloud<-read.csv("windcloud_2017merged.csv")
st.wc<-c(head(windcloud$YEAR, 1), head(windcloud$Month, 1))
nd.wc<-c(tail(windcloud$YEAR, 1), tail(windcloud$Month, 1))
windcloud<-ts(windcloud, start=st.wc, end=nd.wc, frequency=12)

# Compare 2 wind time series
plot(windcloud)
summary(lm(si10~waswind, data=windcloud)) 

# Include indices
dat<-read.csv("../ENSO/indices2017.csv")
st.dat<-c(head(dat$YEAR, 1), head(dat$Month, 1))
nd.dat<-c(tail(dat$YEAR, 1), tail(dat$Month, 1))
indices<-ts(dat, start=st.dat, end=nd.dat, frequency=12)

# Include airport station data
HNLcloudwind<-read.csv("HNLCloudWind_2017Monthly.csv")
st.hnl<-c(head(HNLcloudwind$YEAR, 1), head(HNLcloudwind$Month, 1))
nd.hnl<-c(tail(HNLcloudwind$YEAR, 1), tail(HNLcloudwind$Month, 1))
HNLcw<-ts(HNLcloudwind[,-1], start=st.hnl, end=nd.hnl, frequency=12)

# Merge these
dat.all<-ts.union(indices, windcloud, HNLcw)
dat.all2017<-window(dat.all, end = c(2017,12))

st<-start(dat.all2017)
en<-end(dat.all2017)

YEAR<-rep(st[1]:en[1], each=12)
MO<-rep(1:12, times=en[1]-st[1]+1)

dat.allf<-cbind(YEAR, MO, dat.all2017)
colnames(dat.allf)<-c("YEAR", "MO",
                   dimnames(dat.all2017)[[2]])
# save it to a file
write.csv(dat.allf, "MonthlyTimeSeries_2017.csv", row.names=F)

winds<-c("YEAR", "MO", "windcloud.waswind", "windcloud.si10", "HNLcw.AWND.tenthms")
winddat<-dat.allf[, colnames(dat.allf) %in% winds]
plot(winddat[,-c(1,2)])

test1<-lm(windcloud.waswind~HNLcw.AWND.tenthms, data=winddat)
summary(test1)

test2<-lm(windcloud.si10~HNLcw.AWND.tenthms, data=winddat) # R2 is 0.5
summary(test2)

plot(winddat[,4:5])

