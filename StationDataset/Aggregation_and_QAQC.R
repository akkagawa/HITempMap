## Aggregation_and_QAQC.R
## Author: Aurora Kagawa-Viviani
## Date: April 17-18 2016; re-run 6/17/2017; re-run 5/6/2018
##########################################################
## Match station locations with gridded correlates
## Subset appropriately for analysis: by month-years or years
##  Drawn from NCDCplusHaleNet3.R, SubsettingMonthYears1.R

setwd("xxx/TempMapping")
key<-read.csv("NCDCStationKeys/StationKey_allMay2018.csv")

ghcn_mo<-read.csv("NCDC_DailyTemp/monthly/monthly_mrg_completeNCDCstns.csv")
ghcn_yr<-read.csv("NCDC_DailyTemp/yearly/yearly_mrg_completeNCDCstns.csv")

MLO_mo<-read.csv("MLO/monthly_MLO.csv")
MLO_yr<-read.csv("MLO/yearly_MLO.csv")

################## HaleNet not yet aggregated ##################
filesmo<-list.files("HaleNet/May2018/monthly")
filesyr<-list.files("HaleNet/May2018/yearly")

for (i in 1:length(filesmo)){
  mo<-read.csv(paste("HaleNet/May2018/monthly/", filesmo[i], sep=""))
  yr<-read.csv(paste("HaleNet/May2018/yearly/", filesyr[i], sep=""))
  if(i==1){
    HN_mo<-mo
    HN_yr<-yr
  }else{
    HN_mo<-rbind(HN_mo, mo)
    HN_yr<-rbind(HN_yr, yr)}
}
write.csv(HN_mo, "HaleNet/May2018/monthlyHN_all.csv", row.names=F)
write.csv(HN_yr, "HaleNet/May2018/yearlyHN_all.csv", row.names=F)
##################################################################
## Aggregate data from multiple sources and compare to StationKey
## Read in monthly and annual series data
monthly<-rbind(ghcn_mo, HN_mo[,-14], MLO_mo)
yearly<-rbind(ghcn_yr, HN_yr[,-14], MLO_yr)

## Rename the "ID" column in the station key to "Station"
corrkey<-key[,c(1:2, 10, 12:14)]; names(corrkey)[1]<-"Station"

## Merge data and the key files, matching by "Station"
merged_mo<-merge(monthly, corrkey, by="Station")
merged_yr<-merge(yearly, corrkey, by="Station")

## Split MonthYear column into "Month" and "Year"
merged_mo$Month<-substr(merged_mo$MonthYear, start=5, stop=6)
merged_mo$Year<-substr(merged_mo$MonthYear, start=1, stop=4)
##################################################################
## QA/QC by completeness: Drop incomplete observations
merged_mo_crit<-droplevels(subset(merged_mo, Tmax.obs>=25))  # keep only months with 25 days
   ## Note the drop from 311 stations to 289

merged_yr_crit<-droplevels(subset(merged_yr, Tmax.obs==12))  # keep only full station-years
  ## Note the drop from 311 stations to 219

write.csv(merged_mo_crit, "Merged_mo.csv", row.names=F)
write.csv(merged_yr_crit, "Merged_yr.csv", row.names=F)
##################################################################
## QA/QC by station: 
##   1) plot each station time-series
##   2) visually inspect for obvious outliers
##   3) drop stations that provide spurious data?  Or set some criteria?

start_mo<-tapply(merged_mo_crit$MonthYear, merged_mo_crit$Station, min)
end_mo<-tapply(merged_mo_crit$MonthYear, merged_mo_crit$Station, max)

# Convert to Date class; R doesn't like YearMonth only-needs Day
z.st<-strptime(paste(start_mo, "-01", sep=""), "%Y%m-%d") 
z.end<-strptime(paste(end_mo, "-01", sep=""), "%Y%m-%d")

### Create a series of date/times at defined intervals
for (i in 1:length(start_mo)){
  times_mo<-seq.POSIXt(from=z.st[i], to=z.end[i], by="month") # create series
  times_yr<-seq.POSIXt(from=z.st[i], to=z.end[i], by="year") # create series
  stn_mo<-rep(names(start_mo)[i], times=length(times_mo))
  stn_yr<-rep(names(start_mo)[i], times=length(times_yr))
  filled_mo<-data.frame(Station=stn_mo,
                       MonthYear=paste(substr(times_mo, start=1, stop=4),
                                       substr(times_mo, start=6, stop=7),
                                       sep=""))
  filled_yr<-data.frame(Station=stn_yr,
                       Year=substr(times_yr, start=1, stop=4))
  if (i==1){
    timesFill_mo<-filled_mo
    timesFill_yr<-filled_yr
  }else{
    timesFill_mo<-rbind(timesFill_mo, filled_mo)
    timesFill_yr<-rbind(timesFill_yr, filled_yr)
  }
}

## Merge the even time series with the data to create ts objects
timesFill_mo$Station<-as.character(timesFill_mo$Station)
timesFill_mo$MonthYear<-as.numeric(as.character(timesFill_mo$MonthYear))

timesFill_yr$Station<-as.character(timesFill_yr$Station)
timesFill_yr$Year<-as.numeric(as.character(timesFill_yr$Year))

filled_mo<-merge(timesFill_mo, merged_mo, by=c("Station", "MonthYear"), all=T)
filled_yr<-merge(timesFill_yr, merged_yr, by=c("Station", "Year"), all=T)

## Then plot.ts at monthly and annual frequencies
##  From PlotMonthlybyStation.R (Aug 2015)
ts_mo_stn<-list()
stnkey<-unique(filled_mo$Station)
pdf(file="QAQCstationplots.pdf", width = 10, height = 7)
par(mfrow=c(4,2), mar=c(2,3,2,1)) ## 8 plots per page; save as multipage pdf
    
for (i in 1:length(stnkey)){
  
  ## Subset and organize time series data
  sub<-subset(filled_mo, filled_mo$Station==stnkey[i])
  start<-as.numeric(c(sub$Year[1], sub$Month[1]))
  end<-as.numeric(c(sub$Year[dim(sub)[1]], sub$Month[dim(sub)[1]]))
  ts_Tmax<-ts(sub$Tmax, start=start, end=end,freq=12)
  ts_Tmin<-ts(sub$Tmin, start=start, end=end, freq=12)
  ts_DTR<-ts(sub$DTR, start=start, end=end, freq=12)
  ts_Tavg<-ts(sub$Tavg, start=start, end=end, freq=12)
  
  ts_mo_stn[[i]]<-list(ts_Tmax, ts_Tmin, ts_DTR, ts_Tavg)
  
  ## Prepare for plotting
  tslims<-c(min(ts_DTR, ts_Tmin, na.rm=T),max(ts_Tmax, na.rm=T))
  plot(ts_Tmax, ylim=tslims, col="red",
       main=paste(stnkey[i], sub$Name[1],
                  "Elev(m)", sub$CorrElev_m[1]))
  lines(ts_Tmin, col="blue")
  lines(ts_Tavg, col="black", lty=2)
  lines(ts_DTR, col="black", lty=3)
  
  print(i)
}

dev.off()
    
##################################################################
##  Some station inhomogeneities now visible (drift, etc) and gaps/poor coverage
##  We will proceed, though, without dropping these out.
##################################################################

