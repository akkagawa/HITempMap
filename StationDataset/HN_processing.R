## HN_processing.R
## Author: Aurora Kagawa-Viviani
## Date: Apr 11 2016; re-run 6/16/2017; re-run 5/3/2018
## Notes: Script for downloading and aggregating HaleNet data

setwd("xxx/TempMapping/HaleNet/May2018")

# Define a function to identify number of NA values
numNA<-function(X) {length(which(is.na(X)))}

# Define a dataframe method for is.finite
# see http://stackoverflow.com/questions/8173094/how-to-check-a-data-frame-for-any-non-finite
is.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.finite(x)))
}

################################################
##  Convert HOURLY to DAILY and write to file ##
################################################
files<-list.files(pattern="_Mrg.")   # list files that match "_Mrg."
files.out<-paste("daily_", files, ".csv", sep="")

out<-list()

for (i in 1:length(files)){
  dat<-read.csv(files[i])
  
  # Columns corresponding to Tair
  colTair<-grep(pattern="Tair", names(dat))  
  nameTair<-names(dat)[colTair]
  temp<-data.frame(dat[nameTair])    
  
  # Convert -9999 to NA
  i.na<-temp==-9999    
  is.na(temp)<-i.na

  # index by year-month-day
  ymd<-substr(dat[,1], start=1, stop=10)  
  rows<-length(unique(ymd))
  cols<-length(colTair)
  
  # make empty matrices
  Tmin<-matrix(nrow=rows, ncol=cols)
  Tmax<-matrix(nrow=rows, ncol=cols)
  Obs<-matrix(nrow=rows, ncol=cols)
  DTR<-matrix(nrow=rows, ncol=cols)
  Tavg<-matrix(nrow=rows, ncol=cols)
  
  j<-1
  while (j<=length(colTair)){
    # hourly Tair to daily Tmin, Tmax
    Tmin[,j]     <- tapply(temp[,j], INDEX=ymd, FUN=min, na.rm=T) #min, exclude NA
    Tmax[,j]     <- tapply(temp[,j], INDEX=ymd, FUN=max, na.rm=T)
    Obs[,j]      <- tapply(temp[,j], INDEX=ymd, FUN=length)-      #number obs (incl NA)
                      tapply(temp[,j], INDEX=ymd, FUN=numNA)     #number NAs
    j<-j+1
  }
  
  # calculate DTR and Tavg from daily Tmin and Tmax
  DTR          <- Tmax-Tmin
  Tavg         <- (Tmin+Tmax)/2
  
  # ensure values are tied to right dates
  YMD<-names(tapply(temp[,1], INDEX=ymd, FUN=min, na.rm=T))
  year<-substr(YMD, start=1, stop=4)
  month<-substr(YMD, start=6, stop=7)
  
  # specify station number
  last_station<-unlist(strsplit(files[i], split="_Mrg."))
  
  out[[i]]<-data.frame(Station=last_station[2],
                       enddate=last_station[1],
                       year,
                       month,
                       YMD,
                       Tmax=Tmax,
                       Tmin=Tmin,
                       Obs=Obs,
                       DTR=DTR,
                       Tavg=Tavg)

  print(i)  
  write.csv(out[[i]], file=paste("daily/", files.out[i], sep=""), row.names=F)
}

### Clean up Inf, -Inf, NaN
outCln<-out
for (i in 1:length(outCln)){
  numVals <- outCln[[i]][, !is.finite(outCln[[i]]) ] # pick columns with missing/Inf data
  
  for (j in 1:dim(numVals)[2]){
    finite<-is.finite(numVals[,j])
    numVals[!finite,j]<-NA
  }
  outCln[[i]][, !is.finite(outCln[[i]]) ]<-numVals
}


### For stations with two sensors, take mean of values
outFin<-outCln

for (i in 1:length(outFin)){
  twosens <-summary(outFin)[,1]==15
  if (twosens[i]==F){
    outFin[[i]]$Sensors<-1
  }else{
    out.wk<-outFin[[i]]
    outFin[[i]]$Tmax<-apply(out.wk[grep("Tmax", names(out.wk))], 1, "mean", na.rm=T) 
    outFin[[i]]$Tmin<-apply(out.wk[grep("Tmin", names(out.wk))], 1, "mean", na.rm=T) 
    outFin[[i]]$DTR<-apply(out.wk[grep("DTR", names(out.wk))], 1, "mean", na.rm=T) 
    outFin[[i]]$Tavg<-apply(out.wk[grep("Tavg", names(out.wk))], 1, "mean", na.rm=T) 
    outFin[[i]]$Obs<-apply(out.wk[grep("Obs", names(out.wk))], 1, "sum", na.rm=T)   # SUM obs...
    outFin[[i]]$Sensors<-2-apply(out.wk[grep("Tavg", names(out.wk))], 1, "numNA") # number sensor obs
  }
  print(i)
}
################################################
##  Aggregate to MONTH-YEAR and write to file ##  20h = valid day
################################################
### Utilize same fields for output as ExtractMonthYearv7.R
##  For station metadata: HN_info.txt
# Station	   = HN.###
# MonthYear	
# Elevation	 = 
# Latitude	 = 
# Longitude	 = 

# Tmax, Tmax.obs	
# Tmin, Tmin.obs	
# DTR, DTR.obs	
# Tavg, Tavg.obs

  ##############################################
  ## Read in and organize HN station metadata ##
  ##############################################

  meta<-readLines("HN_info.txt")[-1:-2]           

  station<-vector("character", length=length(meta))
  lat<-vector("character", length=length(meta))
  lon<-vector("character", length=length(meta))
  elev<-vector("character", length=length(meta))
  name<-vector("character", length=length(meta))

  for (i in 1:11){
    station[i] <-paste("HN.", substr(meta[i], start=2, stop=4), sep="")   
    lat[i]     <-substr(meta[i], start=67, stop=75)    # DD.dddddd
    lon[i]     <-paste("-", substr(meta[i], start=80, stop=89), sep="")    # DD.dddddd
    elev[i]    <-substr(meta[i], start=111, stop=114)  # meters
    name[i]    <-substr(meta[i], start=123, stop=136)  
  }

  metadata<-data.frame(Station=station,
                       StationName=name,
                       Elevation=elev,
                       Latitude=lat,
                       Longitude=lon)

write.csv(metadata, file="HN_info.csv", row.names=F)

# Merge all stations into single dataset ???
for (i in 1:length(outFin)){
  raw<- data.frame(Station = outFin[[i]]$Station,
                         year    = outFin[[i]]$year,
                         month   = outFin[[i]]$month,
                         YMD     = outFin[[i]]$YMD,
                         Tmax    = outFin[[i]]$Tmax,
                         Tmin    = outFin[[i]]$Tmin,
                         DTR     = outFin[[i]]$DTR,
                         Tavg    = outFin[[i]]$Tavg,
                         Obs     = outFin[[i]]$Obs,
                         Sensors = outFin[[i]]$Sensors)
  
  if(i==1) {daily.raw<-raw
  }else {daily.raw<-rbind(daily.raw, raw)}
  print(i)
} 
  
##### OR.... ##############
files.out<-paste("monthly/monthly_", files, ".csv", sep="")
monthyear<-list()   # create empty list

for (i in 1:length(outFin)){
  daily.raw<-outFin[[i]]
  # Identify DAYS with insufficient data and remove
  daily.raw$junk<-daily.raw$Obs<20  # or is this too lax?
  daily<-daily.raw[!daily.raw$junk,]
  
  # Create station_year_month index IFF combining multiple stations, as above.  
  # OTHERWISE
  ym<-paste(daily$year, daily$month, sep="")
  
  # DAILY temp to MONTHLY temp
  Tmin     <- tapply(daily$Tmin, INDEX=ym, FUN=mean, na.rm=T) # avg daily min, exclude NA
  Tmin.obs <- tapply(daily$Tmin, INDEX=ym, FUN=length)-    #number obs (incl NA)
    tapply(daily$Tmin, INDEX=ym, FUN=numNA)    #number NAs
  Tmax     <- tapply(daily$Tmax, INDEX=ym, FUN=mean, na.rm=T)
  Tmax.obs <- tapply(daily$Tmax, INDEX=ym, FUN=length)-    
    tapply(daily$Tmax, INDEX=ym, FUN=numNA)    
  
  DTR      <- tapply(daily$DTR, INDEX=ym, FUN=mean, na.rm=T)
  DTR.obs  <- tapply(daily$DTR, INDEX=ym, FUN=length)-    
    tapply(daily$DTR, INDEX=ym, FUN=numNA)    
  
  Tavg     <- tapply(daily$Tavg, INDEX=ym, FUN=mean, na.rm=T)
  Tavg.obs  <- tapply(daily$Tavg, INDEX=ym, FUN=length)-    
    tapply(daily$Tavg, INDEX=ym, FUN=numNA)  
  
  # For HN stations, include a column indicating avg# contributing sensors
  Sensors  <- tapply(daily$Sensors, INDEX=ym, FUN=mean)
  
  # Run the code below IFF combining multiple stations
  # stn<-matrix(unlist(strsplit(names(Tmin), split="_")), ncol=3, byrow=T)
  
  # Find row in metadata that matches station ID from data;
  # Use this to extract station location info
  index<-grep(as.character(unique(daily$Station)), metadata$Station) #match by station#
  
  monthyear[[i]]<-data.frame(Station = metadata$Station[index],
                        MonthYear = names(Tmin),
                        Elevation = metadata$Elevation[index],
                        Latitude	= metadata$Latitude[index],
                        Longitude	= metadata$Longitude[index],
                        Tmax      = Tmax,	
                        Tmax.obs  = Tmax.obs,
                        Tmin      = Tmin,
                        Tmin.obs  = Tmin.obs,
                        DTR	      = DTR,
                        DTR.obs   = DTR.obs,	
                        Tavg      = Tavg,
                        Tavg.obs  = Tavg.obs,
                        Sensors   = Sensors)
  
  print(i)
  write.csv(monthyear[[i]], file=files.out[i], row.names=F)
}

############################################
##  Aggregate to YEARLY and write to file ##  25d = valid month
############################################
files.out<-paste("yearly/yearly_", files, ".csv", sep="")

for (i in 1:length(monthyear)){
  # Identify MONTHS with insufficient data and remove
  monthyear[[i]]$junk<-monthyear[[i]]$DTR.obs<25  #stiffer criteria than PRISM's 25% missing
  my<-monthyear[[i]][!monthyear[[i]]$junk,]
  
  # Create yearly index
  y<-substr(my$MonthYear, start=1, stop=4)
  
  Tmin     <- tapply(my$Tmin, INDEX=y, FUN=mean, na.rm=T) # avg min, exclude NA
  Tmin.obs <- tapply(my$Tmin, INDEX=y, FUN=length)-       #number obs (incl NA)
    tapply(my$Tmin, INDEX=y, FUN=numNA)        #number NAs
  Tmax     <- tapply(my$Tmax, INDEX=y, FUN=mean, na.rm=T)
  Tmax.obs <- tapply(my$Tmax, INDEX=y, FUN=length)-    
    tapply(my$Tmax, INDEX=y, FUN=numNA)    
  
  DTR      <- tapply(my$DTR, INDEX=y, FUN=mean, na.rm=T)
  DTR.obs  <- tapply(my$DTR, INDEX=y, FUN=length)-    
    tapply(my$DTR, INDEX=y, FUN=numNA)    
  
  Tavg     <- tapply(my$Tavg, INDEX=y, FUN=mean, na.rm=T)
  Tavg.obs <- tapply(my$Tavg, INDEX=y, FUN=length)-    
    tapply(my$Tavg, INDEX=y, FUN=numNA)  
  
  # For HN stations, include a column indicating avg# contributing sensors
  Sensors  <- tapply(my$Sensors, INDEX=y, FUN=mean)
  
  year<-data.frame(Station = unique(my$Station),
                   Year = names(Tmin),
                   Elevation = unique(my$Elevation),
                   Latitude	= unique(my$Latitude),
                   Longitude	= unique(my$Longitude),
                   Tmax      = Tmax,	
                   Tmax.obs  = Tmax.obs,
                   Tmin      = Tmin,
                   Tmin.obs  = Tmin.obs,
                   DTR	      = DTR,
                   DTR.obs   = DTR.obs,	
                   Tavg      = Tavg,
                   Tavg.obs  = Tavg.obs,
                   Sensors   = Sensors)
  
  write.csv(year, file=files.out[i], row.names=F)
}

