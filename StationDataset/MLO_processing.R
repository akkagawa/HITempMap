## MLO_processing.R
## Project: HI Temp
## Author: Aurora Kagawa-Viviani
## Date: March 25 2016; revised April 8 2016; rerun 6/16/2017; rerun 5/3/2018
## Notes: Script for downloading and aggregating Mauna Loa Observatory data

setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping/MLO")

## Access GMD ftp server and download hourly data
cur<-unlist(strsplit(date(), split=" "))
url_MLO<-"ftp://ftp.cmdl.noaa.gov/met/mlo/"                              
files<-paste("met_mlo_insitu_1_obop_hour_", c(1977:cur[5]),".txt", sep="")

for (i in 1:length(files)){
  download.file(paste(url_MLO,files[i], sep=""), destfile=paste(files[i]))  
}

readme<-"ftp://ftp.cmdl.noaa.gov/met/mlo/README"
download.file(readme, destfile="README.txt")

##########################
# The data format is as follows:
# xxx yyyy mm dd hh  www  ss.s ff pppp.pp   tt.t   tt.t   tt.t  uu  rr

# Fields in each line are delimited by whitespace.  
# Fields are defined as follows:
  
# Field 1:    [SITE CODE] 
# The three-character sampling location code (see above).

# Field 2:    [YEAR] The sample collection date and time in UTC.
# Field 3:    [MONTH]
# Field 4:    [DAY]
# Field 5:    [HOUR]
# The hour is from 0 to 23, and signifies the beginning of the hour.
# For example, hour 05 is from time 05:00 to 05:59.

# Field 6:    [WIND DIRECTION] 
# Average wind direction from which the wind is blowing, 
# measured clockwise in degrees from true north.  
# North is 0 degrees, east 90 degrees, south 180, west 270
# Missing values are denoted by -999

# Field 7:    [WIND SPEED] 
# Units are meters/second.
# Missing values are denoted by -999.9

# Field 8:    [WIND STEADINESS FACTOR]
# The wind steadiness factor is 100 times the ratio of the vector wind
# speed to the average wind speed for the hour.
# Missing values are denoted by -9

# Field 9:    [BAROMETRIC PRESSURE] 
# Units are hPa
# The station pressure is not corrected for elevation in any way. 
# Missing values are denoted by -999.90

# Field 10:   [TEMPERATURE at 2 Meters] 
# Temperature at 2 meters above ground level. 
# Units are degrees Celsius.
# Missing values are denoted by -999.9

# Field 11:   [TEMPERATURE at 10 Meters] 
# Temperature at 10 meters above ground level.
# Units are degrees Celsius.
# Missing values are denoted by -999.9

# Field 12:   [TEMPERATURE at Tower Top] 
# Temperature at top of instrument tower. This elevation will vary 
# depending on the site.
# Units are degrees Celsius.
# Missing values are denoted by -999.9

# Field 13:   [RELATIVE HUMIDITY] Units are in percent.
# Missing values are denoted by -99

# Field 14:   [PRECIPITATION INTENSITY] Amount of precipitation per hour.  
# The precipitation amount is measured with an unheated tipping 
# bucket rain gauge. 
# Units are millimeters/hour.
# Missing values are denoted by -99

################################################
##  Convert HOURLY to DAILY and write to file ##
################################################
out<-list()
files.out<-paste("mlo_daily_", c(1977:2018),".csv", sep="")

numNA<-function(X) {length(which(is.na(X)))}

for (i in 1:length(files)){
  dat<-read.table(list.files(pattern="met_mlo_insitu_1_obop_hour")[i], header=F)
  temp<-dat[,c("V10","V11","V12")]
  i.na<-temp==-999.9
  is.na(temp)<-i.na
    # Convert -999.9 and -99 to NA
  
  ymd<-paste(dat$V2, 
             sprintf("%02d", dat$V3), 
             sprintf("%02d", dat$V4), sep="/")  #year/month/day
  
  # 2m hourly temp to daily temp
  Tmin2     <- tapply(temp$V10, INDEX=ymd, FUN=min, na.rm=T)#min, exclude NA
  Tmax2     <- tapply(temp$V10, INDEX=ymd, FUN=max, na.rm=T)
  Obs2      <- tapply(temp$V10, INDEX=ymd, FUN=length)-    #number obs (incl NA)
                 tapply(temp$V10, INDEX=ymd, FUN=numNA)    #number NAs
  DTR2      <- Tmax2-Tmin2
  Tavg2     <- (Tmin2+Tmax2)/2

  
  # 10m hourly temp to daily temp
  Tmin10     <-tapply(temp$V11, INDEX=ymd, FUN=min, na.rm=T)
  Tmax10     <-tapply(temp$V11, INDEX=ymd, FUN=max, na.rm=T)
  Obs10      <- tapply(temp$V11, INDEX=ymd, FUN=length)-    #number obs (incl NA)
                  tapply(temp$V11, INDEX=ymd, FUN=numNA)    #number NAs
  DTR10      <- Tmax10-Tmin10  
  Tavg10     <- (Tmin10+Tmax10)/2

  YMD<-names(Tmin2)
  year<-matrix(unlist(strsplit(YMD, split="/")), byrow=T, ncol=3)[,1]
  month<-matrix(unlist(strsplit(YMD, split="/")), byrow=T, ncol=3)[,2]
    
  out[[i]]<-data.frame(Station="MLO",
                       year,
                       month,
                       YMD,
                       Tmax2,
                       Tmin2,
                       Obs2,
                       DTR2,
                       Tavg2,
                       Tmax10,
                       Tmin10,
                       Obs10,
                       DTR10,
                       Tavg10)
                  
print(i)  
write.csv(out[[i]], files.out[i])
}

################################################
##  Aggregate to MONTH-YEAR and write to file ##  20h = valid day
################################################
### Utilize same fields for output as ExtractMonthYearv7.R
##  For station metadata: http://www.esrl.noaa.gov/gmd/dv/site/site.php?code=MLO
# Station	   = MLO
# MonthYear	
# Elevation	 = 3397.00
# Latitude	 = 19.5362
# Longitude	 = -155.5763
# Tmax, Tmax.obs	
# Tmin, Tmin.obs	
# DTR, DTR.obs	
# Tavg, Tavg.obs

# Merge all years into single dataset
for (i in 1:length(out)){
  if(i==1) {daily.raw<-out[[i]]}
  else {daily.raw<-rbind(daily.raw, out[[i]])}
}

# Identify DAYS with insufficient data and remove
daily.raw$junk<-daily.raw$Obs2<20  # or is this too lax?
daily<-daily.raw[!daily.raw$junk,]

# Create year-month index
ym<-paste(daily$year, daily$month, sep="")

# 2m DAILY temp to MONTHLY temp
Tmin2m     <- tapply(daily$Tmin2, INDEX=ym, FUN=mean, na.rm=T) # avg daily min, exclude NA
Tmin.obs2m <- tapply(daily$Tmin2, INDEX=ym, FUN=length)-    #number obs (incl NA)
                tapply(daily$Tmin2, INDEX=ym, FUN=numNA)    #number NAs
Tmax2m     <- tapply(daily$Tmax2, INDEX=ym, FUN=mean, na.rm=T)
Tmax.obs2m <- tapply(daily$Tmax2, INDEX=ym, FUN=length)-    
                tapply(daily$Tmax2, INDEX=ym, FUN=numNA)    

DTR2m      <- tapply(daily$DTR2, INDEX=ym, FUN=mean, na.rm=T)
DTR.obs2m  <- tapply(daily$DTR2, INDEX=ym, FUN=length)-    
                tapply(daily$DTR2, INDEX=ym, FUN=numNA)    

Tavg2m     <- tapply(daily$Tavg2, INDEX=ym, FUN=mean, na.rm=T)
Tavg.obs2m  <- tapply(daily$Tavg2, INDEX=ym, FUN=length)-    
                tapply(daily$Tavg2, INDEX=ym, FUN=numNA)    

monthyear<-data.frame(Station ="MLO",
                      MonthYear = names(Tmin2m),
                      Elevation = 3397.00,
                      Latitude	= 19.5362,
                      Longitude	= -155.5763,
                      Tmax      = Tmax2m,	
                      Tmax.obs  = Tmax.obs2m,
                      Tmin      = Tmin2m,
                      Tmin.obs  = Tmin.obs2m,
                      DTR	      = DTR2m,
                      DTR.obs   = DTR.obs2m,	
                      Tavg      = Tavg2m,
                      Tavg.obs  = Tavg.obs2m)

write.csv(monthyear, file="monthly_MLO.csv", row.names=F)

############################################
##  Aggregate to YEARLY and write to file ##  25d = valid month
############################################

# Identify MONTHS with insufficient data and remove
monthyear$junk<-monthyear$DTR.obs<25  #stiffer criteria than PRISM's 25% missing
my<-monthyear[!monthyear$junk,]

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

year<-data.frame(Station ="MLO",
                      Year = names(Tmin),
                      Elevation = 3397.00,
                      Latitude	= 19.5362,
                      Longitude	= -155.5763,
                      Tmax      = Tmax,	
                      Tmax.obs  = Tmax.obs,
                      Tmin      = Tmin,
                      Tmin.obs  = Tmin.obs,
                      DTR	      = DTR,
                      DTR.obs   = DTR.obs,	
                      Tavg      = Tavg,
                      Tavg.obs  = Tavg.obs)

write.csv(year, file="yearly_MLO.csv", row.names=F)
