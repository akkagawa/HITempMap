## DatasetGeneration_Mar2016.R; re-run 6/16/2017; re-run 5/3/2018; re-run 1/4/2019
## Author: Aurora Kagawa-Viviani
## Notes: Full Script for Temperature Modeling and Mapping
## from NCDC download to organized files of daily data for aggregation to hourly

# Define a function to identify number of NA values
numNA<-function(X) {length(which(is.na(X)))}

#################################################
## Access NCDC server and download all HI data ##
## Drawn from script "redownload.R"            ##
#################################################

setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping/NCDC_DailyTemp")

download.file("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt", 
              destfile="readme.txt")
download.file("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt", 
              destfile="ghcnd-stations.txt")
 
#### Read ghcnd-stations.txt based on information in readme.txt ##
# From readme.txt:
# ------------------------------
#   Variable   Columns   Type
# ------------------------------
# ID            1-11   Character
# LATITUDE     13-20   Real
# LONGITUDE    22-30   Real
# ELEVATION    32-37   Real
# STATE        39-40   Character
# NAME         42-71   Character
# GSN FLAG     73-75   Character
# HCN/CRN FLAG 77-79   Character
# WMO ID       81-85   Character
# ------------------------------

gstns<-readLines('ghcnd-stations.txt')
state<-substr(gstns, 39, 40)

# select stations for which STATE="HI"
gstns.hi<-gstns[grep(state, pattern='HI')]

ID<-substr(gstns.hi,1,11)
lat<-substr(gstns.hi,13,20)
long<-substr(gstns.hi,22,30)
elev<-substr(gstns.hi,32,37)
name<-substr(gstns.hi,42,71)

stn<-data.frame(ID, lat, long, elev, name)
write.csv(stn, "ghcnd-stationsHI.csv", row.names=F) # save station metadata
 
#### Download specified files from server
files<-paste(stn$ID, ".dly", sep="")
dir<-"ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/"
for(i in 714:length(files)){
  download.file(paste(dir,files[i], sep=""), destfile=paste(files[i]))
}

##### Note the dataset version
version<-"ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-version.txt"
download.file(version, destfile="ghcnd-version.txt")

##### Read each file and put into list "dat"
dat<-list()
for (i in 1:length(files)){
  dat[[i]]<-readLines(files[i])  
}

##### Whittle down to temperature records
rowMaxn<-list()
rowMinn<-list()
filesn<-list()
datn<-list()
j<-1
for (i in 1:length(files)){
  rowMax<-grep(dat[[i]], pattern="TMAX")
  rowMin<-grep(dat[[i]], pattern="TMIN")
  if (length(rowMax!=0)&length(rowMin!=0)){  # if there are both Tmax and Tmin data
    filesn[j]<-files[i]                      # set the filename
    datn[[j]]<-dat[[i]]                      # put the unparsed data into list datn
    rowMaxn[[j]]<-rowMax                     # put the indices of Tmax into list rowMaxn
    rowMinn[[j]]<-rowMin
    j<-j+1
  }
}

##### Parse files and coerce into columns
# from readme.txt:
# III. FORMAT OF DATA FILES (".dly" FILES)
#
# Each ".dly" file contains data for one station.  The name of the file
# corresponds to a station's identification code.  For example, "USC00026481.dly"
# contains the data for the station with the identification code USC00026481).
#
# Each record in a file contains one month of daily data.  The variables on each
# line include the following:
#
# ------------------------------
# Variable   Columns   Type
# ------------------------------
# ID            1-11   Character
# YEAR         12-15   Integer
# MONTH        16-17   Integer
# ELEMENT      18-21   Character
# VALUE1       22-26   Integer
# MFLAG1       27-27   Character
# QFLAG1       28-28   Character
# SFLAG1       29-29   Character
# VALUE2       30-34   Integer
# MFLAG2       35-35   Character
# QFLAG2       36-36   Character
# SFLAG2       37-37   Character
# .           .          .
# .           .          .
# .           .          .
# VALUE31    262-266   Integer
# MFLAG31    267-267   Character
# QFLAG31    268-268   Character
# SFLAG31    269-269   Character
# ------------------------------

vi<-22+8*0:30; vf<-vi+4
mi<-vf+1
qi<-mi+1
si<-qi+1

##### Start parsing rows and files

m<-1            # file number
for (m in 1:length(filesn)){
  datMax<-datn[[m]][rowMaxn[[m]]]   # for each station with data (Tmax and Tmin)
  datMin<-datn[[m]][rowMinn[[m]]]   
  
  station<-substr(datMax,1,11)      # parse the text
  year<-substr(datMax,12,15)
  month<-substr(datMax,16,17)
  element<-substr(datMax,18,21)     # Character
  
  days<-sprintf("%02d", 1:31)       # create an variable for day of month
  
  for (n in 1:31){
    value<-substr(datMax,vi[n],vf[n])   #Integer
    mflag<-substr(datMax,mi[n],mi[n])   #Character
    qflag<-substr(datMax,qi[n],qi[n])   #Character
    sflag<-substr(datMax,si[n],si[n])   #Character
    day<-days[n]
    val<-cbind(station, year, month, day,
               element, value, mflag, qflag, sflag)
    if(n==1){vals<-val
    }else{vals<-rbind(vals, val)}
  }
  
  stationN<-substr(datMin,1,11)    # repeat this for Tmin values
  yearN<-substr(datMin,12,15)
  monthN<-substr(datMin,16,17)
  elementN<-substr(datMin,18,21)   # Character
  
  for (p in 1:31){
    valueN<-substr(datMin,vi[p],vf[p])   #Integer
    mflagN<-substr(datMin,mi[p],mi[p])   #Character
    qflagN<-substr(datMin,qi[p],qi[p])   #Character
    sflagN<-substr(datMin,si[p],si[p])   #Character
    dayN<-days[p]
    valN<-cbind(stationN, yearN, monthN, dayN, 
                elementN, valueN, mflagN, qflagN, sflagN)
    if(p==1){valsN<-valN
    }else{valsN<-rbind(valsN, valN)}
  }
  
  if(m==1){vals.df<-vals; 
  valsN.df<-valsN
  }else{vals.df<-rbind(vals.df,vals); valsN.df<-rbind(valsN.df,valsN)}
}

dim(vals.df)  # 2232527  March 25, 2016
dim(valsN.df) # 2229365  March 25, 2016

##### Clean up file
valTmax.df<-data.frame(vals.df)
rmMax<-valTmax.df$value=='-9999'| valTmax.df$qflag!=' '

valTmin.df<-data.frame(valsN.df)
rmMin<-valTmin.df$valueN=='-9999'| valTmin.df$qflagN!=' '

valTmax.cln<-valTmax.df[!rmMax,]
valTmin.cln<-valTmin.df[!rmMin,]

names(valTmin.cln)<-names(valTmax.cln)
valTmax.cln$date<-paste(valTmax.cln$year, valTmax.cln$month, valTmax.cln$day, sep="")
valTmin.cln$date<-paste(valTmin.cln$year, valTmin.cln$month, valTmin.cln$day, sep="")


dim(valTmax.cln) # 2049736
dim(valTmin.cln) # 2046863

#### split into two? Excel max rows ~1million
write.csv(valTmax.cln, "allNCDCdailyMax.csv", row.names=F)
write.csv(valTmin.cln, "allNCDCdailyMin.csv", row.names=F)

######################################################################
## Combine Tmax and Tmin daily values and put into wide form        ##
## Disaggregate so separated by station                             ##
## Note this is a major revision; station-matching to come later    ##
######################################################################
setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping/NCDC_DailyTemp/")

tmax<-read.csv("allNCDCdailyMax.csv") 
tmin<-read.csv("allNCDCdailyMin.csv")

## Identify unique stations (299 for both below)
smax<-unique(tmax$station)
smin<-unique(tmin$station)

## Cut the extras
tmaxsm<-tmax[,-c(2,3,4)] #[,-c("year", "month", "day")]
tminsm<-tmin[,-c(2,3,4)]

## By station, merge daily Tmin and Tmax; WIDE form
bystation<-list()
bystationAll<-list()

dir.create("NCDC_DailyMaxMinStation")
#### Loop through each station
for (i in 1:length(smax)){  
  stnMax<-tmaxsm[tmaxsm$station==smax[i],]
  stnMin<-tminsm[tminsm$station==smax[i],]
  
  #### And merge by date and station
  bystation[[i]] <- merge(stnMax, stnMin, by=c("date", "station"))         # intersection
  bystationAll[[i]]<-merge(stnMax, stnMin, by=c("date", "station") , all=T)# union
  
  ####### create master datasets
  if (i==1){dat<-bystation[[i]]
  }else{dat<-rbind(dat,bystation[[i]])}
  
  if (i==1){datAll<-bystationAll[[i]] 
  }else{datAll<-rbind(datAll, bystationAll[[i]])}
  
  print(i)
  write.csv(bystation[[i]],
            file=paste("NCDC_DailyMaxMinStation/mrg_",smax[i], ".csv", sep=""),
            row.names=F)
}

##### Calculate DTR, Tavg
dat$DTR<-dat$value.x-dat$value.y         #Tmax-Tmin
dat$Tavg<-(dat$value.x + dat$value.y)/2  #(Tmax+Tmin)/2

#### Remove "element" columns and rename
datTrim<-dat[,-grep("element", names(dat))] 

names(datTrim)[grep(".x", names(datTrim))]<-paste("Tmax", c("", ".mflag", ".qflag", ".sflag"), sep="")
names(datTrim)[grep(".y", names(datTrim))]<-paste("Tmin", c("", ".mflag", ".qflag", ".sflag"), sep="")
  
dim(datTrim)   # 2032727      10
write.csv(datTrim, "NCDC_DailyMaxMinStation/mrg_completeNCDCdailystns.csv", row.names=F)


###########
datAll$DTR<-datAll$value.x-datAll$value.y
datAll$Tavg<-(datAll$value.x + datAll$value.y)/2

#### Remove "element" columns and rename
datAllTr<-datAll[,-grep("element", names(datAll))] 

names(datAllTr)<-names(datTrim)

dim(datAllTr)  # 2071350
write.csv(datAll, "NCDC_DailyMaxMinStation/mrg_allNCDCdailystns.csv", row.names=F)

################################################
##  Aggregate to MONTH-YEAR and write to file ##  20h = valid day
################################################
### Utilize same fields for output as ExtractMonthYearv7.R
##  For station metadata: MSHR_ENHANCED.TXT
# Station	   = 
# MonthYear	
# -------- Elevation	 = 
# -------- Latitude	 = 
# -------- Longitude	 = 

# Tmax, Tmax.obs	
# Tmin, Tmin.obs	
# DTR, DTR.obs	
# Tavg, Tavg.obs

# If starting new R session:
daily<-read.csv("NCDC_DailyMaxMinStation/mrg_completeNCDCdailystns.csv")

# Create yearmonth and station_yearmonth index 
ym <- substr(daily$date, start=1, stop=6)
sym <- paste(daily$station, ym, sep="_")
  
# DAILY temp to MONTHLY temp
Tmin     <- tapply(daily$Tmin, INDEX=sym, FUN=mean, na.rm=T) # avg daily min, exclude NA
Tmin.obs <- tapply(daily$Tmin, INDEX=sym, FUN=length)-    #number obs (incl NA)
              tapply(daily$Tmin, INDEX=sym, FUN=numNA)    #number NAs
Tmax     <- tapply(daily$Tmax, INDEX=sym, FUN=mean, na.rm=T)
Tmax.obs <- tapply(daily$Tmax, INDEX=sym, FUN=length)-    
              tapply(daily$Tmax, INDEX=sym, FUN=numNA)    

DTR      <- tapply(daily$DTR, INDEX=sym, FUN=mean, na.rm=T)
DTR.obs  <- tapply(daily$DTR, INDEX=sym, FUN=length)-    
              tapply(daily$DTR, INDEX=sym, FUN=numNA)    

Tavg     <- tapply(daily$Tavg, INDEX=sym, FUN=mean, na.rm=T)
Tavg.obs  <- tapply(daily$Tavg, INDEX=sym, FUN=length)-    
              tapply(daily$Tavg, INDEX=sym, FUN=numNA) 

stn_my<-matrix(unlist(strsplit(names(Tmin), split="_")), ncol=2, byrow=T)

Station = stn_my[,1]
MonthYear = stn_my[,2]

###############################################################################
## Incorporate station metadata that has been QA/QC'd outside of this script ##
##    -compared to RF Atlas station locations
##    -compared to mshr.txt files

#### Temporary fix below:
# Find row in metadata that matches station ID from data;
NCDCkey<-read.csv("../NCDCStationKeys/NCDC_StationKey3.csv", stringsAsFactors=F)
NCDC_QCkey<-read.csv("ncdcstation2.csv", strip.white=T, stringsAsFactors=F)

# Use this to extract station location info
stn<-unique(Station)
meta<-matrix(nrow=length(Station), ncol=3)
names(meta)<-c("Elevation", "Latitude", "Longitude")


for (i in 1:length(stn)){
  index<-grep(stn[i], NCDCkey$Station) #match by GHCND #
  sub<-grep(stn[i], Station)
  meta[sub,]<-matrix(rep(c(NCDCkey$CorrElev_m[index],
                NCDCkey$CorrLat_DD[index],
                NCDCkey$CorrLong_DD[index]), times=length(sub)), ncol=3,byrow=T)
  print(i)
}

dir.create("monthly")
monthyear<-data.frame(Station, 
                      MonthYear,
                      Elevation = meta[,1],
                      Latitude	= meta[,2],
                      Longitude	= meta[,3],
                      Tmax      = Tmax/10,	# Convert from 10ths of degrees to C
                      Tmax.obs  = Tmax.obs,
                      Tmin      = Tmin/10,
                      Tmin.obs  = Tmin.obs,
                      DTR	      = DTR/10,
                      DTR.obs   = DTR.obs,	
                      Tavg      = Tavg/10,
                      Tavg.obs  = Tavg.obs)
write.csv(monthyear, "monthly/monthly_mrg_completeNCDCstns.csv", row.names=F)

#### Separate dataset by station
files.out<-paste("monthly/monthly_", stn, ".csv", sep="")
stn_monthyear<-list()   # create empty list
for (i in 1:length(stn)){
  stn_monthyear[[i]]<-monthyear[which(monthyear$Station==stn[i]),]
  write.csv(stn_monthyear[[i]], file=files.out[i], row.names=F)  
}

############################################
##  Aggregate to YEARLY and write to file ##  25d = valid month
############################################
dir.create("yearly")
# Identify MONTHS with insufficient data and remove
monthyear$junk<-monthyear$DTR.obs<25  #stiffer criteria than PRISM's 25% missing
my<-monthyear[!monthyear$junk,]

# Create year and station_year index 
y <- substr(my$MonthYear, start=1, stop=4)
sy <- paste(my$Station, y, sep="_")

Tmin     <- tapply(my$Tmin, INDEX=sy, FUN=mean, na.rm=T) # avg min, exclude NA
Tmin.obs <- tapply(my$Tmin, INDEX=sy, FUN=length)-       #number obs (incl NA)
              tapply(my$Tmin, INDEX=sy, FUN=numNA)        #number NAs
Tmax     <- tapply(my$Tmax, INDEX=sy, FUN=mean, na.rm=T)
Tmax.obs <- tapply(my$Tmax, INDEX=sy, FUN=length)-    
              tapply(my$Tmax, INDEX=sy, FUN=numNA)    

DTR      <- tapply(my$DTR, INDEX=sy, FUN=mean, na.rm=T)
DTR.obs  <- tapply(my$DTR, INDEX=sy, FUN=length)-    
              tapply(my$DTR, INDEX=sy, FUN=numNA)    

Tavg     <- tapply(my$Tavg, INDEX=sy, FUN=mean, na.rm=T)
Tavg.obs <- tapply(my$Tavg, INDEX=sy, FUN=length)-    
              tapply(my$Tavg, INDEX=sy, FUN=numNA)   
  
stn_y<-matrix(unlist(strsplit(names(Tmin), split="_")), ncol=2, byrow=T)
Station = stn_y[,1]
Year = stn_y[,2]

# Use this to extract station location info
stn<-unique(Station)
meta<-matrix(nrow=length(Station), ncol=3)
names(meta)<-c("Elevation", "Latitude", "Longitude")

for (i in 1:length(stn)){
  index<-grep(stn[i], NCDCkey$Station) #match by GHCND #
  sub<-grep(stn[i], Station)
  meta[sub,]<-matrix(rep(c(NCDCkey$CorrElev_m[index],
                           NCDCkey$CorrLat_DD[index],
                           NCDCkey$CorrLong_DD[index]), times=length(sub)), ncol=3,byrow=T)
  print(i)
}

year<-data.frame(Station,
                 Year,
                 Elevation = meta[,1],
                 Latitude	 = meta[,2],
                 Longitude = meta[,3],
                 Tmax      = Tmax,	
                 Tmax.obs  = Tmax.obs,
                 Tmin      = Tmin,
                 Tmin.obs  = Tmin.obs,
                 DTR	      = DTR,
                 DTR.obs   = DTR.obs,	
                 Tavg      = Tavg,
                 Tavg.obs  = Tavg.obs)

write.csv(year, "yearly/yearly_mrg_completeNCDCstns.csv", row.names=F)

#### Separate dataset by station (279 stations)  
files.out<-paste("yearly/yearly_", stn, ".csv", sep="")
stn_year<-list()   # create empty list
for (i in 1:length(stn)){
  stn_year[[i]]<-year[which(year$Station==stn[i]),]
  write.csv(stn_year[[i]], file=files.out[i], row.names=F)  
}
