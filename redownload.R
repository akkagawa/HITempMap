# redownload.R
# Script to download data from NCDC server
# last download: July 22, 2015
# run: July 23, 2015

####################################
setwd("<WORKING DIRECTORY HERE>")  # Insert working directory

surl<-"ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt"
surl2<-"ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"
download.file(surl, destfile="readme.txt")
download.file(surl2, destfile="ghcnd-stations.txt")

# open this file in Excel using fixed width columns
# read this file based on information in readme.txt
# select stations for which STATE="HI"

#### you can also do this right in R
# gstns<-readLines('ghcnd-stations.txt')
# state<-substr(gstns, 39, 40)
# gstns.hi<-gstns[grep(state, pattern='HI')]
####
stn<-read.csv("ghcnd-stationsHI.csv") 

##### Download files from server
files<-paste(stn$ID, ".dly", sep="")
dir<-"ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/"
for(i in 1:length(files)){
  download.file(paste(dir,files[i], sep=""), destfile=paste(files[i]))
}

##########################################
setwd("C:/Users/Kealohilani/OneDrive/Documents/Projects/TempMapping/NCDC_DailyTemp")
stn<-read.csv("ghcnd-stationsHI.csv") 
files<-paste(stn$ID, ".dly", sep="")
##### Read files and put into list "dat"
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
  if (length(rowMax!=0)&length(rowMin!=0)){
    filesn[j]<-files[i]
    datn[[j]]<-dat[[i]]
    rowMaxn[[j]]<-rowMax
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
datMax<-datn[[m]][rowMaxn[[m]]]
datMin<-datn[[m]][rowMinn[[m]]]

station<-substr(datMax,1,11)
year<-substr(datMax,12,15)
month<-substr(datMax,16,17)
element<-substr(datMax,18,21)  #Character

days<-sprintf("%02d", 1:31)

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

stationN<-substr(datMin,1,11)
yearN<-substr(datMin,12,15)
monthN<-substr(datMin,16,17)
elementN<-substr(datMin,18,21)  #Character

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

dim(vals.df)  # 2215012 
dim(valsN.df) # 2211819

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
     

dim(valTmax.cln) # 2038300
dim(valTmin.cln) # 2035529


#split into two? Excel max rows ~1million
write.csv(valTmax.cln, "allNCDCdailyMax.csv", row.names=F)
write.csv(valTmin.cln, "allNCDCdailyMin.csv", row.names=F)


