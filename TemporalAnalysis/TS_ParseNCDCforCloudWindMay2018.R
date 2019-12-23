# TS_ParseNCDCforCloudWind.R
# Project: HI Temp
# Authos: A. Kagawa-Viviani
# Date: 6/14/2017- 6/15
# Purpose: to look for cloud cover data, wind speed
# modified from redownload.R

# redownload.R->DatasetGeneration_Apr2016.R
# Script to download data from NCDC server

####################################
setwd("xxx/TempMapping/NCDC_DailyTemp")

stn<-read.csv("ghcnd-stationsHI.csv") 
files<-paste(stn$ID, ".dly", sep="")
##### Read files and put into list "dat"
dat<-list()
for (i in 1:length(files)){
  dat[[i]]<-readLines(files[i])
}

##### Whittle down to records of interest
rowMM<-list()  # ACMH = Average cloudiness midnight to midnight from manual observations (percent)
rowSS<-list()  # ACSH = Average cloudiness sunrise to sunset from manual observations (percent)

rowWS<-list()   # AWND = Average daily wind speed (tenths of meters per second)

filesclimvar<-list()
datclimvar<-list()

j<-1
for (i in 1:length(files)){
  rowM<-grep(dat[[i]], pattern="ACMH") #Avg cloudiness mdnt to mdnt
  rowS<-grep(dat[[i]], pattern="ACSH") #Avg cloudiness sunrise to sunset
  rowW<-grep(dat[[i]], pattern="AWND") #Avg wind speed
  
  if (length(rowM!=0)|length(rowS!=0)|length(rowW!=0)){
    filesclimvar[j]<-files[i]
    datclimvar[[j]]<-dat[[i]]
    rowMM[[j]]<-rowM
    rowSS[[j]]<-rowS
    rowWS[[j]]<-rowW
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

for (j in 1:length(filesclimvar)){      # j is file number
  datj<-datclimvar[[j]][c(rowMM[[j]],   # for each station with data (clouds, wind)
                          rowSS[[j]], 
                          rowWS[[j]])]
  if(j==1){datALL<-datj
  }else{ datALL<-c(datALL, datj)}
} 

station<-substr(datALL,1,11)   # parse the text
year<-substr(datALL,12,15)
month<-substr(datALL,16,17)
element<-substr(datALL,18,21)  #Character
  
days<-sprintf("%02d", 1:31)

for (n in 1:31){
  value<-substr(datALL,vi[n],vf[n])   #Integer
  mflag<-substr(datALL,mi[n],mi[n])   #Character
  qflag<-substr(datALL,qi[n],qi[n])   #Character
  sflag<-substr(datALL,si[n],si[n])   #Character
  day<-days[n]
  val<-cbind(station, year, month, day,
             element, value, mflag, qflag, sflag)
  if(n==1){vals<-val
  }else{vals<-rbind(vals, val)}
}

dim(vals)  # 190836  May 15, 2016
vals.df<-data.frame(vals)


##### Clean up file
rmove<-vals.df$value=='-9999'| vals.df$qflag!=' '
vals.cln<-vals.df[!rmove,]
vals.cln$date<-paste(vals.cln$year, vals.cln$month, vals.cln$day, sep="")

dim(vals.cln) # 183975

write.csv(vals.cln, "../WindCloud/allNCDCdailyCloudWind_2017.csv", row.names=F)

###########################################################
# Graph time series
CloudMM<-subset(vals.cln, element=="ACMH")
CloudSS<-subset(vals.cln, element=="ACSH")
WindSp<-subset(vals.cln, element=="AWND")

# Find the station(s) with the longest records: Wind Speed
Wind.start<-tapply(as.numeric(WindSp$date), WindSp$station, min)
Wind.end<-tapply(as.numeric(WindSp$date), WindSp$station, max)
Wind.lproxy<-Wind.end-Wind.start
longest<-max(Wind.lproxy)
names(Wind.start)[which(Wind.lproxy==longest)]
# "USW00021504" "USW00022516" "USW00022521" "USW00022536"
# Hilo, Kahului, Honolulu, Lihue Airports

# Find the station(s) with the longest records: Clouds
CloudMM.start<-tapply(as.numeric(CloudMM$date), CloudMM$station, min)
CloudMM.end<-tapply(as.numeric(CloudMM$date), CloudMM$station, max)
CloudMM.lproxy<-CloudMM.end-CloudMM.start
longest<-max(CloudMM.lproxy, na.rm=T)
names(CloudMM.start)[which(CloudMM.lproxy==longest)]
# "USW00022521"
# Honolulu Intnl Airport

# Take only HNL
HNL.MM<-subset(CloudMM, station=="USW00022521")
HNL.SS<-subset(CloudSS, station=="USW00022521")
HNL.WS<-subset(WindSp, station=="USW00022521")

dates<-seq.Date(from=as.Date("1965/01/01"), to=as.Date("2016/12/31"), "days")
ts.dates<-data.frame(date=as.character(dates), stringsAsFactors = F)

HNL.Clouds<-merge(HNL.MM, HNL.SS, 
                  by=c("date", "year", "month", "day", "station"),
                  all=TRUE)
HNL.ALL<-merge(HNL.Clouds, HNL.WS, 
                  by=c("date", "year", "month", "day", "station"),
                  all=TRUE)
HNL.ALL$date<-paste(HNL.ALL$year, HNL.ALL$month, HNL.ALL$day, sep="-")

# rename columns
names(HNL.ALL)[grep("value", names(HNL.ALL))]<-
  c("ACMH.pct", "ACSH.pct", "AWND.tenthms")
# drop the flags
HNL.ALLs<- HNL.ALL[,!grepl("flag",names(HNL.ALL))]
HNL.ALLs<- HNL.ALLs[,!grepl("element",names(HNL.ALLs))]

HNL.ALLsts<-merge(HNL.ALLs, ts.dates, all=T)
write.csv(HNL.ALLsts, "../WindCloud/HNLCloudWind_2017.csv", row.names=F)


###########################################################
# Pick up new R session
setwd("xxx/TempMapping/WindCloud")
library(xts)
####

dat<-read.csv("HNLCloudWind_2017.csv", stringsAsFactors = F)
dat$dates <- strptime(dat$date, format = "%Y-%m-%d")

df.xts <- xts(x = dat[, c(6:8)], order.by = dat[, "dates"])
HNLmonthly.xts<-apply.monthly(df.xts, mean)
HNLyearly.xts<-apply.yearly(df.xts, mean)

library(plotly)
library(ggplot2)
p.yearly<-autoplot.zoo(HNLyearly.xts)
ggplotly(p.yearly)

p.monthly<-autoplot.zoo(HNLmonthly.xts)
ggplotly(p.monthly)

head(HNLmonthly.xts)
Month<-.indexmon(HNLmonthly.xts)+1
YEAR<-.indexyear(HNLmonthly.xts)+1900

write.csv(data.frame(HNLmonthly.xts,YEAR, Month), "HNLCloudWind_2017Monthly.csv")
