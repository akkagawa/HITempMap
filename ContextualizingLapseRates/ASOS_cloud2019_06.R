## Project: HI Temp Map
## Script: ASOS_cloud2019.R
## Date: 5/28/2019
## Author: A. Kagawa-Viviani
## Notes: this script pulls and analyzes airport ASOS to compare to surface and sounding lapse rates:
##    1) download files
##    2) extract metrics, export as csv aggregated monthly time series
##    3) visualize time series of cloud frequency and ceiling (cloud base) height
##################################################################

setwd("xxx/ASOS_Cloud")

######### NCDC FILES HARD TO PARSE!!! ########################
# ######## Download from NCDC server ###
# dirs<-paste0("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", 1990:2018,"/")
# phli.files<-paste0("911650-22536-", 1990:2018, ".gz")
# phto.files<-paste0("912850-21504-", 1990:2018, ".gz")
# #phto2.files<-paste0("912853-21504-", 1990:2018, ".gz")
# 
# for (i in 1:length(phli.files)){
#   download.file(paste0(dirs, phli.files)[i], phli.files[i])
# }
# 
# for (i in 1:length(phto.files)){
#   download.file(paste0(dirs, phto.files)[i], phto.files[i])
#   #download.file(paste0(dirs, phto2.files)[i], phto2.files[i])
# }

######### USE https://mesonet.agron.iastate.edu INSTEAD! ########################
#
stns<-c("PHTO", "PHLI", "PHNL", "PHNG", "PHOG", "PHKO")
# "PHLI" # Lihue
# "PHNL" "PHNG" # Honolulu (leeward), Kaneohe MCAS (windward, not ASOS)
# "PHOG" # Kahului
# "PHLI" "PHKO" # Hilo (windward), Kona (leeward)

asos<-paste0("https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?",
             "station=",stns, "&data=metar&",
             "year1=1950&month1=1&day1=1&year2=2018&month2=12&day2=31&",
             "tz=Etc%2FUTC&format=onlycomma&latlon=no&missing=M&trace=T&",
             "direct=yes&report_type=1&report_type=2")
for (i in 4:6){
  download.file(asos[i], destfile=paste0(stns[i], ".txt"))
}

######## Parse files #######
### Read in each downloaded METAR file as a dataframe in a multi-station list
# fields: "station", "valid", "metar"

dat<-list()
dat$phto<-read.csv("PHTO.txt", stringsAsFactors = F, quote="") 
dat$phli<-read.csv("PHLI.txt", stringsAsFactors = F, quote="") 
dat$phnl<-read.csv("PHNL.txt", stringsAsFactors = F, quote="") 
dat$phog<-read.csv("PHOG.txt", stringsAsFactors = F, quote="") 
dat$phko<-read.csv("PHKO.txt", stringsAsFactors = F, quote="") 
dat$phng<-read.csv("PHNG.txt", stringsAsFactors = F, quote="") 

### Translate the "valid" field and create a date-time column
for (i in 1:length(dat)){
  dt<-as.POSIXct(dat[[i]]$valid, format="%Y-%m-%d %H:%M", tz="UTC")
  dat[[i]]$dt.hst<-as.POSIXct(format(dt, tz="HST"))
  print(names(dat)[i])
}

### Translate the "metar" field to identify records of BKN and OVC cloud cover
# See https://www.weather.gov/asos/
#   BKN (broken: 50-87% sky cover,  4/8 - 8/8 oktas)
#   OVC (overcast: 87-100% cover, 8/8 oktas), 
#   VV (vertical visibility, fog and rain conditions <600 ft), 
#   CIG (variable height ceiling)

# Extract 1st record corresponding to lowest level using gregexpr()

bkn.l<-ovc.l<-list()
sct.l<-few.l<-clr.l<-list()

# Loop through each of the hourly records for a given station
for(i in 1:length(dat)){
  # Create empty lists
  bkn<-ovc<-list()
  sct<-few<-clr<-list()
  
  for (j in 1:length(dat[[i]]$metar)){      # and each observation      
    bkn.n<-gregexpr("BKN", dat[[i]]$metar[j]) # Look for "BKN", "OVC"
    ovc.n<-gregexpr("OVC", dat[[i]]$metar[j])

    sct.n<-gregexpr("SCT", dat[[i]]$metar[j]) # Look for "BKN", "OVC"
    few.n<-gregexpr("FEW", dat[[i]]$metar[j])
    clr.n<-gregexpr("CLR", dat[[i]]$metar[j])
    
    if (bkn.n[[1]][1]!=-1){            # if you find BKN, pull the value
      bkn[[j]]<-substring(dat[[i]]$metar[j], bkn.n[[1]][1], bkn.n[[1]][1]+7) 
    }else{bkn[[j]]<-NA}                # or log NA
    
    if (ovc.n[[1]][1]!=-1){
      ovc[[j]]<-substring(dat[[i]]$metar[j], ovc.n[[1]][1], ovc.n[[1]][1]+7)
    }else{ovc[[j]]<-NA}
    
    if (sct.n[[1]][1]!=-1){
      sct[[j]]<-substring(dat[[i]]$metar[j], sct.n[[1]][1], sct.n[[1]][1]+6)
    }else{sct[[j]]<-NA}
    
    if (few.n[[1]][1]!=-1){
      few[[j]]<-substring(dat[[i]]$metar[j], few.n[[1]][1], few.n[[1]][1]+6)
    }else{few[[j]]<-NA}
    
    if (clr.n[[1]][1]!=-1){
      clr[[j]]<-substring(dat[[i]]$metar[j], clr.n[[1]][1], clr.n[[1]][1]+6)
    }else{clr[[j]]<-NA}
    
    print(j)
  }
  
  bkn.l[[i]]<-unlist(bkn)
  ovc.l[[i]]<-unlist(ovc)
  
  sct.l[[i]]<-unlist(sct)
  few.l[[i]]<-unlist(few)
  clr.l[[i]]<-unlist(clr)

  print(i)
}

### Name each item within the bkn.l and ovc.l lists
names(bkn.l)<-names(ovc.l)<-names(dat)
names(sct.l)<-names(few.l)<-names(clr.l)<-names(dat)

### For each station, parse the bkn and ovc vectors:
bkn_m<-ovc_m<-list()
sct_m<-few_m<-clr_m<-list()
clouds.xts<-list()
cloudobs<-list()

library(xts)
for (i in 1:length(bkn.l)){
  # u.bkn<-unique(bkn.l[[i]])  
  # u.ovc<-unique(ovc.l[[i]])

  # Account for text spacing/ data transcription issues?
  bkn_ft<-as.numeric(substr(bkn.l[[i]], start=4, stop=6))*100   # default BKN###
  bkn_ft2<-as.numeric(substr(bkn.l[[i]], start=5, stop=7))*100  # other: BKN ##
  
  ovc_ft<-as.numeric(substr(ovc.l[[i]], start=4, stop=6))*100   # default BKN###
  ovc_ft2<-as.numeric(substr(ovc.l[[i]], start=5, stop=7))*100  # other: BKN ##
  
  ## If bkn_ft2>bkn_ft, bknft2 has precedence; replace
  bkn_ft.cor<-bkn_ft
  cor.idx<-which(bkn_ft2>bkn_ft & !is.na(bkn_ft2))
  bkn_ft.cor[cor.idx]<-bkn_ft2[cor.idx]
  
  ovc_ft.cor<-ovc_ft
  cor2.idx<-which(ovc_ft2>ovc_ft & !is.na(ovc_ft2))
  ovc_ft.cor[cor2.idx]<-ovc_ft2[cor2.idx]
  
  ## screen outliers beyond ceilometer 12,000 foot limit (3657.6 m)
  bkn_ft.final<-bkn_ft.cor
  bkn_ft.final[bkn_ft.cor>12000]<-NA
  
  ovc_ft.final<-ovc_ft.cor
  ovc_ft.final[ovc_ft.cor>12000]<-NA
  
 
  ## Tally the CLR, FEW, SCT
  # Account for text spacing/ data transcription issues?
  sct_01<-as.numeric(substr(sct.l[[i]], start=4, stop=6))*100    
  few_01<-as.numeric(substr(few.l[[i]], start=4, stop=6))*100    
  clr_01<-substr(clr.l[[i]], start=1, stop=3) 

  ## Convert from ft to meters and save as xts
  bkn_m[[i]]<-bkn_ft.final*0.3048                  
  ovc_m[[i]]<-ovc_ft.final*0.3048                  
  sct_m[[i]]<-sct_01*0.3048                  
  few_m[[i]]<-few_01*0.3048                  
  clr_m[[i]]<-as.numeric(!is.na(clr_01))                  
             
  # Tally cloud observations (T/F->0/1)
  obs_01<-!is.na(sct_01)|!is.na(few_01)|!is.na(clr_01)|
    !is.na(bkn_ft.final)|!is.na(ovc_ft.final)
  cloudobs[[i]]<-as.numeric(obs_01)
                   
  # make xts object
  clouds.xts[[i]]<-xts(cbind(bkn_m[[i]], ovc_m[[i]], sct_m[[i]], few_m[[i]], clr_m[[i]], cloudobs[[i]]),
                        order.by=dat[[i]]$dt.hst) 
  names(clouds.xts[[i]])<-paste(names(dat)[i], 
                                c("BKN.m", "OVC.m", "SCT.m", "FEW.m", "CLR", "OBS"), sep="_")

  print(i)
}

### Name each item within the clouds.xts list by station
names(clouds.xts)<-names(dat)

for (i in 1:length(dat)){
  minobs<-apply(clouds.xts[[i]][,1:4], MARGIN=1, FUN=min, na.rm=T)
  meanBKNOVC<-apply(clouds.xts[[i]][,1:2], MARGIN=1, FUN=mean, na.rm=T)

  minobs[!is.finite(minobs)]<-NA
  meanBKNOVC[!is.finite(meanBKNOVC)]<-NA
  
  clouds.xts[[i]]$MinObs<-minobs
  clouds.xts[[i]]$MeanBKNOVC<-meanBKNOVC
}

test<-is.finite(clouds.xts[[1]])

### For each station, reduce the hourly BKN and OVC data:

tally<-function(x){length(which(!is.na(x)))}

# BKN.DY.z<-BKN.NT.z<-BKN.DY.fr<-BKN.NT.fr<-list()
# OVC.DY.z<-OVC.NT.z<-OVC.DY.fr<-OVC.NT.fr<-list()

BKN.MDDY.z<-BKN.MDNT.z<-BKN.MDDY.fr<-BKN.MDNT.fr<-list()
OVC.MDDY.z<-OVC.MDNT.z<-OVC.MDDY.fr<-OVC.MDNT.fr<-list()
SCT.MDDY.z<-SCT.MDNT.z<-SCT.MDDY.fr<-SCT.MDNT.fr<-list()
FEW.MDDY.z<-FEW.MDNT.z<-FEW.MDDY.fr<-FEW.MDNT.fr<-list()
all.MDDY.z<-all.MDNT.z<-all.MDDY.fr<-all.MDNT.fr<-list()
bkn_ovc.MDDY.z<-bkn_ovc.MDNT.z<-bkn_ovc.MDDY.fr<-bkn_ovc.MDNT.fr<-list()
CLR.MDDY.z<-CLR.MDNT.z<-CLR.MDDY.fr<-CLR.MDNT.fr<-list()

cloudobs.DY.ta<-cloudobs.NT.ta<-cloudobs.MDDY.ta<-cloudobs.MDNT.ta<-list()

for (i in 1:length(dat))  {
  ## Separate daytime and nighttime
  # daytime<-which(.indexhour(clouds.xts[[i]])>6 & 
  #                  .indexhour(clouds.xts[[i]])<=18)
  # nttime<-which(.indexhour(clouds.xts[[i]])>18 | 
  #                 .indexhour(clouds.xts[[i]])<=6)
  
  mdday<-which(.indexhour(clouds.xts[[i]])>12 &   # Midday, 1-3 PM
                 .indexhour(clouds.xts[[i]])<=15)
  mdnt<-which(.indexhour(clouds.xts[[i]])>0 &     # Midnight, 1-3 AM
                .indexhour(clouds.xts[[i]])<=3)
  
  ## Aggregate to monthly values
  # BKN.DY.z[[i]]<-apply.monthly(clouds.xts[[i]][daytime, 1], FUN=median, na.rm=T)
  # BKN.NT.z[[i]]<-apply.monthly(clouds.xts[[i]][nttime, 1], FUN=median, na.rm=T)
  # 
  # OVC.DY.z[[i]]<-apply.monthly(clouds.xts[[i]][daytime, 2], FUN=median, na.rm=T)
  # OVC.NT.z[[i]]<-apply.monthly(clouds.xts[[i]][nttime, 2], FUN=median, na.rm=T)
  # 
  # BKN.DY.ta<-apply.monthly(clouds.xts[[i]][daytime, 1], FUN=tally)
  # BKN.NT.ta<-apply.monthly(clouds.xts[[i]][nttime, 1], FUN=tally)
  # OVC.DY.ta<-apply.monthly(clouds.xts[[i]][daytime, 2], FUN=tally)
  # OVC.NT.ta<-apply.monthly(clouds.xts[[i]][nttime, 2], FUN=tally)
  # 
  # cloudobs.DY.ta[[i]]<-apply.monthly(cloudobs[[i]][daytime], FUN=sum)
  # cloudobs.NT.ta[[i]]<-apply.monthly(cloudobs[[i]][nttime], FUN=sum)
  
  # BKN.DY.fr[[i]]<-BKN.DY.ta/cloudobs.DY.ta[[i]]
  # BKN.NT.fr[[i]]<-BKN.NT.ta/cloudobs.NT.ta[[i]]
  # OVC.DY.fr[[i]]<-OVC.DY.ta/cloudobs.DY.ta[[i]]
  # OVC.NT.fr[[i]]<-OVC.NT.ta/cloudobs.NT.ta[[i]]
  
  BKN.MDDY.z[[i]]<-apply.monthly(clouds.xts[[i]][mdday, 1], FUN=median, na.rm=T)
  BKN.MDNT.z[[i]]<-apply.monthly(clouds.xts[[i]][mdnt, 1], FUN=median, na.rm=T)
  
  OVC.MDDY.z[[i]]<-apply.monthly(clouds.xts[[i]][mdday, 2], FUN=median, na.rm=T)
  OVC.MDNT.z[[i]]<-apply.monthly(clouds.xts[[i]][mdnt, 2], FUN=median, na.rm=T)

  SCT.MDDY.z[[i]]<-apply.monthly(clouds.xts[[i]][mdday, 3], FUN=median, na.rm=T)
  SCT.MDNT.z[[i]]<-apply.monthly(clouds.xts[[i]][mdnt, 3], FUN=median, na.rm=T)
  
  FEW.MDDY.z[[i]]<-apply.monthly(clouds.xts[[i]][mdday, 4], FUN=median, na.rm=T)
  FEW.MDNT.z[[i]]<-apply.monthly(clouds.xts[[i]][mdnt, 4], FUN=median, na.rm=T)
  
  all.MDDY.z[[i]]<-apply.monthly(clouds.xts[[i]]$MinObs[mdday], FUN=median, na.rm=T)
  all.MDNT.z[[i]]<-apply.monthly(clouds.xts[[i]]$MinObs[mdnt], FUN=median, na.rm=T)
  
  bkn_ovc.MDDY.z[[i]]<-apply.monthly(clouds.xts[[i]]$MeanBKNOVC[mdday], FUN=median, na.rm=T)
  bkn_ovc.MDNT.z[[i]]<-apply.monthly(clouds.xts[[i]]$MeanBKNOVC[mdnt], FUN=median, na.rm=T)
  
  # BKN.MDDY.ta<-apply.monthly(clouds.xts[[i]][mdday, 1], FUN=tally)
  # BKN.MDNT.ta<-apply.monthly(clouds.xts[[i]][mdnt, 1], FUN=tally)
  # OVC.MDDY.ta<-apply.monthly(clouds.xts[[i]][mdday, 2], FUN=tally)
  # OVC.MDNT.ta<-apply.monthly(clouds.xts[[i]][mdnt, 2], FUN=tally)
  
  all.MDDY.ta<-apply.monthly(clouds.xts[[i]]$MinObs[mdday], FUN=tally)
  all.MDNT.ta<-apply.monthly(clouds.xts[[i]]$MinObs[mdnt], FUN=tally)
  
  bkn_ovc.MDDY.ta<-apply.monthly(clouds.xts[[i]]$MeanBKNOVC[mdday], FUN=tally)
  bkn_ovc.MDNT.ta<-apply.monthly(clouds.xts[[i]]$MeanBKNOVC[mdnt], FUN=tally)
  
  cloudobs.MDDY.ta[[i]]<-apply.monthly(clouds.xts[[i]][mdday, 6], FUN=sum)
  cloudobs.MDNT.ta[[i]]<-apply.monthly(clouds.xts[[i]][mdnt, 6], FUN=sum)
  
  CLR.MDDY.fr[[i]]<-apply.monthly(clouds.xts[[i]][mdday, 5], FUN=sum)/cloudobs.MDDY.ta[[i]]
  CLR.MDNT.fr[[i]]<-apply.monthly(clouds.xts[[i]][mdnt, 5], FUN=sum)/cloudobs.MDNT.ta[[i]]
  
  all.MDDY.fr[[i]]<-all.MDDY.ta/cloudobs.MDDY.ta[[i]]
  all.MDNT.fr[[i]]<-all.MDNT.ta/cloudobs.MDNT.ta[[i]]
  
  bkn_ovc.MDDY.fr[[i]]<-bkn_ovc.MDDY.ta/cloudobs.MDDY.ta[[i]]
  bkn_ovc.MDNT.fr[[i]]<-bkn_ovc.MDNT.ta/cloudobs.MDNT.ta[[i]]
  
  
  # BKN.MDDY.fr[[i]]<-BKN.MDDY.ta/cloudobs.MDDY.ta[[i]]
  # BKN.MDNT.fr[[i]]<-BKN.MDNT.ta/cloudobs.MDNT.ta[[i]]
  # OVC.MDDY.fr[[i]]<-OVC.MDDY.ta/cloudobs.MDDY.ta[[i]]
  # OVC.MDNT.fr[[i]]<-OVC.MDNT.ta/cloudobs.MDNT.ta[[i]]
  
  print(i)
}

# mo.bkn_dynt<-mo.bkn_mddynt<-list()
# mo.ovc_dynt<- mo.ovc_mddynt<-list()

mo.bkn_ovc_mddynt<-list()
mo.all_mddynt<-list()
mo.clr_mddynt<-list()

for (i in 1:length(dat)){
  # mo.bkn_dynt[[i]]<-merge(xts(BKN.DY.z[[i]], 
  #                        order.by=as.Date(paste0(substr(index(BKN.DY.z[[i]]), 1, 7), "-01"))),
  #                    xts(BKN.NT.z[[i]], 
  #                        order.by=as.Date(paste0(substr(index(BKN.NT.z[[i]]), 1, 7), "-01"))),
  #                    xts(BKN.DY.fr[[i]], 
  #                        order.by=as.Date(paste0(substr(index(BKN.DY.fr[[i]]), 1, 7), "-01"))),
  #                    xts(BKN.NT.fr[[i]], 
  #                        order.by=as.Date(paste0(substr(index(BKN.NT.fr[[i]]), 1, 7), "-01"))))
  
  # mo.bkn_mddynt[[i]]<-merge(xts(BKN.MDDY.z[[i]], 
  #                               order.by=as.Date(paste0(substr(index(BKN.MDDY.z[[i]]), 1, 7), "-01"))),
  #                           xts(BKN.MDNT.z[[i]], 
  #                               order.by=as.Date(paste0(substr(index(BKN.MDNT.z[[i]]), 1, 7), "-01"))),
  #                           xts(BKN.MDDY.fr[[i]], 
  #                               order.by=as.Date(paste0(substr(index(BKN.MDDY.fr[[i]]), 1, 7), "-01"))),
  #                           xts(BKN.MDNT.fr[[i]], 
  #                               order.by=as.Date(paste0(substr(index(BKN.MDNT.fr[[i]]), 1, 7), "-01"))))

  # mo.ovc_dynt[[i]]<-merge(xts(OVC.DY.z[[i]], 
  #                        order.by=as.Date(paste0(substr(index(OVC.DY.z[[i]]), 1, 7), "-01"))),
  #                    xts(OVC.NT.z[[i]], 
  #                        order.by=as.Date(paste0(substr(index(OVC.NT.z[[i]]), 1, 7), "-01"))),
  #                    xts(OVC.DY.fr[[i]], 
  #                        order.by=as.Date(paste0(substr(index(OVC.DY.fr[[i]]), 1, 7), "-01"))),
  #                    xts(OVC.NT.fr[[i]], 
  #                        order.by=as.Date(paste0(substr(index(OVC.NT.fr[[i]]), 1, 7), "-01"))))
  
  # mo.ovc_mddynt[[i]]<-merge(xts(OVC.MDDY.z[[i]], 
  #                               order.by=as.Date(paste0(substr(index(OVC.MDDY.z[[i]]), 1, 7), "-01"))),
  #                           xts(OVC.MDNT.z[[i]], 
  #                               order.by=as.Date(paste0(substr(index(OVC.MDNT.z[[i]]), 1, 7), "-01"))),
  #                           xts(OVC.MDDY.fr[[i]], 
  #                               order.by=as.Date(paste0(substr(index(OVC.MDDY.fr[[i]]), 1, 7), "-01"))),
  #                           xts(OVC.MDNT.fr[[i]], 
  #                               order.by=as.Date(paste0(substr(index(OVC.MDNT.fr[[i]]), 1, 7), "-01"))))

  mo.bkn_ovc_mddynt[[i]]<-merge(xts(bkn_ovc.MDDY.z[[i]], 
                                order.by=as.Date(paste0(substr(index(bkn_ovc.MDDY.z[[i]]), 1, 7), "-01"))),
                            xts(bkn_ovc.MDNT.z[[i]], 
                                order.by=as.Date(paste0(substr(index(bkn_ovc.MDNT.z[[i]]), 1, 7), "-01"))),
                            xts(bkn_ovc.MDDY.fr[[i]], 
                                order.by=as.Date(paste0(substr(index(bkn_ovc.MDDY.fr[[i]]), 1, 7), "-01"))),
                            xts(bkn_ovc.MDNT.fr[[i]], 
                                order.by=as.Date(paste0(substr(index(bkn_ovc.MDNT.fr[[i]]), 1, 7), "-01"))))
  
  mo.all_mddynt[[i]]<-merge(xts(all.MDDY.z[[i]], 
                                    order.by=as.Date(paste0(substr(index(all.MDDY.z[[i]]), 1, 7), "-01"))),
                                xts(all.MDNT.z[[i]], 
                                    order.by=as.Date(paste0(substr(index(all.MDNT.z[[i]]), 1, 7), "-01"))),
                                xts(all.MDDY.fr[[i]], 
                                    order.by=as.Date(paste0(substr(index(all.MDDY.fr[[i]]), 1, 7), "-01"))),
                                xts(all.MDNT.fr[[i]], 
                                    order.by=as.Date(paste0(substr(index(all.MDNT.fr[[i]]), 1, 7), "-01"))))
  mo.clr_mddynt[[i]]<-merge(xts(CLR.MDDY.fr[[i]], 
                                order.by=as.Date(paste0(substr(index(CLR.MDDY.fr[[i]]), 1, 7), "-01"))),
                            xts(CLR.MDNT.fr[[i]], 
                                order.by=as.Date(paste0(substr(index(CLR.MDNT.fr[[i]]), 1, 7), "-01"))))
                          
  
    # Name the columns
  # names(mo.bkn_dynt[[i]])<-names(mo.bkn_mddynt[[i]])<-c("BKNday.z", "BKNnt.z", "BKNday.fr", "BKNnt.fr")
  # names(mo.ovc_dynt[[i]])<-names(mo.ovc_mddynt[[i]])<-c("OVCday.z", "OVCnt.z", "OVCday.fr", "OVCnt.fr")

  names(mo.bkn_ovc_mddynt[[i]])<-c("OBday.z", "OBnt.z", "OBday.fr", "OBnt.fr")
  names(mo.all_mddynt[[i]])<-c("OBSFday.z", "OBSFnt.z", "OBSFday.fr", "OBSFnt.fr")
  names(mo.clr_mddynt[[i]])<-c("CLRday.fr", "CLRnt.fr")
  
  }


### Name each item within the mo.bkn_ovc_mddynt and mo.all_mddynt lists by station
names(mo.bkn_ovc_mddynt)<-names(mo.all_mddynt)<-names(mo.clr_mddynt)<-names(dat)

# pdf("BKN_OVC_monthly_dynt.pdf")
# par(mfrow=c(1,2))
# for (i in 1:length(mo.bkn_dynt)){
#   b1<-plot(mo.bkn_dynt[[i]][,1:2], col=c("red", "blue"), lty=1, ylim=c(0,3000),
#           main=paste(names(mo.bkn_dynt)[i], "BKN1 base (m)"))
#   b1<-addLegend(legend.loc = "topleft", 
#                 legend.names= c("0700-1800", "1900-0600"),
#                 fill=c("red", "blue"))
#   addSeries(mo.bkn_dynt[[i]][,3:4], col=c("red", "blue"), lty=1, ylim=c(0,1), 
#             main=paste(names(mo.bkn_dynt)[i],"BKN freq"))
#   print(b1)
#   o1<-plot(mo.ovc_dynt[[i]][,1:2], col=c("red", "blue"), lty=1, ylim=c(0,3000), 
#           main=paste(names(mo.ovc_dynt)[i], "OVC1 base (m)"))
#   o1<-addLegend(legend.loc = "topleft", 
#                 legend.names= c("0700-1800", "1900-0600"),
#                 fill=c("red", "blue"))
#   addSeries(mo.ovc_dynt[[i]][,3:4], col=c("red", "blue"), lty=1, ylim=c(0,1), 
#             main=paste(names(mo.ovc_dynt)[i],"OVC freq"))
#   print(o1)
# }
# dev.off()

pdf("ALL_monthly_mddynt.pdf")
par(mfrow=c(1,2))
for (i in 1:length(mo.bkn_ovc_mddynt)){
  bo1<-plot(mo.bkn_ovc_mddynt[[i]][,1:2], col=c("red", "blue"), lty=1, ylim=c(0,3000),
           main=paste(names(mo.bkn_ovc_mddynt)[i], "BKNOVC base (m)"))
  bo1<-addLegend(legend.loc = "topleft", 
                legend.names= c("1300-1500", "0100-0300"),
                fill=c("red", "blue"))
  addSeries(mo.bkn_ovc_mddynt[[i]][,3:4], col=c("red", "blue"), lty=1, ylim=c(0,1), 
            main=paste(names(mo.bkn_ovc_mddynt)[i],"BKNOVC freq"))
  print(bo1)
  a1<-plot(mo.all_mddynt[[i]][,1:2], col=c("red", "blue"), lty=1, ylim=c(0,3000), 
           main=paste(names(mo.all_mddynt)[i], "ALL base (m)"))
  a1<-addLegend(legend.loc = "topleft", 
                legend.names= c("1300-1500", "0100-0300"),
                fill=c("red", "blue"))
  addSeries(mo.all_mddynt[[i]][,3:4], col=c("red", "blue"), lty=1, ylim=c(0,1), 
            main=paste(names(mo.all_mddynt)[i],"ALL freq"))
  print(a1)
  
}
dev.off()


#############################################################################
# Reduce data further: annual series
yr.bkn_ovc_mddynt<-yr.all_mddynt<-yr.clr_mddynt<-list()

for (i in 1:length(dat)){
  for (j in 1:4){    # each column: z day, z night, freq day, freq night
    yr.ob<-apply.yearly(mo.bkn_ovc_mddynt[[i]][,j], mean, na.rm=T)
    yr.obsf<-apply.yearly(mo.all_mddynt[[i]][,j], mean, na.rm=T)
    
    if(j==1){
      yr.bkn_ovc_mddynt[[i]]<-yr.ob
      yr.all_mddynt[[i]]<-yr.obsf

    }else{
      yr.bkn_ovc_mddynt[[i]]<-merge(yr.bkn_ovc_mddynt[[i]], yr.ob, all=T)
      yr.all_mddynt[[i]]<-merge(yr.all_mddynt[[i]], yr.obsf, all=T)}
    
  print(paste(i, j))
  }
  
  for (k in 1:2){
    yr.clr<-apply.yearly(mo.clr_mddynt[[i]][,k], mean)
    if (k==1){yr.clr_mddynt[[i]]<-yr.clr
    }else{
      yr.clr_mddynt[[i]]<-merge(yr.clr_mddynt[[i]], yr.clr, all=T)
    }
  }
  # names(yr.bkn_ovc_mddynt[[i]])<-c("BKNday.z", "BKNnt.z", "BKNday.fr", "BKNnt.fr")
  # names(yr.all_mddynt[[i]])<-c("OVCday.z", "OVCnt.z", "OVCday.fr", "OVCnt.fr")
  names(yr.clr_mddynt[[i]])<-c("CLRday.fr", "CLRnt.fr")
}
### Name each item within the yr.bkn_ovc_mddynt and yr.all_mddynt lists by station
names(yr.bkn_ovc_mddynt)<-names(yr.all_mddynt)<-names(yr.clr_mddynt)<-names(dat)

pdf("OB_ALL_annual_mddynt.pdf", width=11, height=7)
par(mfrow=c(1,2))
for (i in 1:length(dat)){
z<-merge(yr.bkn_ovc_mddynt[[i]][,1:2],yr.all_mddynt[[i]][,1:2], all=T)
fr<-merge(yr.bkn_ovc_mddynt[[i]][,3:4], yr.all_mddynt[[i]][,3:4], all=T)

p1<-plot(z, main=paste(names(dat)[i], "ceiling ht (m)"),
     col=c("red", "blue", "red", "blue"), lty=c(1,1,2,2),
     ylim=c(0,3000))
p1<-addLegend(legend.loc = "bottomleft", 
              legend.names= c("OVC/BKN: 1400", "OVC/BKN: 0200",
                              "ALL: 1400", "ALL: 0200"),
              col=c("red", "blue", "red", "blue"),
              lty=c(1,1,2,2))
p1<-addSeries(fr, main=paste(names(dat)[i], "frequency"), 
          col=c("red", "blue", "red", "blue"), lty=c(1,1,2,2), 
          ylim=c(0,1))
print(p1)
}
dev.off()


for (i in 1:length(dat)){
  mo<-merge(mo.bkn_ovc_mddynt[[i]], mo.all_mddynt[[i]], mo.clr_mddynt[[i]], all=T)
  yr<-merge(yr.bkn_ovc_mddynt[[i]], yr.all_mddynt[[i]], yr.clr_mddynt[[i]], all=T)
  
  if(i==1){
    mo.master.ceil<-mo
    yr.master.ceil<-yr
    
  }else{
    mo.master.ceil<-merge(mo.master.ceil, mo, all=T)
    yr.master.ceil<-merge(yr.master.ceil, yr, all=T)}
}

names(mo.master.ceil)<-names(yr.master.ceil)<-
  paste(rep(names(dat),each=10), 
        rep(c(names(yr.bkn_ovc_mddynt[[1]]), 
              names(yr.all_mddynt[[1]]),
              names(yr.clr_mddynt[[1]])), 
            times=length(dat)), sep="_")

### Export the monthly and annual data
write.csv(as.data.frame(mo.master.ceil), file="CeilometerOB_all_monthly.csv",  row.names=T)
write.csv(as.data.frame(yr.master.ceil), file="CeilometerOB_all_yearly.csv",  row.names=T)


# master.ceil.90<-window(master.ceil, start="1990-01-01", end="2018-12-01")
# master.bkn.mddy.z<-master.ceil.90[,grep("BKNday.z", names(master.ceil.90))]
# master.bkn.mddy.fr<-master.ceil.90[,grep("BKNday.fr", names(master.ceil.90))]
# 
# master.ovc.mddy.z<-master.ceil.90[,grep("OVCday.z", names(master.ceil.90))]
# master.ovc.mddy.fr<-master.ceil.90[,grep("OVCday.fr", names(master.ceil.90))]
# 
# names(master.bkn.mddy.z)
###
# PHTO: WW, Ha
# PHLI: WW, Ka
# PHNL: LW, Oa ---
# PHOG: WW, Ma
# PHKO: LW, Ha ---
# PHNG: WW, Oa

# par(mfrow=c(1,2))
# b1<-plot(master.bkn.mddy.z, main="Midday BKN1 (m)", 
#      col=c("red", "green", "orange", "magenta", "red","orange"),
#      lty=c(1,1,2,1,2,1),ylim=c(0,3000))
# b1<-addSeries(master.bkn.mddy.fr, main="Midday BKN1 (freq)",
#      col=c("red", "green", "orange", "magenta", "red","orange"),
#      lty=c(1,1,2,1,2,1), ylim=c(0,0.8) )
# b1
# 
# o1<-plot(master.ovc.mddy.z, main="Midday OVC1 (m)", 
#          col=c("red", "green", "orange", "magenta", "red","orange"),
#          lty=c(1,1,2,1,2,1), ylim=c(0,3000))
# o1<-addSeries(master.ovc.mddy.fr, main="Midday OVC1 (freq)",
#               col=c("red", "green", "orange", "magenta", "red","orange"),
#               lty=c(1,1,2,1,2,1), ylim=c(0,0.8))
# o1

dygraph(bkn.momn.xts)
dygraph(bkn.mofr.xts)






# ################# Less important ###########################
# vv.phto<-list()
# for (i in 1:length(dat.phto$metar)){
#   vv.n<-gregexpr("VV", dat.phto$metar[i])
#   if (vv.n[[1]][1]!=-1){
#     vv.phto[[i]]<-substring(dat.phto$metar[i], vv.n[[1]][1], vv.n[[1]][1]+6)
#   }else{vv.phto[[i]]<-NA}
# }
# vv.phto<-unlist(vv.phto)
# 
# cig.phto<-list()
# for (i in 1:length(dat.phto$metar)){
#   cig.n<-gregexpr("CIG", dat.phto$metar[i])
#   if (cig.n[[1]][1]!=-1){
#     cig.phto[[i]]<-substring(dat.phto$metar[i], cig.n[[1]][1], cig.n[[1]][1]+6) # bottom layer
#   }else{cig.phto[[i]]<-NA}
# }
# cig.phto<-unlist(cig.phto)
# 
# 
# vv1_ft<-as.numeric(substr(vv.phto, start=4, stop=6))*100
# cig_m<-as.numeric(substr(vv.phto, start=4, stop=6))*100
# 
# vv1_m<-vv1_ft*0.3048
# cig_m<-cig_ft*0.3048

metar.cloud<-data.frame(station=stn,
                        datetime=dt,
                        BKN1=bkn1_m,
                        OVC1=ovc1_m)

dat.phli<-metar.cloud[metar.cloud$station=="PHLI",]
dat.phto<-metar.cloud[metar.cloud$station=="PHTO",]

# library(xts)
# dat.phto.xts<-xts(dat.phto[,-c(1,2)], order.by=dat.phto$datetime)
# dat.phli.xts<-xts(dat.phli[,-c(1,2)], order.by=dat.phli$datetime)
# tzone(dat.phto.xts)<-"HST"
# tzone(dat.phli.xts)<-"HST"
# 
# dat.phto.1999_17.xts<-window(dat.phto.xts, 
#                              start="1999-01-01", 
#                              end = "2017-12-31")
# dat.phli.1999_17.xts<-window(dat.phli.xts, 
#                              start="1999-01-01", 
#                              end = "2017-12-31")
# 
# dat.phto.daily<-apply.daily(dat.phto.1999_17.xts, FUN="mean", na.rm=T)
# dat.phli.daily<-apply.daily(dat.phli.1999_17.xts, FUN="mean", na.rm=T)
# plot(dat.phto.daily, col=c("black", "blue"))
# 
# frac<-function(x){length(which(!is.na(x)))/length(x)}
# phto.bkn.freq<-apply.daily(dat.phto.1999_17.xts$BKN1, FUN=frac)
# phto.ovc.freq<-apply.daily(dat.phto.1999_17.xts$OVC1, FUN=frac)
# phli.bkn.freq<-apply.daily(dat.phli.1999_17.xts$BKN1, FUN=frac)
# phli.ovc.freq<-apply.daily(dat.phli.1999_17.xts$OVC1, FUN=frac)
# 
# addSeries(phto.bkn.freq, col="black")
# 
# plot(dat.phli.daily, col=c("black", "blue"))
# addSeries(phli.bkn.freq, col="black")


################################################################################## 
# 
# library(dygraphs)
# dygraph(daytime.phto) %>%
#   dySeries("freq", axis="y2")
# 
# library(zyp)
# y<-as.numeric(daytime.phto$BKN1)
# x<-rep(0:18, each=12)+seq(from=0, to=0.9, by=1/12)
# zyp.sen(y~x)
# confint(zyp.sen(y~x)) # rising Hilo BKN1 cloud base, 23m/yr
# 
# y<-as.numeric(daytime.phto$OVC1)
# x<-rep(0:18, each=12)+seq(from=0, to=0.9, by=1/12)
# zyp.sen(y~x)
# confint(zyp.sen(y~x)) # rising Hilo OVC1 cloud base, 7m/yr
# 
# y<-as.numeric(daytime.phto$bkn.freq)
# x<-rep(0:18, each=12)+seq(from=0, to=0.9, by=1/12)
# zyp.sen(y~x)
# confint(zyp.sen(y~x)) # decreasing Hilo BKN1 cloud frequency, -0.005%/yr
# 
# y<-as.numeric(daytime.phto$ovc.freq)
# x<-rep(0:18, each=12)+seq(from=0, to=0.9, by=1/12)
# zyp.sen(y~x)
# confint(zyp.sen(y~x)) # decreasing Hilo OVC1 cloud frequency, -0.012%/yr
# 
# # Lihue
# y<-as.numeric(daytime.phli$BKN1)
# x<-rep(0:18, each=12)+seq(from=0, to=0.9, by=1/12)
# zyp.sen(y~x)
# confint(zyp.sen(y~x)) # NS change to Lihue BKN1 cloud base
# 
# y<-as.numeric(daytime.phli$OVC1)
# x<-rep(0:18, each=12)+seq(from=0, to=0.9, by=1/12)
# zyp.sen(y~x)
# confint(zyp.sen(y~x)) # rising to Lihue OVC1 cloud base, 4.8m/yr
# 
# y<-as.numeric(daytime.phli$bkn.freq)
# x<-rep(0:18, each=12)+seq(from=0, to=0.9, by=1/12)
# zyp.sen(y~x)
# confint(zyp.sen(y~x)) # NS Lihue BKN1 cloud frequency
# 
# y<-as.numeric(daytime.phli$ovc.freq)
# x<-rep(0:18, each=12)+seq(from=0, to=0.9, by=1/12)
# zyp.sen(y~x)
# confint(zyp.sen(y~x)) # NS Lihue OVC1 cloud frequency
