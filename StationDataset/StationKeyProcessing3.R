## StationKey_processing3.R
## Author: Aurora Kagawa-Viviani
## Date: Apr 13-16 2016; re-run 6/17/2017; re-run 5/6/2018
## Notes: Script for organizing station metadata and generating a final station key

########################################################################
## NCDC                                                              ###
########################################################################
# use "monthly/monthly_mrg_completeNCDCstns.csv" for 299 stations with Tmax/Tmin data
# use "ghcnd-stationsHI.csv" for list of 688 HI stations downloaded: lat, long, elev, name
# use "ncdcstation2.csv" for parsed mshr.txt metadata and station relocations

setwd("xxx/TempMapping/NCDC_DailyTemp")

### Match everything else only to the stations used in the monthly Temp dataset
monthly<-read.csv("monthly/monthly_mrg_completeNCDCstns.csv")
Tghcnd<-unique(monthly$Station)             # 299 values
SK<-data.frame(ghcnd_id = Tghcnd,
               start = tapply(monthly$MonthYear, monthly$Station, min),
               end   = tapply(monthly$MonthYear, monthly$Station, max),
               Tmin  = tapply(monthly$Tmin, monthly$Station, mean),
               Tmax  = tapply(monthly$Tmax, monthly$Station, mean),
               DTR   = tapply(monthly$DTR, monthly$Station, mean),
               Tavg  = tapply(monthly$Tavg, monthly$Station, mean),
               Obs   = tapply(monthly$Tavg, monthly$Station, length))
               

allghcnd<-read.csv("ghcnd-stationsHI.csv", strip.white=T)     # 688   5      ghcnd = "ID"
# rename column for merging
names(allghcnd)[grep("ID", names(allghcnd))]<-"ghcnd_id"  
names(allghcnd)[grep("elev", names(allghcnd))]<-"elev.m"

#### MERGE dataframes SK (299), allghcnd (688)
merged<-merge(SK, allghcnd, by="ghcnd_id", all=F)   # take ONLY 299 stations of interest
write.csv(merged, file="mergedNCDCstation299.csv", row.names=F)

#### Optional: Compare to metadata on station movements, etc: mshr(2749)  
mshr<-read.csv("ncdcstation2.csv") # 2749   17    ghcnd = "ghcnd_id"  NO Midway
meta<-list()
for (i in 1:length(Tghcnd)){
  meta[[i]]<-subset(mshr, as.character(mshr$ghcnd_id)==as.character(Tghcnd[i]))
  
  # look at only most recent coordinates
  if (i==1){metaLast<-meta[[i]][dim(meta[[i]])[1],]
  }else{metaLast<-rbind(metaLast, meta[[i]][dim(meta[[i]])[1],])}
} 

mergedmshr<-merge(merged, metaLast, by="ghcnd_id")
write.csv(mergedmshr, file="merged_mshr_NCDCstation298.csv", row.names=F)  

##################################################################
## FindingSKNs.R
## Goal: checking NCDC station locations
## Script to match station names with State Key Numbers (SKN) values
##    documented elsewhere by Abby Frazier and/or Jon Eischeid
## Script used exact matching ("which") and fuzzy matching ("agrep")

## Step1 following this is manual examination of NCDC station names that do not match other files
## Step2 following this is to extract lat/long/elevation from the other files
##    in some cases, NCDC locations are more precise; keep these
##    in other cases, NCDC locations are off; use the RF atlas corrected values here

###############################################################################
## open mergedNCDCstation299.csv (merged_mshr_NCDCstation299.csv) in excel, ##
###   and save to drop quotes                                                ## 
###############################################################################
dat<-read.csv("merged_mshr_NCDCstation298_rmwhtspc.csv", strip.white = T, stringsAsFactors=F)  

ghcn<-as.character(dat$ghcnd_id) # create a list of unique ghcnd IDs
NCDC<-dat$name              # use these names

#Identify RAWS
#remove "HAWAII" to aid in string fuzzy matching
raws     <-which(substr(dat$ghcnd_id, start=1, stop=3)=="USR")
rawsHI<-substr(NCDC[raws], start=1, stop=nchar(NCDC[raws])-7)  
NCDCshort<-NCDC; NCDCshort[raws]<-rawsHI
  
## match NCDCshort to SKN

ncdcSKN     <-read.csv("../NCDCStationKeys/ncdc_hedr2_rmwhtspc.csv", stringsAsFactors = F); SKN<-vector("character") # initial ncdc_hedr.csv was garbled
tempSKN     <-read.csv("../NCDCStationKeys/TempHedrCurr.csv", stringsAsFactors = F); SKN2<-vector("character")
JonSKN      <-read.csv("../NCDCStationKeys/JonMonthlyHedr.csv", stringsAsFactors = F); SKN3<-vector("character")
rfhedrSKN   <-read.csv("../NCDCStationKeys/RF_Hedr_4.13.11.csv", stringsAsFactors = F); SKN4<-vector("character")
rf2015SKN   <-read.csv("../NCDCStationKeys/RFFinalStationData_Used_201504pr.csv", stringsAsFactors = F); SKN5<-vector("character")
rfun2015SKN <-read.csv("../NCDCStationKeys/RFFinalStations_NotUsed_201504pr.csv", stringsAsFactors = F); SKN6<-vector("character")

#### EXACT MATCHES in name
for (i in 1:length(NCDCshort)){
  SKNindex<-which(ncdcSKN$Name==NCDCshort[i])
  if (length(SKNindex)!=0){SKN[i]<-paste(ncdcSKN$SKN[SKNindex], collapse="/")
  }else{SKN[i]<-NA}
  
  SKN2index<-which(tempSKN$Name==NCDCshort[i])
  if (length(SKN2index)!=0){SKN2[i]<-paste(tempSKN$SKN[SKN2index], collapse="/")
  }else{SKN2[i]<-NA}
  
  SKN3index<-which(JonSKN$Name==NCDCshort[i])
  if (length(SKN3index)!=0){SKN3[i]<-paste(JonSKN$SKN[SKN3index], collapse="/")
  }else{SKN3[i]<-NA}
  
  SKN4index<-which(rfhedrSKN$Name==NCDCshort[i])
  if (length(SKN4index)!=0){SKN4[i]<-paste(rfhedrSKN$SKN[SKN4index], collapse="/")
  }else{SKN4[i]<-NA}
  
  SKN5index<-which(rf2015SKN$Name==NCDCshort[i])
  if (length(SKN5index)!=0){SKN5[i]<-paste(rf2015SKN$SKN[SKN5index], collapse="/")
  }else{SKN5[i]<-NA}
  
  SKN6index<-which(rfun2015SKN$Name==NCDCshort[i])
  if (length(SKN6index)!=0){SKN6[i]<-paste(rfun2015SKN$SKN[SKN6index], collapse="/")
  }else{SKN6[i]<-NA}
  
  print(i)
}

#### FUZZY MATCHING
SKNa<-vector("character")
SKN2a<-vector("character")
SKN3a<-vector("character")
SKN4a<-vector("character")
SKN5a<-vector("character")
SKN6a<-vector("character")

for (i in 1:length(NCDCshort)){
  SKNaindex<-agrep(NCDCshort[i], ncdcSKN$Name, max.distance=0.1, ignore.case=T)
  if (length(SKNaindex)!=0){SKNa[i]<-paste(ncdcSKN$SKN[SKNaindex], collapse="/")}
  else{SKNa[i]<-NA}
  
  SKN2aindex<-agrep(NCDCshort[i], tempSKN$Name, max.distance=0.1, ignore.case=T)
  if (length(SKN2aindex)!=0){SKN2a[i]<-paste(tempSKN$SKN[SKN2aindex], collapse="/")}
  else{SKN2a[i]<-NA}
  
  SKN3aindex<-agrep(NCDCshort[i], JonSKN$Name, max.distance=0.1, ignore.case=T)
  if (length(SKN3aindex)!=0){SKN3a[i]<-paste(JonSKN$SKN[SKN3aindex], collapse="/")}
  else{SKN3a[i]<-NA}
  
  SKN4aindex<-agrep(NCDCshort[i], rfhedrSKN$Name, max.distance=0.1, ignore.case=T)
  if (length(SKN4aindex)!=0){SKN4a[i]<-paste(rfhedrSKN$SKN[SKN4aindex], collapse="/")}
  else{SKN4a[i]<-NA}
  
  SKN5aindex<-agrep(NCDCshort[i], rf2015SKN$Name, max.distance=0.1, ignore.case=T)
  if (length(SKN5aindex)!=0){SKN5a[i]<-paste(rf2015SKN$SKN[SKN5aindex], collapse="/")}
  else{SKN5a[i]<-NA}
  
  SKN6aindex<-agrep(NCDCshort[i], rfun2015SKN$Name, max.distance=0.1, ignore.case=T)
  if (length(SKN6aindex)!=0){SKN6a[i]<-paste(rfun2015SKN$SKN[SKN6aindex], collapse="/")}
  else{SKN6a[i]<-NA}
  
  print(i)
}

out<-cbind(Station=ghcn,
           NCDCname=NCDCshort,
           ncdc_hedr=SKN,
           ncdc_hedrfuzzy=SKNa,
           temp_hedr=SKN2,
           temp_hedrfuzzy=SKN2a,
           jon_hedr=SKN3,
           jon_hedrfuzzy=SKN3a,
           rf_hedr=SKN4,
           rf_hedrfuzzy=SKN4a,
           rf_2015=SKN5,
           rf_2015fuzzy=SKN5a,
           rfun_2015=SKN6,
           rfun_2015fuzzy=SKN6a)

## Compare ghcnd (688) locations to matched SKN #s
merged2<-cbind(dat, out)
write.csv(merged2, "../NCDCStationKeys/SKNcomparisonMay2018_298.csv", row.names=F)

#####################################################################################
## IN EXCEL: For each station, manually match the SKN that converges across multiple 
##   files for a given name.  Where no SKN pops up, manually compare with the two  
##   RF atlas files for matching dates/similar location and elevation.
## ADD in HaleNet station data/SKN values here!! (could have added them prior)
## Save file as "MatchedSKN_ghcnd.csv"-- includes overall station stats
#####################################################################################

##############################################################
##  Extract locations from the Rainfall Atlas site listing  ##
##  Built off: ExtractLocations3.R                          ##
##############################################################
setwd("xxx/TempMapping/NCDCStationKeys")

#matchedSKN<-read.csv("MatchedSKN_ghcnd.csv", stringsAsFactors=F)
#matchedSKN<-read.csv("MatchedSKN_ghcnd2.csv", stringsAsFactors=F)
#matchedSKN<-read.csv("MatchedSKN_ghcnd3.csv", stringsAsFactors=F)
matchedSKN<-read.csv("MatchedSKN_ghcnd4.csv", stringsAsFactors=F)

dat<-matchedSKN[,-c(13:26)]     # remove extraneous columns
dat$SKN<-as.character(dat$SKN)  # set SKN to string, not numeric

loc_RFused<-read.csv("RFFinalStationData_Used_201504pr.csv", stringsAsFactors=F)[,c(1:4, 14:17, 96, 95)]
loc_RFunused<-read.csv("RFFinalStations_NotUsed_201504pr.csv", stringsAsFactors=F)[,c(1:4, 14:19)]
loc_RF<-rbind(loc_RFused, loc_RFunused)
loc_RF$SKN<-as.character(loc_RF$SKN)  # set SKN to string

ghcnDLoc<-merge(dat, loc_RF, by="SKN", all.x=T)  # Merge dataframes but keep all 299 stations

#write.csv(ghcnDLoc, file="StationLocationsApr2016.csv", row.names=F)
#write.csv(ghcnDLoc, file="StationLocationsApr20162.csv", row.names=F)
#write.csv(ghcnDLoc, file="StationLocationsApr20163.csv", row.names=F)
write.csv(ghcnDLoc, file="StationLocationsApr201805.csv", row.names=F)

###############################################################################
## In Excel, compare elevations from NCDC data and RF atlas data; compare dates of 
##   observation and temp vs elevation for outliers, look for station mismatches
## In ArcMap (see HITempStations.mxd), compare the ghcnd stations with 
##   RF atlas stations over a DEM; note that ghcnd stations often wrong (in water)
##
##  ITERATE and rerun the above script until satisfied
###############################################################################

########################################################
##  Generate an NCDC corrected location station key   ##
########################################################
##  Compare also to merged_mshr_NCDCstation299.csv;
compr<-read.csv("../NCDC_DailyTemp/merged_mshr_NCDCstation298_rmwhtspc.csv")[,-c(2:12, 15, 17, 26)] # remove extraneous
master<-merge(ghcnDLoc, compr, by="ghcnd_id", all.x=T)
head(master)

## Very little difference between ghcnd station records and mshr last records: GOOD
diffLat<-master$lat-master$lat_dec; max(abs(diffLat), na.rm=T)
diffLon<-master$long-master$lon_dec; max(abs(diffLon), na.rm=T)
## Elev not examined since in feet
## But how do these line up with DEMs??  Not always so good.

##  Identify higher precision and better documented stations for GHCND (mshr)
which(master$precis=="DDMMSS" & master$datumh=="NAD83")
precision<-nchar(as.character(master$precis))>5 #DDMMSS, DDdddd, DDddddd
datumNAD83<-master$datumh=="NAD83" 
datumOldHi<-master$datumh=="OLD HAWAIIAN"
hq<-precision & (datumNAD83|datumOldHi)

##  Use ALL RF atlas values for Lat/Long for each station
master$CorrElev_m<-master$ElevM
master$CorrLat_DD<-master$Lat_DD
master$CorrLong_DD<-master$Lon_DD

##  Identify missing stations not documented by RF Atlas
##  Use ghcnd locations for these
miss<-is.na(master$Name)
  notWGS<-master[miss,]
  write.csv(notWGS, file="notWGS.csv", row.names=F)

master$ghcn<-master$precis; master$ghcn[!miss]<-NA    
master$CorrElev_m[miss]<-master$elev.m[miss]
master$CorrLat_DD[miss]<-master$lat[miss]
master$CorrLong_DD[miss]<-master$long[miss]
		
names(master)[9]<-"MoObs"
names(master)[10:13]<-paste("ghcnd_", names(master)[10:13], sep="")
names(master)[14:22]<-paste("rfa_", names(master)[14:22], sep="")
#write.csv(master, "NCDCkey_Apr2016.csv", row.names=F)
#write.csv(master, "NCDC_HNkey_Jun2017.csv", row.names=F)
write.csv(master, "NCDC_HNkey_May2018.csv", row.names=F)

#############################################################
##  In Excel, add the NOAA-ESRL-GMD MLO station 
##  For station metadata: http://www.esrl.noaa.gov/gmd/dv/site/site.php?code=MLO
##   mlo<-data.frame(Station	   = "MLO", Elevation	 = 3397.00,
##                Latitude	 = 19.5362, Longitude	 = -155.5763)
##  Save file of all stations as ## StationKey_all.csv ##
##  Next script:

#############################################################
##  In Excel, remove Midway and French Frigate Shoals (total stations 309)
## NCDC: 297, HN: 11, MLO: 1
