## ExtractSpatialCovariates.R (from ExtractRFValues.R)
## Project: HI Temp
## Author: A. Kagawa-Viviani
## Date: 25 April 2016, revised 6/16/2017; re-run 5/6/2018
## Notes: Extract covariates based on layers of spatial data
## script based on ExtractMeanTempValues.r (Ryan Longman)

library('raster')
library('rgdal')

#Set Working Directory
setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping")

#Read in station key
stationkey<- read.csv(file = "NCDCStationKeys/StationKey_allMay2018.csv", header = TRUE)

#Set spatial coordinates to create a Spatial object:
coordinates(stationkey) <- c("CorrLong_DD","CorrLat_DD")  # define x & y as CorrLong_DD and CorrLat_DD
coordSK<- coordinates(stationkey)   # these are WGS84, but some are NAD83. 

#################### Find covariates ####################################
#### Ensure rasters are in GCS WGS84 in Arc before extracting covariates
#### Station coordinates (from RF Atlas) are GCS WGS84 except for a few NAD83, but no diff

## Most HI Temp covariates here:
dirs<-list.dirs("C:/Users/Aurora/OneDrive/GIS/HITemp_rastercovariates", 
                full.names=T)  # list directories
  dem<-dirs[grep("dem", dirs)]
  tpi<-dirs[grep("tpi", dirs)]       # topographic position
  cpi<-dirs[grep("cpi", dirs)]       # coastal proximity
  
  # albedo
  albedo<-dirs[grep("albedo", dirs)]
  
  # vegetation
  ndvi<-dirs[grep("ndvi_ann", dirs)]
  evi<-dirs[grep("evi_ann", dirs)]
  lai<-dirs[grep("lai_ann", dirs)]
  veg_ht<-dirs[grep("veg_ht_ann", dirs)]
  fr_veg<-dirs[grep("fr_v_c_ann", dirs)]
 
  # Cloud cover here:  
  clouds<-dirs[grep("CloudFreq_month_raster/cl_frq", dirs)]
  
  # Wind speed here:  
  winds<-dirs[grep("WindSpeed_ann_hr_raster/wind_sd", dirs)]

## Rainfall Atlas data here:
rfdirs<-list.dirs("C:/Users/Aurora/OneDrive/GIS/StateRFGrids_mm", full.names=T)
  staterf<-rfdirs[grep("staterf", rfdirs)]
  
################ Visualize spatial covariates ######################################
dem.1<-raster(dem[2]); plot(dem.1, main="DEM")
tpi.stk<-stack(tpi); plot(tpi.stk, main="TPI")
cpi.1<-raster(cpi); plot(cpi.1, main="CPI")
  
albedo.ann<-raster(albedo) # read as raster, etc
plot(albedo.ann) # plot, etc

ndvi.1<-raster(ndvi); plot(ndvi.1)
evi.1<-raster(evi); plot(evi.1)
lai.1<-raster(lai); plot(lai.1)
veg_ht.1<-raster(veg_ht); plot(veg_ht.1)
fr_veg.1<-raster(fr_veg); plot(fr_veg.1)
ndvi.ann.1<-raster(ndvi); plot(ndvi.ann.1)
  

################### Extract values ##########################
## Identify all ESRI raster files (.adf files)
allfiles<-paste(c(dem, tpi, cpi, albedo, 
                  ndvi, evi, lai, veg_ht, fr_veg,
                  clouds, winds, staterf), 
                "w001001.adf", sep="/")

xval<-list()
for (i in 1:length(allfiles)){
  out<-raster(allfiles[i])
  xval[[i]]<-extract(out , coordSK)  #extract values & return covariates
  print(paste(i, proj4string(out)))
}

type<-function(x) {x[length(x)-1]}
names(xval)<-lapply(strsplit(allfiles, split="/"), type) 

xval.df<-as.data.frame(xval)

covariates<-data.frame(stationkey, xval.df)
covariates<-covariates[,-grep("optional",names(covariates))]

covariates$wind_sp_ms<-rowMeans(covariates[, grep(pattern="wind_sd",names(covariates))])

write.csv(covariates, "Analyses/SpatialCovar.csv", row.names=F)

