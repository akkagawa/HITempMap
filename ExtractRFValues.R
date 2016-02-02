# ExtractRFValues.R
# Date: 28 January 2016 (AKV), revised 2 Feb 2016
# Notes: Extract mean annual rainfall
# script based on ExtractMeanTempValues.r (Ryan Longman)

library('raster')
library('rgdal')

#Set Working Directory
setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping/GISfiles/RyanL")

#Read in mean temperature files with multiple stations
Tair_Mean <- read.csv(file = "Tair_Mean.csv",header = TRUE)
Tmax_Mean <- read.csv(file = "Tmax_Mean.csv",header = TRUE)
Tmin_Mean <- read.csv(file = "Tmin_Mean.csv",header = TRUE)

#Set spatial coordinates to create a Spatial object:
coordinates(Tair_Mean) = c("lon","lat")  # define x & y as longitude and latitude
coordinates(Tmax_Mean) = c("lon","lat")  # define x & y as longitude and latitude
coordinates(Tmin_Mean) = c("lon","lat")  # define x & y as longitude and latitude

Coord.air <- coordinates(Tair_Mean)
Coord.max <- coordinates(Tmax_Mean)
Coord.min <- coordinates(Tmin_Mean)

#### Extract values from staterf_mmann raster
##############Identify all rasters (.adf files)
RF_ann.fname<- dir("../StateRFGrids_mm_new/staterf_mmann", pattern="w001001.adf")  # same as list.files()

RF_ann<-raster(paste("../StateRFGrids_mm_new/staterf_mmann", RF_ann.fname, sep="/"))

RF_ann.air <- extract(RF_ann , Coord.air) #returns vector of mean annual rainfall
RF_ann.max <- extract(RF_ann , Coord.max) #returns vector of mean annual rainfall
RF_ann.min <- extract(RF_ann , Coord.min) #returns vector of mean annual rainfall

Tair_Mean<-data.frame(Tair_Mean, RFmm=RF_ann.air)[,-18] #remove column 18 ("optional")
Tmax_Mean<-data.frame(Tmax_Mean, RFmm=RF_ann.max)[,-18]
Tmin_Mean<-data.frame(Tmin_Mean, RFmm=RF_ann.min)[,-18]

######## Need to compare to ArcGIS output since I'm not sure I'm extracting values correctly

write.csv(Tair_Mean, file="Tair_MeanRF.csv", row.names=F)
write.csv(Tmax_Mean, file="Tmax_MeanRF.csv", row.names=F)
write.csv(Tmin_Mean, file="Tmin_MeanRF.csv", row.names=F)
