# HITempMap
These scripts are associated with the Hawaii temperature mapping and modeling effort by A. Kagawa-Viviani:

## DatasetGeneration_May2018.R; re-run 5/3/2018
Notes: Full Script for Temperature Modeling and Mapping, from NCDC download to organized files of daily data for aggregation to monthly, yearly.

## HN_processing.R
Notes: Script for downloading and aggregating HaleNet data

## MLO_processing.R
Notes: Script for downloading and aggregating Mauna Loa Observatory data

## StationKey_processing3.R
Notes: Script for organizing station metadata and generating a final station key (req's DatasetGeneration...)

## Aggregation_and_QAQC.R
## Author: Aurora Kagawa-Viviani
## Date: April 17-18 2016; re-run 6/17/2017
## Match station locations with gridded correlates
## Subset appropriately for analysis: by month-years or years
##  Drawn from NCDCplusHaleNet3.R, SubsettingMonthYears1.R

### not used?
## Spatial_StationViz.R
## Notes: Script for visualizing station locations and spatial patterns


---------------- Spatial Covariates
## ExtractSpatialCovariates_May2018.R (from ExtractRFValues.R)
## Project: HI Temp
## Author: A. Kagawa-Viviani
## Date: 25 April 2016, revised 6/16/2017
## Notes: Extract covariates based on layers of spatial data
## script based on ExtractMeanTempValues.r (Ryan Longman)


## AnalysesSpatial_20180511b.R   * maybe should be flow accumulation or Beven Topmodel TPI instead of this other TPI?
## Analysis 3: Spatial Patterns
## Temperature variables vs elevation, TPI, CI, albedo, LAI, MAP, Cloud freq, wind speed
# Take station month year data and station annual data
# Filter 30 year base period??
# Take only complete years (so as to not bias dataset)
# Analyze station mean annual AND mean month for (xx-xx base period)
# Make scatterplot matrix of station mean annual Tmax, Tmin, Tavg, and DTR versus all covariates


## AnalysesValidation_20180516.R
## 6/17/2017
## A. Kagawa-Viviani
## Notes: this script performs model fitting and evaluation:
##    1) mean annual station values
##        a) simple linear regression on elevation for Tmax, Tmin, Tavg
##        b) multiple linear regression using other predictor variables
##            for Tmax, Tmin, Tavg, and DTR, using variables selected through AIC
##    2) mean monthly station values
##        a) regression on elevation
##        b) multiple linear regression using other predictor variables
##            for Tmax, Tmin, Tavg, and DTR, using variables selected through AIC
## make annual map of Tmax, Tmin, Tavg, DTR
## use jacknifing to calculate MAE, RMSE for mean annual predictions

AnalysesMonthlyValidation_20180515.R
## fit monthly models
## re-predict and make maps  (incorporates elements of AnalysesMapping.R files)  
## 

----------------- Time Series Covariates

## AnalysesTemporal_p1_20180517.R (formerly AnalysesTemporal_p1_20180220.R, AnalysesJul2017Temporal.RAnalysesJun2017Temporal.R, AnalysesApr2016v4, Analyses.R)
## Author: Aurora Kagawa-Viviani
## Date: Feb 23, 2018
## Notes: Script for running analyses on time series
## Feb 2018 notes: removed Midway (-999 elevation)
## 7/20/2017 notes: created new script for interannual/interdecadal variability, revised for cloud/wind 7/20/2017
## 6/23/2017 notes: added TWI criteria, simplified Fig 1 (stations)
## Apr 18-20 2016 notes: major overhaul
## DESCRIPTION: This script addresses only station coverage, ***TRENDS
## Generates plot of stations (Figure 1)

####### not necessarily useful
## TrendMethodComparison.R
## Author: Aurora Kagawa-Viviani
## Date: Feb 5 2018
## Notes: Script for calculating trends with annual time series

############# for Time series comparison with climate variables
# TS_ParseNCDCforCloudWindMay2018.R
# Project: HI Temp
# Authos: A. Kagawa-Viviani
# Date: 6/14/2017- 6/15
# Purpose: to look for cloud cover data, wind speed
# modified from redownload.R
# Note redownload.R->DatasetGeneration_Apr2016.R
# Script to download data from NCDC server

## Indices_processing1Sep2017.R
## Author: Aurora Kagawa-Viviani
## Date: Apr 19 2016, updated 7/20/2017, 9/2017
## Notes: Script to organize various climate indices

## AnalysesTemporal_p2_20180521
## AnalysesSep212017Temporal_p2.R   *** NATURAL VARIABILITY
## AnalysesSep192017Temporal.R (formerly AnalysesJul2017Temporal.R, AnalysesJun2017Temporal.R, AnalysesApr2016v4, Analyses.R)
## Author: Aurora Kagawa-Viviani
## Date: Apr 18-20 2016, modified and rerun 6/23/2017, 
##   revised for cloud/wind 7/20/2017, finalizing ts analyses 9/2017
## Notes: Script for running analyses on time series
