# HITempMap
These scripts are associated with the Hawaii temperature mapping and modeling effort by A. Kagawa-Viviani:

# Station Dataset
## DatasetGeneration_May2018.R
- Last run: 5/3/2018
- Notes: Full Script for Temperature Modeling and Mapping, from NCDC download to organized files of daily data for aggregation to monthly, yearly.

## HN_processing.R
- Last run: 5/3/2018
- Notes: Script for aggregating hourly HaleNet data and calculating daily, monthly, and annual statistics

## MLO_processing2019.R
- Last run: 5/21/2019
- Notes: Script for downloading and aggregating hourly Mauna Loa Observatory data and calculating daily, monthly, and annual statistics

## StationKey_processing3.R
- Last run: 5/6/2018
- Notes: Script for organizing station metadata and generating a final station key (req's DatasetGeneration...)

## Aggregation_and_QAQC.R
- Last run: 5/6/2018
1. Match all station locations with gridded correlates
2. Subset appropriately for analysis: by month-years or years

# Spatial Covariates
## ExtractSpatialCovariates_May2018.R (from ExtractRFValues.R)
- Last run: 5/6/2018
- Notes: Extract covariates based on layers of spatial data

## AnalysesSpatial_20180525.R  
- Last run: 5/25/2018
- Notes: Analysis 3: Spatial Patterns
- Temperature variables vs elevation, TPI*, CI, albedo, LAI, MAP, Cloud freq, wind speed
- *TPI* maybe should be flow accumulation or Beven Topmodel TPI instead of this other TPI?
- Take station month year data and station annual data: complete years (so as to not bias dataset)
- Analyze station mean annual AND mean month for (xx-xx base period)
- Make scatterplot matrix of station mean annual Tmax, Tmin, Tavg, and DTR versus all covariate

## AnalysesValidation_20180629.R
- Last run: 06/29/2018
- Notes: this script performs model fitting and evaluation:
1. mean annual station values
  - simple linear regression on elevation for Tmax, Tmin, Tavg
  - multiple linear regression using other predictor variables for Tmax, Tmin, Tavg, and DTR, using variables selected through AIC
2. mean monthly station values
  - regression on elevation
  - multiple linear regression using other predictor variables for Tmax, Tmin, Tavg, and DTR, using variables selected through AIC 
3. make annual map of Tmax, Tmin, Tavg, DTR
4. use jacknifing to calculate MAE, RMSE for mean annual predictions
5. produces FIGURE 2: Validation Stats of best models

## AnalysesMonthlyValidation_20180515.R
- Notes: fit monthly models
- re-predict and make maps  (incorporates elements of AnalysesMapping.R files)  

# Time Series Covariates
## AnalysesTemporal_p1_20180630.R
- Notes: Script for running analyses on time series
- Feb 2018 notes: removed Midway (-999 elevation)
- 7/20/2017 notes: created new script for interannual/interdecadal variability, revised for cloud/wind 7/20/2017
- 6/23/2017 notes: added TWI criteria, simplified Fig 1 (stations)
- Apr 18-20 2016 notes: major overhaul
- DESCRIPTION: This script addresses only station coverage, ***TRENDS
- produces FIGURE 1: plot of stations
- produces FIGURE 3: plots of annual trends, monthly for 1905-2017

## TrendMethodComparison.R
- Notes: Ancillary script for calculating trends with annual time series using different methods

# Comparing temperature time series comparison with climate variables
## TS_ParseNCDCforCloudWindMay2018.R
- Date: 6/14/2017- 6/15
- Purpose: to look for cloud cover data, wind speed
- script modified from redownload.R to examine airport cloud cover and wind speed info from NCDC server

## Indices_processing1Sep2017.R
- Notes: Script to organize various climate indices

## AnalysesTemporal_p2_20180521.R
- NATURAL VARIABILITY
- revised for cloud/wind 7/20/2017, finalizing ts analyses 9/2017
- Notes: Script for running analyses on time series
- produces FIGURE 4: Fig4_InterannualVar_SST_RFI.pdf
- produces FIGURE 5: 

# Contextualizing lapse rates: comparing with soundings and ceilometer
## ASOS_cloud2019.R
- Pull ASOS records, METAR format
- Parse for ceilometer info and export

## Sonde_UWyo.R
- Pull Uwyo records
- Parse and split into profile and calculated indices

## Sonde_UWyo_profileTRENDS20190730.R  (Temp, RH, Mixing ratio)
- Read in UWyo sounding profile information
- Combine and export monthly time series by layers, 400m slices

## LapseRateFigure_20191114.R
- Read in surface, ceilometer, and sounding data from 1978-2017
