# HITempMap
These scripts are associated with the Hawaii temperature mapping and modeling effort by A. Kagawa-Viviani:

# Station Dataset
## [DatasetGeneration_May2018.R](../StationDataset/DatasetGeneration_May2018.R)
- Last run: 5/3/2018
- Notes: Full Script for Temperature Modeling and Mapping, from NCDC download to organized files of daily data for aggregation to monthly, yearly.

## [HN_processing.R](../StationDataset/HN_processing.R)
- Last run: 5/3/2018
- Notes: Script for aggregating hourly HaleNet data and calculating daily, monthly, and annual statistics

## [MLO_processing2019.R](../StationDataset/MLO_processing2019.R)
- Last run: 5/21/2019
- Notes: Script for downloading and aggregating hourly Mauna Loa Observatory data and calculating daily, monthly, and annual statistics

## [StationKey_processing3.R](../StationDataset/StationKey_processing3.R)
- Last run: 5/6/2018
- Notes: Script for organizing station metadata and generating a final station key (req's DatasetGeneration...)

## [Aggregation_and_QAQC.R](../StationDataset/Aggregation_and_QAQC.R)
- Last run: 5/6/2018
- Match all station locations with gridded correlates
- Subset appropriately for analysis: by month-years or years

# Spatial Analysis
## ExtractSpatialCovariates_May2018.R (from ExtractRFValues.R)
- Last run: 5/6/2018
- Notes: Extract covariates based on layers of spatial data

## AnalysesSpatial_Mod_Val_Map_20190612.R
- Last run: 6/21/2019
1. Notes: Spatial patterns, mean annual values
   - Temperature variables vs elevation, TPI*, CI, albedo, LAI, MAP, cloud freq, wind speed
   - Take station month year data and station annual data: complete years (so as to not bias dataset)
   - Analyze station mean annual AND mean month for (2006-2017 period)
   - Make scatterplot matrix of station mean annual Tmax, Tmin, Tavg, and DTR versus all covariates

2. Notes: Model selection and validation
   - Select best model for annual station-years looking at first order effects of all variable combinations
   - Perform k-fold cross validation to calculate MAE, RMSE for mean annual predictions from best models
   - produce FIGURE 2: Validation Stats of best models

3. Notes: Mapping
   - fit annual, monthly, and month year model coefficients based on best models from annual station-years
   - predict and make maps

# Temporal Analysis: trends in sea level air temperature
## AnalysesTemporal_p0_20190613.R
- calculates regression time series from station data, Tz0, dT/dz
- addresses only station coverage 
- produces FIGURE 1: plot of stations

## AnalysesTemporal_p1_20190809.R	
- Calculates nonparametric trends of Tz0, dT/dz
- produces FIGURE 3: plots of annual trends, monthly for 1905-2017, 1958-2017

## TrendMethodComparison.R
- Notes: Ancillary script for calculating trends with annual time series using different methods

# Temporal Analysis: Comparing sea level air temperature and climate variables
## TS_ParseNCDCforCloudWindMay2018.R
- script to examine airport cloud cover and wind speed info from GHCND records

## Indices_processing_20180521.R
- Notes: Script to obtain and organize various climate indices

## AnalysesTemporal_p2_20190809.R
- Compares Tz0 to SST, RFI, WS, and CC, MEI, PDO, IPO, NPGO
- pre-whiten time series and look at CCFs
- filter and plot time series together
- Produce Figure 4: Tz0 and SST, RFT, WS, and CC
- Produce Supporting Info Figure 6: Tz0 and PDO, NPGO, IPO, MEI

# Contextualizing lapse rates (-dT/dz): comparing with soundings and ceilometer
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
