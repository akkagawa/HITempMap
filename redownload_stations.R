# redownload_stations.R
# Description: script to read in NCDC station metadata and select Hawaii station info
#   this file includes information on station relocations

## see http://www.ncdc.noaa.gov/homr/reports for data below

setwd("C:/Users/Kealohilani/OneDrive/Documents/Projects/TempMapping/QAQC")
stations<-readLines("mshr_enhanced_201506.txt")

stcode<-substr(stations, 779,788)
hi<-which(stcode=='HI        ')
hi.stns<-stations[hi]
  
###### Metadata from MSHR_ENHANCED.TXT
# STATE_PROV            10 	 X(10) 	      779-788       USPS two character alphabetic abbreviation for each state, uppercase.
#
# SOURCE_ID   	        20 	 X(20)        001-020       Unique identifier of source NCEI system.
# SOURCE 			          10 	 X(10) 	      022-031       Name of source NCEI system (MMS, ISIS, GHCND, GHCNM-LT etc).
# BEGIN_DATE 		         8 	 YYYYMMDD     033-040       Beginning date of record, set to 00010101 if date is unknown.
# END_DATE 		           8 	 YYYYMMDD     042-049	    Ending date of record, set to 99991231 if station is currently open.
# STATION_STATUS 		    20 	 X(20) 	      051-070       For the Cooperative Network, displays INACTIVE if station is currently 
#   inactive and not closed. END_DATE would be effective date of inactivation. 
#   For USCRN / USRCRN / AL USRCRN stations, the status will display either 
#   OPERATIONAL, NON-OPERATIONAL, CLOSED or ABANDONED. 
# NCDCSTN_ID 		        20 	 X(20) 	      072-091       Unique identifier used by NCEI.
# GHCND_ID   	          20 	 X(20) 	      240-259       Populated if station is included in GHCN-Daily product.
# NAME_PRINCIPAL   	   100 	 X(100)       261-360	    Name of station, upper case may contain characters, numbers or symbols.
# NAME_PRINCIPAL_SHORT 	30 	 X(30) 	      362-391       Name of station, upper case may contain characters, numbers or symbols.
# ELEV_GROUND   	      40 	 X(40) 	      990-1029      Ground elevation.  For Coop network, average elevation of the ground 
#   in a 20-meter(60ft) circle around the primary rain gauge. For 1st & 2nd Order stations, elevation of the official temperature sensor for the station.
# ELEV_GROUND_UNIT 	    20 	 X(20) 	      1031-1050     Ground elevation unit (should always be FEET).
# LAT_DEC   	          20 	 X(20)        1300-1319     Decimal latitude, blank indicates North and "-" indicates South.
# LON_DEC 		          20 	 X(20)        1321-1340     Decimal longitude, blank indicates East and "-" indicates West.
# LAT_LON_PRECISION 	  10 	 X(10) 	      1342-1351     Indicates precision of source lat and lon, see Reference Table below.
# RELOCATION 		        62 	 X(62) 	      1353-1414     Distance and direction of station relocation expressed as a distance 
#   value (1-4 characters), space, distance units (2 character abbreviation), 
#   space, and direction (1-3 character 16-point cardinal direction). Date of 
#   relocation indicated by begin date of record.
# DATUM_HORIZONTAL      30 	 X(30) 	      1602-1631     Horizontal reference datum used to determine the spatial fix of the station.
#   Only available in newer periods where GPS receivers were used.
# DATUM_VERTICAL 		    30 	 X(30)        1633-1662     Vertical reference datum used to determine the elevation of the station.
#   Only available in newer periods where GPS receivers were used.
# LAT_LON_SOURCE  	   100 	 X(100)       1664-1763     Latitude/longitude data source.  Only available in newer periods where GPS 
#   receivers were used.
source_id<-substr(hi.stns, 1, 20)
source_<-substr(hi.stns, 22, 31)
date_beg<-substr(hi.stns, 33, 40)
date_end<-substr(hi.stns, 42, 49)
status<-substr(hi.stns, 51, 70)
ncdc_id<-substr(hi.stns, 72, 91)
ghcnd_id<-substr(hi.stns, 240, 259)
name_p<-substr(hi.stns, 261, 360)
name_pshort<-substr(hi.stns, 362, 391)
elev<-substr(hi.stns, 990, 1029)
elev_unit<-substr(hi.stns, 1031, 1050)
lat_dec<-substr(hi.stns,1300, 1319)
lon_dec<-substr(hi.stns, 1321, 1340)
precis<-substr(hi.stns, 1342, 1351)
reloc<-substr(hi.stns, 1353, 1414)
datumh<-substr(hi.stns, 1602, 1631)
datumv<-substr(hi.stns, 1633, 1662)

stnfinal<-cbind(source_id, source_,
                date_beg, date_end, status,
                ncdc_id, ghcnd_id, name_p, name_pshort, 
                elev, elev_unit, lat_dec, lon_dec,
                precis, reloc, datumh, datumv)
stnfinal<-data.frame(stnfinal)
write.csv(stnfinal, "ncdcstation.csv", row.names=F)
