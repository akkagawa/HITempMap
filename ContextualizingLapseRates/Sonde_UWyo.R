## Project: HI Temp Map
## Script: Sonde_UWyo.R
## Date: 5/22/2019
## Author: A. Kagawa-Viviani
## Notes: this script pulls and analyzes U Wyo soundings to compare to surface lapse rates:
##    1) download files
##    2) extract metrics, export as csv aggregated monthly time series
##    3) visualize Script to compare sonde and station-derived temperatures to better understand 0-1600m lapse rates
##################################################################

setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping/NOT_USED/AKV_Sonde/UWyo")

## Specify station ()
years<-1973:2018
months<-sprintf("%02d", 1:12)
days<-sprintf("%02d", 1:31)
hour<- c("00",  "12")
## 00 UTC = 2pm HST PREVIOUS day
## 12 UTC = 2am HST

monthd <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
leapmonthd <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

##################################################################
station<-91285  # Hilo

for (i in 1:length(years)){
  if (years[i] %% 4 !=0){lmonth<-monthd}else{lmonth<-leapmonthd}
  for (j in 1:length(months)){
    reqdata<-paste0("http://weather.uwyo.edu/cgi-bin/sounding?region=naconf",
                    "&TYPE=TEXT%3ALIST",
                    "&STNM=", station,
                    "&YEAR=", years[i], 
                    "&MONTH=", months[j],
                    "&FROM=", "01", "00",  #ddhh (UTC so either 00 or 12)
                    "&TO=", lmonth[j], "12")
    files<-paste("sonde", station, years[i], months[j], sep="_")
    if (i==1 & j==1){
      Monthly.HILO<-reqdata
      Monthly.HILO.files<-files
    }else{
      Monthly.HILO<-c(Monthly.HILO, reqdata)
      Monthly.HILO.files<-c(Monthly.HILO.files, files)}
    
    print(paste(years[i], months[j]))}
}
for (i in 271:length(Monthly.HILO)){
  download.file(Monthly.HILO[i], 
                destfile=paste0("Hilo/", Monthly.HILO.files[i], ".txt"))
}


##################################################################
station<-91165  # Lihue

for (i in 1:length(years)){
  if (years[i] %% 4 !=0){lmonth<-monthd}else{lmonth<-leapmonthd}
  for (j in 1:length(months)){
    reqdata<-paste0("http://weather.uwyo.edu/cgi-bin/sounding?region=naconf",
                    "&TYPE=TEXT%3ALIST",
                    "&STNM=", station,
                    "&YEAR=", years[i], 
                    "&MONTH=", months[j],
                    "&FROM=", "01", "00",  #ddhh (UTC so either 00 or 12)
                    "&TO=", lmonth[j], "12")
    files<-paste("sonde", station, years[i], months[j], sep="_")
    if (i==1 & j==1){
      Monthly.LIH<-reqdata
      Monthly.LIH.files<-files
    }else{
      Monthly.LIH<-c(Monthly.LIH, reqdata)
      Monthly.LIH.files<-c(Monthly.LIH.files, files)}
    
    print(paste(years[i], months[j]))}
}
for (i in 1:length(Monthly.LIH)){
  download.file(Monthly.LIH[i], 
                destfile=paste0("Lihue/", Monthly.LIH.files[i], ".txt"))
}


####################################################################
# ## Extract calculated metrics
# sdata<-list.files("Hilo/")
# 
# daily.list<-vector("list")
# for (f in 1:length(sdata)){
#   dat<-readLines(paste0("Hilo/", sdata[f]))
#   # Extract calculated metrics
#   st<-grep("Station identifier", dat)
#   nd<-grep("Precipitable water", dat)
#   
#   if(length(st)>0 & length(nd)>0){
#   daily<-trimws(dat[st:nd])
#   val<-matrix(unlist(strsplit(daily, ": ")), byrow=T, ncol=2)
#   val.df<-data.frame(t(val[,2]))
#   names(val.df)<-val[,1]
#   daily.list[[f]]<-val.df
#   print(f)
#   }else{print("skip")}
# 
# }


####################################################################
# Extract sonde profile and calculated metrics

Ldata<-list.files("Lihue/")
bymonth<-list()
indices.master<-list()

chunks<-matrix(c(1, 100, 101, 200, 201, 300, 301, 400, 401, 500, 501, length(Ldata)), byrow=T, ncol=2)

for (x in 1:nrow(chunks)){
  for (f in chunks[x,1]:chunks[x,2]){
    
    dat<-readLines(paste0("Lihue/", Ldata[f]))
    
    # Identify sections with these fields to bracket
    st<-grep("<H2>91165 PHLI Lihue Observations", dat)
    nd<-grep("</PRE>", dat)
    nd<-nd[seq(from=2, to= length(nd), by=2)]
    
    ## SUBSET 00Z and 12Z launches
    for (j in 1:length(st)){
      bymonth[[j]]<-dat[st[j]:nd[j]]
      
      ## SUBSET the profile/launch stats
      # --------------------------
      # Variable  Units   Columns   
      # --------------------------
      # PRES      hPa        1-8    
      # HGHT       m        9-15   
      # TEMP       C       16-22   
      # DWPT       C       23-29   
      # RELH       %       30-36   
      # MIXR      g/kg     37-43   
      # DRCT      deg      44-50 
      # SKNT      knot     51-57   
      # THTA       K       58-64   
      # THTE       K       65-71
      # THTV       K       72-78
      # ------------------------------
      
      # Identify lines to slice by: profile header (hdr), stats header (sthdr) and tail (sttail)
      hdr<-grep("PRES", bymonth[[j]])
      sthdr<-grep("sounding indices", bymonth[[j]])
      sttail<-length(bymonth[[j]])-1
      
      # # Extract the profile data
      # slaunch<-bymonth[[j]][(hdr+3):(sthdr-1)]  
      # pres<-as.numeric(substr(slaunch, 1,8))
      # hght<-as.numeric(substr(slaunch, 9,15))
      # temp<-as.numeric(substr(slaunch, 16,22))
      # dwpt<-as.numeric(substr(slaunch, 23,29))
      # relh<-as.numeric(substr(slaunch, 30,36))
      # mixr<-as.numeric(substr(slaunch, 37,43))
      # drct<-as.numeric(substr(slaunch, 44,50))
      # sknt<-as.numeric(substr(slaunch, 51,57))
      # thta<-as.numeric(substr(slaunch, 58,64))
      # thte<-as.numeric(substr(slaunch, 65,71))
      # thtv<-as.numeric(substr(slaunch, 72,78))
      # 
      # # Keep track of station, launch time and date (UTC)
      # stn<-unlist(strsplit(bymonth[[j]][1], " "))[2]
      # tm.utc<-unlist(strsplit(bymonth[[j]][1], " "))[6]
      # date.utc<-unlist(strsplit(bymonth[[j]][1], " "))[7:9]
      # date.UTC<-as.Date(substr(paste(date.utc, collapse=" "), 1, 11),
      #                   format="%d %b %Y")
      # 
      # # Put all the profile data in one dataframe
      # sl.df<-data.frame(stn, tm.utc, date.UTC,
      #                   pres, hght, temp, dwpt, relh, mixr, drct, sknt, 
      #                   thta, thte, thtv)
      # 
      # # Combine multiple launches into one big dataframe (for a given month)
      # if(f==chunks[x,1] & j==1){master.sl<-sl.df}else{master.sl<-rbind(master.sl, sl.df)}
      # 
      # Now Process the sonde statistics
      sindices<-bymonth[[j]][(sthdr+1):sttail]
      
      sindices<-trimws(sindices)
      val<-matrix(unlist(strsplit(sindices, ": ")), byrow=T, ncol=2)
      val.df<-data.frame(t(val[,2]))
      names(val.df)<-val[,1]
      
      # Combine these into a single LIST (for a given month); different sizes
      if(f==chunks[x,1] & j==1){
        indices.list<-list()
        n<-1}
      indices.list[[n]]<-val.df
      n<-n+1
    }
    print(Ldata[f])
    
  }
  #write.csv(master.sl, paste0("Lihue/allsonde", x, ".csv"), row.names = F)
  indices.master[[x]]<-indices.list
}

####################################################################
## Extract calculated metrics
names(indices.list[[j]])
# See http://weather.uwyo.edu/upperair/indices.html
# ############################
fields<-c(
  "Station identifier",
  "Station number",
  "Observation time",
  "Station latitude",
  "Station longitude",
  "Station elevation",
  "Showalter index", #(Stability index: decrease-> more convective)
  "Lifted index",    #(Similar to Showalter index)
  "LIFT computed using virtual temperature",
  "SWEAT index",     #(Severe weather threat index)
  "K index",
  "Cross totals index",
  "Vertical totals index",
  "Totals totals index",
  "Convective Available Potential Energy",
  "CAPE using virtual temperature",
  "Convective Inhibition",
  "CINS using virtual temperature",
  "Equilibrum Level",
  "Equilibrum Level using virtual temperature",
  "Level of Free Convection",
  "LFCT using virtual temperature",
  "Bulk Richardson Number",
  "Bulk Richardson Number using CAPV",
  "Temp [K] of the Lifted Condensation Level",
  "Pres [hPa] of the Lifted Condensation Level",
  "Mean mixed layer potential temperature",
  "Mean mixed layer mixing ratio",
  "1000 hPa to 500 hPa thickness",
  "Precipitable water [mm] for entire sounding")

##################### merge the indices
dat.indices<-list()
for (i in 1:length(indices.master)){
  dat.indices[[i]]<-indices.master[[i]][[1]]
  for (j in 2:length(indices.master[[i]])){
    dat.indices[[i]]<-merge(dat.indices[[i]], 
                            indices.master[[i]][[j]], all=T)
    print(paste(i,j))
  }
}

master.dat<-merge(dat.indices[[1]], dat.indices[[2]], all=T)
for (i in 3:length(dat.indices)){
  master.dat<-merge(master.dat, dat.indices[[i]], all=T)}

#master.datT<-master.dat[,-c(31:44)]
dt.utc<-as.POSIXct(master.dat$'Observation time', format="%y%m%d/%H%M", tz="UTC")
master.dat$dt.hst<-format(dt.utc, tz="HST")

write.csv(master.dat, "Lihue_sindices_all.csv", row.names=F)


###################################################################
Hdata<-list.files("Hilo/")
bymonth<-list()
indices.master<-list()

chunks<-matrix(c(1, 100, 101, 200, 201, 300, 301, 400, 401, 500, 501, length(Hdata)), byrow=T, ncol=2)

for (x in 1:nrow(chunks)){
  for (f in chunks[x,1]:chunks[x,2]){
    dat<-readLines(paste0("Hilo/", Hdata[f]))
    
    # Identify sections with these fields to bracket
    st<-grep("<H2>91285 PHTO Hilo Observations", dat)
    nd<-grep("</PRE>", dat)
    nd<-nd[seq(from=2, to= length(nd), by=2)]

    ## SUBSET launches AND calculated indices
    for (j in 1:length(st)){
      bymonth[[j]]<-dat[st[j]:nd[j]]

      ## SUBSET the profile/launch stats

      # Identify lines to slice by: profile header (hdr), stats header (sthdr) and tail (sttail)
      hdr<-grep("PRES", bymonth[[j]])
      sthdr<-grep("sounding indices", bymonth[[j]])
      sttail<-length(bymonth[[j]])-1
      # 
      # # Extract the profile data
      # slaunch<-bymonth[[j]][(hdr+3):(sthdr-1)]  
      # pres<-as.numeric(substr(slaunch, 1,8))
      # hght<-as.numeric(substr(slaunch, 9,15))
      # temp<-as.numeric(substr(slaunch, 16,22))
      # dwpt<-as.numeric(substr(slaunch, 23,29))
      # relh<-as.numeric(substr(slaunch, 30,36))
      # mixr<-as.numeric(substr(slaunch, 37,43))
      # drct<-as.numeric(substr(slaunch, 44,50))
      # sknt<-as.numeric(substr(slaunch, 51,57))
      # thta<-as.numeric(substr(slaunch, 58,64))
      # thte<-as.numeric(substr(slaunch, 65,71))
      # thtv<-as.numeric(substr(slaunch, 72,78))
      # 
      # # Keep track of station, launch time and date (UTC)
      # stn<-unlist(strsplit(bymonth[[j]][1], " "))[2]
      # tm.utc<-unlist(strsplit(bymonth[[j]][1], " "))[6]
      # date.utc<-unlist(strsplit(bymonth[[j]][1], " "))[7:9]
      # date.UTC<-as.Date(substr(paste(date.utc, collapse=" "), 1, 11),
      #                   format="%d %b %Y")
      # 
      # # Put all the profile data in one dataframe
      # sl.df<-data.frame(stn, tm.utc, date.UTC,
      #                   pres, hght, temp, dwpt, relh, mixr, drct, sknt, 
      #                   thta, thte, thtv)
      # 
      # # Combine multiple launches into one big dataframe (for a given month)
      # if(f==chunks[x,1] & j==1){master.sl<-sl.df}else{master.sl<-rbind(master.sl, sl.df)}
      # 
      # 
      #############################################
      # Now Process the sonde statistics
      sindices<-bymonth[[j]][(sthdr+1):sttail]
      
      sindices<-trimws(sindices)
      val<-matrix(unlist(strsplit(sindices, ": ")), byrow=T, ncol=2)
      val.df<-data.frame(t(val[,2]))
      names(val.df)<-val[,1]
      
      # Combine these into a single LIST (for a given month); different sizes
      if(f==chunks[x,1] & j==1){
        indices.list<-list()
        n<-1}
      indices.list[[n]]<-val.df
      n<-n+1
    }
    
    print(Hdata[f])
    
  }
  #write.csv(master.sl, paste0("Hilo/allsonde", x, ".csv"), row.names = F)
  indices.master[[x]]<-indices.list
}

##################### merge the indices
dat.indices<-list()
for (i in 1:length(indices.master)){
  dat.indices[[i]]<-indices.master[[i]][[1]]
  for (j in 2:length(indices.master[[i]])){
    dat.indices[[i]]<-merge(dat.indices[[i]], 
                            indices.master[[i]][[j]], all=T)
    print(paste(i,j))
  }
}

master.dat<-merge(dat.indices[[1]], dat.indices[[2]], all=T)
for (i in 3:length(dat.indices)){
  master.dat<-merge(master.dat, dat.indices[[i]], all=T)}

#master.datT<-master.dat[,-c(31:44)]
dt.utc<-as.POSIXct(master.dat$'Observation time', format="%y%m%d/%H%M", tz="UTC")
master.dat$dt.hst<-format(dt.utc, tz="HST")

write.csv(master.dat, "Hilo_sindices_all.csv", row.names=F)

###################################################################
setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping/NOT_USED/AKV_Sonde/UWyo")

# Visualize indices
library(xts)
library(dygraphs)

Ldat<-read.csv("Lihue_sindices_all.csv", stringsAsFactors = F)
Hdat<-read.csv("Hilo_sindices_all.csv", stringsAsFactors = F)

LHdat<-rbind(Ldat, Hdat)

LHdat$CINS.using.virtual.temperature<-as.numeric(LHdat$CINS.using.virtual.temperature)
LHdat$X1000.hPa.to.500.hPa.thickness<-as.numeric(LHdat$X1000.hPa.to.500.hPa.thickness)
LHdat$Bulk.Richardson.Number <-as.numeric(LHdat$Bulk.Richardson.Number)
LHdat$Bulk.Richardson.Number.using.CAPV<-as.numeric(LHdat$Bulk.Richardson.Number.using.CAPV)
LHdat$Pres..hPa..of.the.Lifted.Condensation.Level<-as.numeric(LHdat$Pres..hPa..of.the.Lifted.Condensation.Level)

LHdat$dt<-as.POSIXct(LHdat$dt.hst)
  LHdat.14<-as.POSIXlt(LHdat$dt)$hour==14
  LHdat.2<-as.POSIXlt(LHdat$dt)$hour==2

lih<-LHdat$Station.number==91165|
  LHdat$Station.identifier=="PHLI"|
  LHdat$Station.identifier=="LIH"

hilo<-LHdat$Station.number==91285|
  LHdat$Station.identifier=="PHTO"|
  LHdat$Station.identifier=="ITO"

LHdat.day.Lih<-LHdat[LHdat.14 & lih,]  # Station ID: PHLI/LIH
LHdat.day.Hilo<-LHdat[LHdat.14 & hilo,] # Station ID: PHTO/ITO

LHdat.nt.Lih<-LHdat[LHdat.2 & lih,]
LHdat.nt.Hilo<-LHdat[LHdat.2 & hilo,]

nidx<-c(1:6, 31:32)  # ignore certain columns

Ldat.day.xts<-xts(LHdat.day.Lih[,-nidx], order.by=LHdat.day.Lih$dt)
Ldat.nt.xts<-xts(LHdat.nt.Lih[,-nidx], order.by=LHdat.nt.Lih$dt)

Hdat.day.xts<-xts(LHdat.day.Hilo[,-nidx], order.by=LHdat.day.Hilo$dt)
Hdat.nt.xts<-xts(LHdat.nt.Hilo[,-nidx], order.by=LHdat.nt.Hilo$dt)

numNA<-function(X) {length(which(is.na(X)))}

for (i in 1:24){
  Ld<-apply.monthly(Ldat.day.xts[,i], median, na.rm=T)
  Ln<-apply.monthly(Ldat.nt.xts[,i], median, na.rm=T)
  Hd<-apply.monthly(Hdat.day.xts[,i], median, na.rm=T)
  Hn<-apply.monthly(Hdat.nt.xts[,i], median, na.rm=T)
  
  Ld.n<-apply.monthly(Ldat.day.xts[,i], length)-
    apply.monthly(Ldat.day.xts[,i], numNA)
  Ln.n<-apply.monthly(Ldat.nt.xts[,i], length)-
    apply.monthly(Ldat.nt.xts[,i], numNA)
  Hd.n<-apply.monthly(Hdat.day.xts[,i], length)-
    apply.monthly(Hdat.day.xts[,i], numNA)
  Hn.n<-apply.monthly(Hdat.nt.xts[,i], length)-
    apply.monthly(Hdat.nt.xts[,i], numNA)
  
  
  if(i==1){
    L.mo.medians.day<-Ld
    L.mo.medians.night<-Ln
    H.mo.medians.day<-Hd
    H.mo.medians.night<-Hn
    
    Ld.obs<-Ld.n 
    Ln.obs<-Ln.n 
    Hd.obs<-Hd.n 
    Hn.obs<-Hn.n 
    names(Ld.obs)<-names(Ln.obs)<-names(Hd.obs)<-names(Hn.obs)<-"Observations"

  }else{
    L.mo.medians.day<-merge(L.mo.medians.day, Ld)
    L.mo.medians.night<-merge(L.mo.medians.night, Ln)
    H.mo.medians.day<-merge(H.mo.medians.day, Hd)
    H.mo.medians.night<-merge(H.mo.medians.night, Hn) 
    
    Ld.obs<-merge(Ld.obs, Ld.n)
    Ln.obs<-merge(Ln.obs, Ln.n) 
    Hd.obs<-merge(Hd.obs, Hd.n)
    Hn.obs<-merge(Hn.obs, Hn.n) 
    
    }
}

## Flag months with missing data
rm.LD<-Ld.obs<20
rm.LN<-Ln.obs<20
dygraph(Ld.obs, main="Lihue Daytime")
dygraph(Ln.obs, main="Lihue Night")

L.mo.medians.day_cln<-L.mo.medians.day
L.mo.medians.day_cln[rm.LD]<-NA
L.mo.medians.night_cln<-L.mo.medians.night
L.mo.medians.night_cln[rm.LN]<-NA

rm.HD<-Hd.obs<20
rm.HN<-Hn.obs<20
dygraph(Hd.obs, main="Hilo Daytime")
dygraph(Hn.obs, main="Hilo Night")

H.mo.medians.day_cln<-H.mo.medians.day
H.mo.medians.day_cln[rm.HD]<-NA
H.mo.medians.night_cln<-H.mo.medians.night
H.mo.medians.night_cln[rm.HN]<-NA

# # Merge data with number of soundings (to screen out months with no data)
# L.mo.medians.day<-merge(Ld.obs, L.mo.medians.day)
# L.mo.medians.night<-merge(Ln.obs, L.mo.medians.night)
# H.mo.medians.day<-merge(Hd.obs, H.mo.medians.day)
# H.mo.medians.night<-merge(Hn.obs, H.mo.medians.night)

# Merge day, night soundings for each station:
# Lihue
index(L.mo.medians.day_cln)<-as.Date(paste0(substr(index(L.mo.medians.day_cln), 1, 7),"-01"))
index(L.mo.medians.night_cln)<-as.Date(paste0(substr(index(L.mo.medians.night_cln), 1, 7),"-01"))
L.monthly.all<-merge(L.mo.medians.day_cln, L.mo.medians.night_cln)
write.csv(as.data.frame(L.monthly.all), "Lihue_monthly_medians.csv")

# Hilo
index(H.mo.medians.day_cln)<-as.Date(paste0(substr(index(H.mo.medians.day_cln), 1, 7),"-01"))
index(H.mo.medians.night_cln)<-as.Date(paste0(substr(index(H.mo.medians.night_cln), 1, 7),"-01"))
H.monthly.all<-merge(H.mo.medians.day_cln, H.mo.medians.night_cln)
write.csv(as.data.frame(H.monthly.all), "Hilo_monthly_medians.csv")

# pdf("MonthlyMedians_Lihue.pdf")
# for (i in 1:25){
#   print(plot.xts(L.monthly.all[,c(i, i+25)], 
#                  main=paste("Lihue:", names(L.monthly.all)[i]),
#                  type="p", col=c("red", "blue"), 
#                  ylim=quantile(L.monthly.all[,c(i, i+25)], c(0.01, 0.99), na.rm=T)))  
# }
# dev.off() 
# 
# pdf("MonthlyMedians_Hilo.pdf")
# for (i in 1:25){
#   print(plot.xts(H.monthly.all[,c(i, i+25)], 
#                  main=paste("Hilo:", names(H.monthly.all)[i]),
#                  type="p", col=c("red", "blue"), 
#                  ylim=quantile(H.monthly.all[,c(i, i+25)], c(0.01, 0.99), na.rm=T)))  
# }
# dev.off() 


for (i in 1:48){
  L<-apply.yearly(L.monthly.all[,i], mean)
  H<-apply.yearly(H.monthly.all[,i], mean)
  
  if(i==1){
    L.yearly.all<-L
    H.yearly.all<-H
    
  }else{
    L.yearly.all<-merge(L.yearly.all, L)
    H.yearly.all<-merge(H.yearly.all, H)
  }}
write.csv(as.data.frame(L.yearly.all), "Lihue_annual.csv")
write.csv(as.data.frame(H.yearly.all), "Hilo_annual.csv")

# pdf("YearlyMean_Lihue.pdf")
# for (i in 1:24){
#   print(plot.xts(L.yearly.all[,c(i, i+24)], 
#                  main=paste("Lihue:", names(L.yearly.all)[i]),
#                  type="l", col=c("red", "blue")))  
# }
# dev.off() 
# 
# pdf("YearlyMean_Hilo.pdf")
# for (i in 1:24){
#   print(plot.xts(H.yearly.all[,c(i, i+24)], 
#                  main=paste("Hilo:", names(H.yearly.all)[i]),
#                  type="l", col=c("red", "blue")))  
# }
# dev.off() 

allLH<-merge(L.yearly.all, H.yearly.all, all=T)

pdf("YearlyMean_LH_scr.pdf")
for (i in 1:24){
  a<-plot.xts(allLH[,c(i, i+24, i+48, i+72)], 
                 main=names(H.yearly.all)[i],
                 type="l", col=c("red", "blue", "red", "blue"),
                 lty=c(2,2,1,1))
  addLegend("bottomleft", on=0, ncol=2,
            legend.names = c("Hilo day", "Hilo night", 
                             "Lihue day", "Lihue night"), 
            lty=c(1,1,2,2), lwd=2, col=c("red", "blue", "red", "blue"))
print(a)
}
dev.off()


# bring in surface lapse rate information 
## Calculate annual series and compare to station-derived data
# Compare to fitted MONTHLY coefficients
coefsy<-read.csv("../../../annual_lm_2017.csv", stringsAsFactors = F)
coefsy.xts<-xts(coefsy[,-c(1:2)], 
                order.by=as.Date(paste0(coefsy$coefs_Year,"-12-01")))
mrg<-merge(coefsy.xts[,-c(7:9, 13)], allLH, all=F)

# plot relationship of Tmax lapse rate with HILO sounding indices
for (i in 58:81){
  plot(as.numeric(mrg$coefs_Tmax.m)~as.numeric(mrg[,i]),
       main=names(mrg)[i])
  mod<-lm(as.numeric(mrg$coefs_Tmax.m)~as.numeric(mrg[,i]))
  abline(mod)
  par(mfrow=c(2,2))
  plot(mod)
  par(mfrow=c(1,1))
}

for (i in 82:105){
  plot(as.numeric(mrg$coefs_Tmin.m)~as.numeric(mrg[,i]),
       main=names(mrg)[i])
}

mrg<-mrg[mrg$R2_Tavg>0.8,]
pdf("Sondes_vs_TavgLapse.pdf")
for (i in 10:33){
  plot(as.numeric(mrg[,i])~as.numeric(mrg$coefs_Tavg.m),  # Lihue daytime
       main=names(mrg)[i], col="red", 
       ylim=range(as.numeric(mrg[,i]), as.numeric(mrg[,i+24]),
                  as.numeric(mrg[,i+48]), as.numeric(mrg[,i+72]), na.rm=T)*c(0.95,1))
  points(as.numeric(mrg[,i+24])~as.numeric(mrg$coefs_Tavg.m), # Lihue night
         col="blue")
  points(as.numeric(mrg[,i+48])~as.numeric(mrg$coefs_Tavg.m), # Hilo night
         col="red", pch=19)
  points(as.numeric(mrg[,i+72])~as.numeric(mrg$coefs_Tavg.m), # Hilo night
         col="blue", pch=19)
  legend("bottom",pch=c(19,19,1,1), horiz=T, bty="o",
         col=c("red", "blue", "red", "blue"),
         legend = c("Hilo day", "Hilo night", 
                          "Lihue day", "Lihue night"))
}
daytime<-data.frame(Tmax0=mrg$coefs_Tmax.b,
                    TmaxdTdz<-mrg$coefs_Tmax.m,
                    mrg[, 58:81])
plot(daytime)

##############
t1958<-coefsy[coefsy$coefs_Year>=1958,]
lm.tavgm<-lm(coefs_Tavg.m~coefs_Year, data=t1958[t1958$R2_Tavg>0.80,])
library(segmented)

# Call: segmented.lm(obj = lm.tmaxm)
# 
# Meaningful coefficients of the linear terms:
#   (Intercept)     coefs_Year  U1.coefs_Year  
# -9.362e-03      1.452e-06     -4.086e-05  
# 
# Estimated Break-Point(s):
#   psi1.coefs_Year  
# 1993  

segmented(lm.tavgm)->sg
plot(sg)
 
plot(coefs_Tavg.m~R2_Tavg, data=t1958[t1958$R2_Tavg>0.80,])
summary(lm(coefs_Tavg.m~R2_Tavg, data=t1958))
summary(lm(coefs_Tavg.m~R2_Tavg, data=t1958[t1958$R2_Tmax>0.80,]))

test<-lm(coefs_Tmax.m~coefs_Year, data=tmax1958[tmax1958$R2_Tmax>0.80,])
