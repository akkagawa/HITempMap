# TempModeling.R
# Date: 28 January 2016 (AKV), revised 2/2/2016 (AKV)
# Notes: Model temperature on elevation and rainfall; 
#        return coefficients and diagnostics

setwd("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping/GISfiles/RyanL")

#Read in mean temperature files with multiple stations
Tair <- read.csv(file = "Tair_MeanRF.csv",header = TRUE)
Tmax <- read.csv(file = "Tmax_MeanRF.csv",header = TRUE)
Tmin <- read.csv(file = "Tmin_MeanRF.csv",header = TRUE)

# for kicks:
plot(Tair$APR~Tair$elev, ylim=c(-3,38), main="APR")
points(Tmax$APR~Tair$elev, col="red")
points(Tmin$APR~Tair$elev, col="blue")

# REVISION (2/2/2016) Remove HKAP and USC00512679; same structure for all
bad<-c(grep("HKAP",Tair$Sta_ID),grep("USC00512679",Tair$Sta_ID))
Tair<-Tair[-bad,]
Tmax<-Tmax[-bad,]
Tmin<-Tmin[-bad,]
################################################
# Multiple linear regression method: elevation and RF

mo.Tair<-Tair[,6:17]
mo.Tmax<-Tmax[,6:17]
mo.Tmin<-Tmin[,6:17]

lm.Tair<-list()
lm.Tmax<-list()
lm.Tmin<-list()

for (i in 1:12){
  lm.Tair[[i]]<-summary(lm(mo.Tair[,i]~Tair$elev*Tair$RFmm))
  lm.Tmax[[i]]<-summary(lm(mo.Tmax[,i]~Tmax$elev*Tmax$RFmm))
  lm.Tmin[[i]]<-summary(lm(mo.Tmin[,i]~Tmin$elev*Tmin$RFmm))
}

################################################
# Testing sensitivity of R2 to those high elevation station networks:
# dropping LHN
#iLHN<-26:32
#Tair<-Tair[-iLHN,]
#Tmax<-Tmax[-iLHN,]
#Tmin<-Tmin[-iLHN,]

# AND/or dropping CraterNet
#iCrater<-1:6
#Tair<-Tair[-iCrater,]
#Tmax<-Tmax[-iCrater,]
#Tmin<-Tmin[-iCrater,]

################################################
# Segmented regression section

source("C:/Users/Aurora/OneDrive/Documents/Projects/TempMapping/pwrtemp2.R")
z1<-0
z2<-4200
z3<-2150  #z3 is trade wind inversion elevation, 2150 meters


### modify section below!
head(Tair)
dat<-list(Tair=Tair, Tmax=Tmax, Tmin=Tmin)

t<-data.frame(matrix(data=NA, nrow=dim(Tair)[1], ncol=dim(Tair)[2])) # create empty matrix for predicted values, residuals, diagnostics
names(t)<-names(Tair)  #assumes format same for all

preds<-list(Tair=t, Tmax=t, Tmin=t)
resids<-list(Tair=t, Tmax=t, Tmin=t)

for (i in 1:3){ # Tmax, Tmin, Tavg
  sub<-dat[[i]]
  preds[[i]][,c(1:5,18)]<-sub[,c(1:5,18)]
  resids[[i]][,c(1:5,18)]<-sub[,c(1:5,18)]
  
  for (m in 6:17){
    nas<-which(is.na(sub[,m]))
    if(length(nas)>0){
      subna<-dat[[i]][-nas,]
      out<-optim(par=c(20,10,3,0), 
                 fn=SStemp, 
                 method="BFGS",
                 elev=subna$elev,
                 rf=subna$RFmm, 
                 t_actual=subna[,m])
    }else{
    out<-optim(par=c(20,10,3,0), 
               fn=SStemp, 
               method="BFGS",
               elev=sub$elev,
               rf=sub$RFmm, 
               t_actual=sub[,m])
    }
    
    rname<-paste(names(dat)[i], names(sub)[m], sep="/")
    obs<-length(which(!is.na(sub[,m]))) # number of observations
    hiobs<-length(which(sub$elev>=2150))
    
    ### calculate and return residuals
    r1<-sub$elev<z3             # separate into below/above inversion at 2150
    Y_a1<-mean(sub[r1,m], na.rm=T)     # average value of low elevation segment
    Y_a2<-mean(sub[!r1,m], na.rm=T)    # average of upper elevation segment
    
    
    # below 2150m:
    preds[[i]][r1,m]<- (out$par[1]*(z3-sub$elev[r1])+   
                          out$par[2]*(sub$elev[r1]-z1))/(z3-z1) +
      out$par[4]*sub$RFmm[r1]       # use rainfall from atlas
    
    # above 2150m:
    preds[[i]][!r1,m]<- (out$par[2]*(z2-sub$elev[!r1])+
                out$par[3]*(sub$elev[!r1]-z3))/(z2-z3) + 
      out$par[4]*sub$RFmm[!r1]
    
    resids[[i]][,m]<-sub[,m]-preds[[i]][,m]                 # matrix of residuals
    
    #out$value is equivalent to sum of squares of residuals (sum(resids^2))
    
    resids_a1<-resids[[i]][r1,m]  # lower elev
    resids_a2<-resids[[i]][!r1,m] # upper elev
    
    R2_a1<-1-(sum(resids_a1^2, na.rm=T)/sum((sub[r1,m]-Y_a1)^2, na.rm=T))  #lower R2 (below 2150m)
    R2_a2<-1-(sum(resids_a2^2, na.rm=T)/sum((sub[!r1,m]-Y_a2)^2, na.rm=T)) #upper R2
    Cd<-1-sum(resids[[i]][,m]^2, na.rm=T)/sum((sub[,m]-mean(sub[,m], na.rm=T))^2, na.rm=T) #coefficient of determination
    fits<-c(model=rname,
            NumObs=obs, 
            HiObs=hiobs,
            unlist(out),
            R2_lo=R2_a1,
            R2_hi=R2_a2,
            Cd=Cd)
    
    
    if (i==1 & m==6){
      finalfits<-fits
    }else{finalfits<-rbind(finalfits, fits)  # rbind successive years
    }
    
  }
}


# Save predicted values/residuals for each year as reference:

for (i in 1:3){
  write.csv(preds[[i]],
            file=paste("Segment_Predicted2_", names(resids)[i],".csv", sep=""),
            row.names=F)
  write.csv(resids[[i]],
            file=paste("Segment_Residuals2_", names(resids)[i],".csv", sep=""),
            row.names=F)
}

finalfits.df<-data.frame(finalfits)  # convert to dataframe
write.csv(finalfits.df, "finalfits2.csv", row.names=F) 
