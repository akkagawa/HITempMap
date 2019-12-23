## AnalysesTemporal_p1_20180220.R (formerly AnalysesJul2017Temporal.RAnalysesJun2017Temporal.R, AnalysesApr2016v4, Analyses.R)
## Author: Aurora Kagawa-Viviani
## Date: 6/13/2019
## Notes: Script for running analyses on calculated time series
## DESCRIPTION: This script addresses only station coverage, trends

setwd("xxx/TempMapping")

##################################################################
## Analysis 1: Temporal Trends : 
##  Analyze:
##    - non-parametric trend in Tmax(z=0), Tmin(z=0), DTR(z=0), Tavg(z=0)
##          from 1918-2017, 1968-2017
##  See also TrendMethodComparison.R for comparison with other nls fitting
##################################################################
st.ts<-1905
nd.ts<-2017

###############################################################################
###### A1.1: Analyze trends over different periods ############################
library(Kendall)
library(zyp)  # or library(trend)

coefsy<-read.csv("annual_lm_2017.csv")
rename<-grep("coefs", names(coefsy))  
newname<-unlist(strsplit(names(coefsy)[rename], split="coefs_"))[seq(2,18,by=2)]
names(coefsy)[rename]<-newname

## Convert slope m to lapse rate by changing sign:
# note d(DTR/dz)dt is not very interpretable
coefsy[,grep(".m", names(coefsy), fixed=T)]<-(coefsy[,grep(".m", names(coefsy), fixed=T)])*(-1)

######## z=0 Trends over 1905-2017: ######
## But note I am not propagating my error here..
## T(z=0): b
senmax0.100<-zyp.sen(Tmax.b~Year, coefsy); ci.senmax0.100<-confint(senmax0.100) 
senmin0.100<-zyp.sen(Tmin.b~Year, coefsy); ci.senmin0.100<-confint(senmin0.100)
senavg0.100<-zyp.sen(Tavg.b~Year, coefsy); ci.senavg0.100<-confint(senavg0.100)
sendtr0.100<-zyp.sen(DTR.b~Year, coefsy); ci.sendtr0.100<-confint(sendtr0.100)

## Lapse rates: -m, -dT/dz  (should be opposite in sign)
senmaxlapse.100<-zyp.sen(Tmax.m~Year, coefsy); ci.senmaxlapse.100<-confint(senmaxlapse.100) 
senminlapse.100<-zyp.sen(Tmin.m~Year, coefsy); ci.senminlapse.100<-confint(senminlapse.100)
senavglapse.100<-zyp.sen(Tavg.m~Year, coefsy); ci.senavglapse.100<-confint(senavglapse.100)
#sendtrlapse.100<-zyp.sen(DTR.m~Year, coefsy); ci.sendtrlapse.100<-confint(sendtrlapse.100)

sentrends.100<-list(senmax0.100, senmin0.100, senavg0.100, sendtr0.100,
                    senmaxlapse.100, senminlapse.100, senavglapse.100)
senCI.100<-list(ci.senmax0.100, ci.senmin0.100, ci.senavg0.100, ci.sendtr0.100,
                ci.senmaxlapse.100, ci.senminlapse.100, ci.senavglapse.100)

trend.100<-data.frame(trend=unlist(lapply(sentrends.100, function(x) coef(x)["Year"])),
                     CI=matrix(unlist(lapply(senCI.100, function(x) x["Year",])), 
                               ncol=2, byrow=T),
                     metric=rep(c("T(z=0)", "-dT/dz"),  times=c(4,3) ),
                     start="1905",
                     measure=c("Tmax", "Tmin", "Tavg", "DTR", "Tmax", "Tmin", "Tavg"))
names(trend.100)[2:3]<-c("CI.025", "CI.975")

###### z=0 Trends over 1958-2017: ###### 
coefsy60<-subset(coefsy, coefsy$Year>=1958)
## T(z=0): b
senmax0.60<-zyp.sen(Tmax.b~Year, coefsy60); ci.senmax0.60<-confint(senmax0.60) 
senmin0.60<-zyp.sen(Tmin.b~Year, coefsy60); ci.senmin0.60<-confint(senmin0.60)
senavg0.60<-zyp.sen(Tavg.b~Year, coefsy60); ci.senavg0.60<-confint(senavg0.60)
sendtr0.60<-zyp.sen(DTR.b~Year, coefsy60); ci.sendtr0.60<-confint(sendtr0.60)

## Lapse rates: -m, -dT/dz (should be opposite in sign)
senmaxlapse.60<-zyp.sen(Tmax.m~Year, coefsy60); ci.senmaxlapse.60<-confint(senmaxlapse.60) 
senminlapse.60<-zyp.sen(Tmin.m~Year, coefsy60); ci.senminlapse.60<-confint(senminlapse.60)
senavglapse.60<-zyp.sen(Tavg.m~Year, coefsy60); ci.senavglapse.60<-confint(senavglapse.60)

sentrends.60<-list(senmax0.60, senmin0.60, senavg0.60, sendtr0.60,
                    senmaxlapse.60, senminlapse.60, senavglapse.60)
senCI.60<-list(ci.senmax0.60, ci.senmin0.60, ci.senavg0.60, ci.sendtr0.60,
                ci.senmaxlapse.60, ci.senminlapse.60, ci.senavglapse.60)

trend.60<-data.frame(trend=unlist(lapply(sentrends.60, function(x) coef(x)["Year"])),
                    CI=matrix(unlist(lapply(senCI.60, function(x) x["Year",])), 
                              ncol=2, byrow=T),
                    metric=rep(c("T(z=0)", "-dT/dz"), times=c(4,3) ),
                    start="1958",
                    measure=c("Tmax", "Tmin", "Tavg", "DTR", "Tmax", "Tmin", "Tavg"))
names(trend.60)[2:3]<-c("CI.025", "CI.975")

###### z=0 Trends over 1978-2017: ###### 
coefsy40<-subset(coefsy, coefsy$Year>=1978)
## T(z=0): b
senmax0.40<-zyp.sen(Tmax.b~Year, coefsy40); ci.senmax0.40<-confint(senmax0.40) 
senmin0.40<-zyp.sen(Tmin.b~Year, coefsy40); ci.senmin0.40<-confint(senmin0.40)
senavg0.40<-zyp.sen(Tavg.b~Year, coefsy40); ci.senavg0.40<-confint(senavg0.40)
sendtr0.40<-zyp.sen(DTR.b~Year, coefsy40); ci.sendtr0.40<-confint(sendtr0.40)

## Lapse rates: -m, -dT/dz (should be opposite in sign)
senmaxlapse.40<-zyp.sen(Tmax.m~Year, coefsy40); ci.senmaxlapse.40<-confint(senmaxlapse.40) 
senminlapse.40<-zyp.sen(Tmin.m~Year, coefsy40); ci.senminlapse.40<-confint(senminlapse.40)
senavglapse.40<-zyp.sen(Tavg.m~Year, coefsy40); ci.senavglapse.40<-confint(senavglapse.40)

sentrends.40<-list(senmax0.40, senmin0.40, senavg0.40, sendtr0.40,
                   senmaxlapse.40, senminlapse.40, senavglapse.40)
senCI.40<-list(ci.senmax0.40, ci.senmin0.40, ci.senavg0.40, ci.sendtr0.40,
               ci.senmaxlapse.40, ci.senminlapse.40, ci.senavglapse.40)

trend.40<-data.frame(trend=unlist(lapply(sentrends.40, function(x) coef(x)["Year"])),
                     CI=matrix(unlist(lapply(senCI.40, function(x) x["Year",])), 
                               ncol=2, byrow=T),
                     metric=rep(c("T(z=0)", "-dT/dz"), times=c(4,3) ),
                     start="1978",
                     measure=c("Tmax", "Tmin", "Tavg", "DTR", "Tmax", "Tmin", "Tavg"))
names(trend.40)[2:3]<-c("CI.025", "CI.975")

###### Trends over 1998-2017: ###### 
coefsy20<-subset(coefsy, coefsy$Year>=1998)
## T(z=0): b
senmax0.20<-zyp.sen(Tmax.b~Year, coefsy20); ci.senmax0.20<-confint(senmax0.20) 
senmin0.20<-zyp.sen(Tmin.b~Year, coefsy20); ci.senmin0.20<-confint(senmin0.20)
senavg0.20<-zyp.sen(Tavg.b~Year, coefsy20); ci.senavg0.20<-confint(senavg0.20)
sendtr0.20<-zyp.sen(DTR.b~Year, coefsy20); ci.sendtr0.20<-confint(sendtr0.20)

## Lapse rates: -m, -dT/dz (should be opposite in sign)
senmaxlapse.20<-zyp.sen(Tmax.m~Year, coefsy20); ci.senmaxlapse.20<-confint(senmaxlapse.20) 
senminlapse.20<-zyp.sen(Tmin.m~Year, coefsy20); ci.senminlapse.20<-confint(senminlapse.20)
senavglapse.20<-zyp.sen(Tavg.m~Year, coefsy20); ci.senavglapse.20<-confint(senavglapse.20)

sentrends.20<-list(senmax0.20, senmin0.20, senavg0.20, sendtr0.20,
                   senmaxlapse.20, senminlapse.20, senavglapse.20)
senCI.20<-list(ci.senmax0.20, ci.senmin0.20, ci.senavg0.20, ci.sendtr0.20,
               ci.senmaxlapse.20, ci.senminlapse.20, ci.senavglapse.20)

trend.20<-data.frame(trend=unlist(lapply(sentrends.20, function(x) coef(x)["Year"])),
                     CI=matrix(unlist(lapply(senCI.20, function(x) x["Year",])), 
                               ncol=2, byrow=T),
                     metric=rep(c("T(z=0)", "-dT/dz"), times=c(4,3) ),
                     start="1998",
                     measure=c("Tmax", "Tmin", "Tavg", "DTR", "Tmax", "Tmin", "Tavg"))
names(trend.20)[2:3]<-c("CI.025", "CI.975")

sentrend<-rbind(trend.100, trend.60, trend.40, trend.20)

write.csv(sentrend, file="SenTrends_2017_05_58_78_98.csv", row.names=F)
## SEE SenTrends.xlsx

###############################################################################
############### A1.2 Plotting time series (ANNUAL) #######################################
Tmax.exp<-expression("T"["max, z0"])
Tmin.exp<-expression("T"["min, z0"])
Tavg.exp<-expression("T"["avg, z0"])
DTR.exp<-expression("DTR"[" z0"])

Tmax.tr<-coef(senmax0.100)[2]*10 #100-year trend, convert to per decade
Tmin.tr<-coef(senmin0.100)[2]*10
Tavg.tr<-coef(senavg0.100)[2]*10
DTR.tr<-coef(sendtr0.100)[2]*10

Tmax.tr100.leg<-bquote(+.(round(Tmax.tr, 2))~degree*C~dec^-1*", 1905-2017")
Tmin.tr100.leg<-bquote(+.(round(Tmin.tr, 2))~degree*C~dec^-1*", 1905-2017")
Tavg.tr100.leg<-bquote(+.(round(Tavg.tr, 2))~degree*C~dec^-1*", 1905-2017")
DTR.tr100.leg<-bquote(.(round(DTR.tr, 2))~degree*C~dec^-1*", 1905-2017")

Tmax.tr60<-coef(senmax0.60)[2]*10 #60-year trend, convert to per decade
Tmin.tr60<-coef(senmin0.60)[2]*10
Tavg.tr60<-coef(senavg0.60)[2]*10
DTR.tr60<-coef(sendtr0.60)[2]*10

Tmax.tr60.leg<-bquote(+.(round(Tmax.tr60, 2))~degree*C~dec^-1*" (ns), 1958-2017")
Tmin.tr60.leg<-bquote(+.(round(Tmin.tr60, 2))~degree*C~dec^-1*", 1958-2017")
Tavg.tr60.leg<-bquote(+.(round(Tavg.tr60, 2))~degree*C~dec^-1*", 1958-2017")
DTR.tr60.leg<-bquote(.(round(DTR.tr60, 2))~degree*C~dec^-1*", 1958-2017")

# par(op)
# par(mfrow=c(2,2), mar=c(2,5,2,1))
# # Tmax
# with(coefsy, plot(Tmax.b~Year, type="l", col="red",lwd=2, 
#                   ylim=c(mean(coefsy$Tmax.b)-1.5, mean(coefsy$Tmax.b)+1.5),
#                   ylab=Tmax.exp, cex.lab=1.5),
#      axis(1, at=seq(1900, 2017,5)))
# abline(coef(senmax0.100), col="red", lty=2, lwd=2)
# legend("topleft", bty="n", legend=Tmax.tr100.leg, lty=2, lwd=2,col="red")
# 
# # Tmin  
# with(coefsy, plot(Tmin.b~Year, type="l", col="blue",lwd=2,
#                   ylim=c(mean(coefsy$Tmin.b)-1.5, mean(coefsy$Tmin.b)+1.5),
#                   ylab=Tmin.exp, cex.lab=1.5))
# abline(coef(senmin0.100), col="blue", lty=2, lwd=2)
# legend("topleft", bty="n", legend=Tmin.tr100.leg, lty=2, lwd=2,col="blue")
# 
# # Tavg
# with(coefsy, plot(Tavg.b~Year, type="l", col="black",lwd=2,
#                   ylim=c(mean(coefsy$Tavg.b)-1.5, mean(coefsy$Tavg.b)+1.5),
#                   ylab=Tavg.exp, cex.lab=1.5))
# abline(coef(senavg0.100), col="black", lty=2, lwd=2)
# legend("topleft", bty="n", legend=Tavg.tr100.leg, lty=2, lwd=2,col="black")
#   
# # DTR
# with(coefsy, plot(DTR.b~Year, type="l", col="darkgray",lwd=2,
#                   ylab=DTR.exp, cex.lab=1.5))
# abline(coef(sendtr0.100), col="darkgray", lty=2, lwd=2)
# legend("bottomleft", bty="n", legend=DTR.tr100.leg, lty=2, lwd=2,col="darkgray")
#   
###############################################################################
############### A1.3 Plotting time series (by MONTH) #######################################
st.ts<-1905
nd.ts<-2017

coefs<-read.csv("monthly_lm_2017.csv")
rename<-grep("coefs", names(coefs))  
newname<-unlist(strsplit(names(coefs)[rename], split="coefs_"))[seq(2,18,by=2)]
names(coefs)[rename]<-newname

# USE MONTHLY SERIES
monthly<-ts(coefs[,-1], frequency=12, start=c(st.ts,1), end=c(nd.ts,12))

# Extract month-specific time series: Jans, Febs, etc
months.l<-list()
cyc<-cycle(monthly)
for (i in 1:12){
  months.l[[i]]<-monthly[cyc==i,c("Tmax.b", "Tmin.b", "Tavg.b",  "DTR.b")]
}
names(months.l)<-month.abb
months.ts<-lapply(months.l, FUN=ts, start=st.ts, end=nd.ts, frequency=1)

# # Make a series of graphs of the raw annual time series and trends
# pdf("TimeSeriesbyMonth_lm_2017_1905.pdf")
# par(mfrow=c(6,2), mar=c(2,4,2,2))
# 
# sen.mo.max<-list()
# sen.mo.min<-list()
# for (i in 1:12){
#   x<-as.numeric(time(months.ts[[i]]))
# 
#   ymax<-as.numeric(months.ts[[i]][,"Tmax.b"])
#   sen.mo.max[[i]]<-zyp.sen(ymax~x)
#   plot(months.ts[[i]][,"Tmax.b"],
#        main=paste0(names(months.ts)[i], ", ",
#                    round((sen.mo.max[[i]]$coefficients[2])*10, 3),
#                    " C/decade"),
#        ylab=Tmax.exp, col="red", cex.lab=1.2)
#   abline(sen.mo.max[[i]]$coefficients, lty=2)
# 
#   ymin<-as.numeric(months.ts[[i]][,"Tmin.b"])
#   sen.mo.min[[i]]<-zyp.sen(ymin~x)
#   plot(months.ts[[i]][,"Tmin.b"],
#        main=paste0(names(months.ts)[i], ", ",
#                    round((sen.mo.min[[i]]$coefficients[2])*10, 3),
#                    " C/decade"),
#        ylab=Tmin.exp,col="blue", cex.lab=1.2)
#   abline(sen.mo.min[[i]]$coefficients, lty=2)
# }
# dev.off()

######### develop figure for manuscript: monthly long term trends
# Calculate sen slope
mos<-time(months.ts)  # for the whole 100-year period
xtime<-st.ts:nd.ts
tmax0<-lapply(months.ts, function(x){x[,"Tmax.b"]}) # extract Tmax.b by month
tmin0<-lapply(months.ts, function(x){x[,"Tmin.b"]}) # extract Tmin.b by month
tavg0<-lapply(months.ts, function(x){x[,"Tavg.b"]}) # extract Tavg.b by month
#dtr0<-lapply(months.ts, function(x){x[,"DTR.b"]}) # extract DTR.b by month

mo.trend<-function(y, xtime){zyp.sen(y~xtime)}

Tmax0.zyp<-lapply(tmax0, mo.trend, xtime=xtime)
Tmax0.tr<-lapply(Tmax0.zyp, coef)
Tmax0.95ci<-lapply(Tmax0.zyp, confint.zyp)

Tmin0.zyp<-lapply(tmin0, mo.trend, xtime=xtime)
Tmin0.tr<-lapply(Tmin0.zyp, coef)
Tmin0.95ci<-lapply(Tmin0.zyp, confint.zyp)

# Combine into dataframe and convert to degrees per decade
MinMaxTrend<-data.frame(
  month=month.abb,
  Tmax0.tr=matrix(unlist(Tmax0.tr), byrow=T, ncol=2)[,2]*10,
  Tmax0.lo=matrix(unlist(Tmax0.95ci), byrow=T, ncol=4)[,2]*10,
  Tmax0.up=matrix(unlist(Tmax0.95ci), byrow=T, ncol=4)[,4]*10,
  Tmin0.tr=matrix(unlist(Tmin0.tr), byrow=T, ncol=2)[,2]*10,
  Tmin0.lo=matrix(unlist(Tmin0.95ci), byrow=T, ncol=4)[,2]*10,
  Tmin0.up=matrix(unlist(Tmin0.95ci), byrow=T, ncol=4)[,4]*10)

# pdf("TrendsbyMonth_Sen_1905_2017.pdf")
# par(mfrow=c(1,1), mar=c(4,5,4,2))
# plot(MinMaxTrend$Tmax0.tr, pch=0, cex=2.5,
#      ylim=c(-0.05, max(MinMaxTrend$Tmin0.up)),
#      ylab=expression(Trend~(degree*C~decade^-1)), 
#      xlab="Month", xaxt = "n",
#      main="Temperature trends near sea level, 1905-2017")
# abline(0,0)
# segments(1:12, MinMaxTrend$Tmax0.lo, 
#          1:12, MinMaxTrend$Tmax0.up,
#          col="red",lwd=3) 
# points(1:12+0.1, MinMaxTrend$Tmin0.tr, pch=0, cex=2.5)
# segments(1:12+0.1, MinMaxTrend$Tmin0.lo, 
#          1:12+0.1, MinMaxTrend$Tmin0.up,
#          col="blue",lwd=3) 
# axis(side=1, at=1:12, labels=month.abb)
# legend(x="bottom", ncol=2, bty="n",
#        legend=c(Tmax.exp, Tmin.exp),
#        lwd=3, col=c("red", "blue"))
# dev.off()


months58.ts<-lapply(months.l, FUN=ts, start=1958, end=2017, frequency=1)

mos58<-time(months58.ts)  # for the whole 100-year period
xtime58<-1958:2017
tmax058<-lapply(months58.ts, function(x){x[,"Tmax.b"]}) # extract Tmax.b by month
tmin058<-lapply(months58.ts, function(x){x[,"Tmin.b"]}) # extract Tmin.b by month


Tmax0.zyp58<-lapply(tmax058, mo.trend, xtime=xtime58)
Tmax0.tr58<-lapply(Tmax0.zyp58, coef)
Tmax0.95ci58<-lapply(Tmax0.zyp58, confint.zyp)

Tmin0.zyp58<-lapply(tmin058, mo.trend, xtime=xtime58)
Tmin0.tr58<-lapply(Tmin0.zyp58, coef)
Tmin0.95ci58<-lapply(Tmin0.zyp58, confint.zyp)

# Combine into dataframe and convert to degrees per decade
MinMaxTrend58<-data.frame(
  month=month.abb,
  Tmax0.tr=matrix(unlist(Tmax0.tr58), byrow=T, ncol=2)[,2]*10,
  Tmax0.lo=matrix(unlist(Tmax0.95ci58), byrow=T, ncol=4)[,2]*10,
  Tmax0.up=matrix(unlist(Tmax0.95ci58), byrow=T, ncol=4)[,4]*10,
  Tmin0.tr=matrix(unlist(Tmin0.tr58), byrow=T, ncol=2)[,2]*10,
  Tmin0.lo=matrix(unlist(Tmin0.95ci58), byrow=T, ncol=4)[,2]*10,
  Tmin0.up=matrix(unlist(Tmin0.95ci58), byrow=T, ncol=4)[,4]*10)


##################################################################
####### Making a master figure
pdf("Fig3_Sen_05_58_17f.pdf")
mat<-matrix(data=c(1:4,5,5), nrow=3, ncol=2, byrow=T) 
layout(mat)

# Tmax
par(fig=c(0,0.5,0.7,1), mar=c(0,5,0,0), oma=c(1,1,3,1))
with(coefsy, plot(Tmax.b~Year, type="l", col="red",lwd=2, 
                  ylim=c(26, 30),
                  cex.axis=1.1, xaxt='n', yaxt='n', ylab=""))
axis(side=1, at=seq(1910, 2017, by=10), labels=F, tick=T, tcl=-0.3)
axis(side=2, labels=T, tick=T, cex.axis=1.1, tcl=-0.3, las=1)
mtext(side=2, text=Tmax.exp, cex=1.3, line=3)
mtext(text="Sea level surface air temperatures and trends", outer=T,
      side=3, line=0.6)

abline(coef(senmax0.100), col="red", lty=2, lwd=2)
legend("topleft", bty="n", 
       legend=as.expression(c(Tmax.tr100.leg, Tmax.tr60.leg)),
       lty=c("33","12"), lwd=2,
       col=c("red", "black"), cex=1.1)
usr <- par("usr")
clip(x1=1958, usr[2], usr[3], usr[4])
abline(coef(senmax0.60), col="black", lty="12", lwd=2)


# Tmin  
par(fig=c(0.5,1,0.7,1), new=T, mar=c(0,0,0,5), oma=c(1,1,3,1))
with(coefsy, 
     plot(Tmin.b~Year, type="l", col="blue",lwd=2,
                  ylim=c(18, 22),
                  xaxt='n', yaxt='n', las=1))
axis(side=1, at=seq(1910, 2017, by=10), labels=F, tick=T, tcl=-0.3)
axis(side=4, labels=T, tick=T, cex.axis=1.1, 
     tcl=-0.3, las=1)
mtext(side=4, text=Tmin.exp, cex=1.3, line=4)

abline(coef(senmin0.100), col="blue", lty="33", lwd=2)
legend("topleft", bty="n", 
       legend=as.expression(c(Tmin.tr100.leg, Tmin.tr60.leg)), 
       lty=c("33","12"), lwd=2,
       col=c("blue", "black"), cex=1.1)
usr <- par("usr")
clip(x1=1958, usr[2], usr[3], usr[4])
abline(coef(senmin0.60), col="black", lty="12", lwd=2)


# Tavg
par(fig=c(0,0.5,0.4,0.7), new=T, mar=c(0,5,0,0), oma=c(1,1,3,1))
with(coefsy, plot(Tavg.b~Year, type="l", col="black",lwd=2,
                  ylim=c(22, 26), ylab="", cex.axis=1.1, xaxt='n', yaxt='n'))
axis(side=1, at=seq(1910, 2017, by=10), labels=F, tick=T)
axis(side=1, at=seq(1920, 2017, by=20), labels=T, tick=T)
axis(side=2, labels=T, tick=T, cex.axis=1.1, tcl=-0.3, las=1)
mtext(side=2, text=Tavg.exp, cex=1.3, line=3)

abline(coef(senavg0.100), col="black", lty="33", lwd=2)
legend("topleft", bty="n", 
       legend=as.expression(c(Tavg.tr100.leg, Tavg.tr60.leg)), 
       lty=c("33","12"), lwd=2,
       col=c("black", "gray"), cex=1.1)
usr <- par("usr")
clip(x1=1958, usr[2], usr[3], usr[4])
abline(coef(senavg0.60), col="gray", lty="12", lwd=2)

# DTR
par(fig=c(0.5,1,0.4,0.7), new=T, mar=c(0,0,0,5), oma=c(1,1,3,1))
with(coefsy, plot(DTR.b~Year, type="l", col="darkgray",lwd=2,
                  xaxt='n', yaxt='n', ylim=c(6, 10)))
axis(side=1, at=seq(1910, 2017, by=10), labels=F, tick=T)
axis(side=1, at=seq(1920, 2017, by=20), labels=T, tick=T)
axis(side=4, labels=T, tick=T, tcl=-0.3, cex.axis=1.1, las=1)
mtext(side=4, text=DTR.exp, cex=1.3, line=4)

abline(coef(sendtr0.100), col="darkgray", lty="33", lwd=2)
legend("bottomleft", bty="n", 
       legend=as.expression(c(DTR.tr100.leg, DTR.tr60.leg)), 
       lty=c("33","12"), lwd=2,
       col=c("gray", "black"), cex=1.1)

usr <- par("usr")  # recent trend
clip(x1=1958, usr[2], usr[3], usr[4])
abline(coef(sendtr0.60), col="black", lty="12", lwd=2)

# Monthly trends: 1905-2017
par(fig=c(0,0.5,0,0.4), new=T, mar=c(2,5,3,0.1), oma=c(1,1,3,1))
month.A<-substr(month.abb, start=1, stop=1)

plot(MinMaxTrend$Tmax0.tr, pch=2, col="red", cex=1.5, 
     ylim=c(-0.1, 0.35), ylab="",
     xlab="Month", xaxt = "n", cex.lab=1.3, cex.axis=1.1, las=1, tcl=-0.3)
mtext(side=2, text=expression(Trend~(degree*C~dec^-1)), cex=1, line=3)
mtext("1905-2017", 3, line=-2, cex=0.9)
abline(0,0)
segments(1:12, MinMaxTrend$Tmax0.lo, 
         1:12, MinMaxTrend$Tmax0.up,
         col="red",lwd=2) 
points(1:12+0.1, MinMaxTrend$Tmin0.tr, pch=6, col="blue", cex=1.5)
segments(1:12+0.1, MinMaxTrend$Tmin0.lo, 
         1:12+0.1, MinMaxTrend$Tmin0.up,
         col="blue",lwd=2) 
axis(side=1, at=1:12, labels=month.A, tcl=-0.3)
legend(x="bottom", ncol=2, bty="n", pch=c(2,6),
       legend=c(Tmax.exp, Tmin.exp), cex=1.2,
       col=c("red", "blue"), bg="white")

# Monthly trends: 1958-2017
par(fig=c(0.5,1,0,0.4), new=T, mar=c(2,0.1,3,5), oma=c(1,1,3,1), new=T)
month.A<-substr(month.abb, start=1, stop=1)

plot(MinMaxTrend58$Tmax0.tr, pch=2, col="red", cex=1.5,
     ylim=c(-0.1, 0.35), xlab="Month", xaxt="n", 
     yaxt = "n", cex.lab=1.5, cex.axis=1.1)
mtext("1958-2017", 3, line=-2, cex=0.9)
axis(side=4, labels=T, tick=T, cex.axis=1.1, tcl=-0.3, las=1)
mtext(side=4, text=expression(Trend~(degree*C~dec^-1)), cex=1, line=4)
abline(0,0)
segments(1:12, MinMaxTrend58$Tmax0.lo, 
         1:12, MinMaxTrend58$Tmax0.up,
         col="red",lwd=2) 
points(1:12+0.1, MinMaxTrend58$Tmin0.tr, pch=6, col="blue", cex=1.5)
segments(1:12+0.1, MinMaxTrend58$Tmin0.lo, 
         1:12+0.1, MinMaxTrend58$Tmin0.up,
         col="blue",lwd=2) 
axis(side=1, at=1:12, labels=month.A)
legend(x="bottom", ncol=2, bty="n", pch=c(2,6),
       legend=c(Tmax.exp, Tmin.exp), cex=1.2,
       col=c("red", "blue"), bg="white")
dev.off()

