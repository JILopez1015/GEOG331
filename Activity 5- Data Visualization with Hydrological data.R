### Activity #5
##JL 4/1/2022

#####  load in packages   ####
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyverse)

#####  reading in stream flow data   ######----
datH<- read.csv("Z://data/streamflow/stream_flow_data.csv",
                na.strings = c("Eqp"))
head(datH)


#####  reading in precip data   #############
datP<-read.csv("Z://data/streamflow/2049867.csv")

##hourly precip is in mm

head(datP)

#####  Only using the most reliable measurements (flagged as A by USGS)   #########
#A was flagged as Approved for publication
##stream flow data
datD<- datH[datH$discharge.flag == "A",]

####Variables for Precip and discharge
precip <- datP$HPCP
disch <- datD$discharge
#####  define time for streamflow   #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)
#defining month
datD$month<-month(datesD)

#####  define time for precipitation   #####
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year
datP$year <- year(dateP)
#get month
datP$month <- month(dateP)

#####  get decimal formats   #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))

#calculate times for datP
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))

#####  plot discharge   ####
plot(datD$decYear, datD$discharge, type="l", xlab="Year",
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#####  PLOTTING FREQ WITH HISTOGRAMS   ####

histP<- hist(precip)

histD <- hist(disch)

#####  Average daily discharge across all years   ####
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#new data.frame with only 2017 values
disch.17 <- datD %>% filter(year=="2017")

#new data frame to combine 2017 and overall

#average each day for 2017
aveF.2<-aggregate(disch, by=list(datD$doy), FUN="mean")
colnames(aveF.2) <- c("Day","dailyAve")

sdf.2<-aggregate(disch.17$discharge, by=list(disch.17$doy), FUN="sd")
colnames(sdf.2) <- c("doy","dailySD")

#####  Q5 PLOTTED WITH 2017 DATA   ###########################
#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))

#make plot
plot(aveF$doy,aveF$dailyAve,
     type="l",
     xlab="Year",
     ylab="",
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes

polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
) 

lines(aveF.2[,2],col="red")
#mtext to move ylab down
mtext(expression(paste("Discharge ft"^"3 ","sec"^"-1")), side=2, at=25, line=2)
      
#axis ticks      
axis(1, seq(0,400, by=50), #tick intervals
     lab=seq(0,400, by=50)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle

#adding legend to plot
legend("topright", c("overall mean","2017 mean","1 standard deviation"), #legend items
       lwd=c(2,1),#lines
       col=c("black","red",rgb(0.392, 0.584, 0.929,.2)),#colors
       bty="n",#no legend border; might have to run whole plot again to avoid
                #past legend showing up
       x.intersp = 5 )


#####  Q6 PLOTTED MEDIAN TO SEE DIFFERENCE   ###########################

##calculating median
med.2017 <- aggregate(disch.17$discharge, by=list(disch.17$doy), FUN="median")
colnames(med.2017) <- c("doy","Daily Median")

#plotting daily median and average

#start new plot
dev.new(width=8,height=8)


#make plot
plot(med.2017$doy,med.2017$`Daily Median`,
     type="l",
     xlab="Year",
     ylab="",
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes

lines(aveF.2[,2],col="red")

#mtext to move ylab down
mtext(expression(paste("Discharge ft"^"3 ","sec"^"-1")), side=2, at=25, line=2)

#axis ticks      
axis(1, seq(0,400, by=50), #tick intervals
     lab=seq(0,400, by=50)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle

#####  Q7: DAYS WITH 24 HR PRECIP MEASUREMENTS+PLOTS   #################
#extracting all summer months since we do not have temp
##summer is June to September
P.summer <- datP %>% filter(month==c("6","7","8","9"))


##df of doy and hour

d.h.df <- tibble(P.summer[,c(7,6,5)])
view(d.h.df)

##count indicates which days had less than 24 hrs of precip measurements
#counts less than 5 do not cover full day
count.hr<- d.h.df %>% count(year,doy, sort=TRUE)

param <-ifelse(count.hr$n >=5,
               paste(count.hr$doy,count.hr$year),"0")
param[1:10] ##doy and year that have 24 hr precip


##plotting discharge and days with full precip measurements

#start new plot
dev.new(width=8,height=8)

#bigger margins
#par(mai=c(1,1,1,1))

#make plot
plot(aveF$doy,aveF$dailyAve,
     type="l",
     xlab="Year",
     ylab="",
     lwd=2,
     ylim=c(0,45),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
 

abline(v=c(252,248,258,169,270,234,273,250,162,238),
       col="blue",
       lty=3)

#mtext to move ylab down
mtext(expression(paste("Discharge ft"^"3 ","sec"^"-1")), side=2, at=25, line=2)

#axis ticks      
axis(1, seq(0,400, by=50), #tick intervals
     lab=seq(0,400, by=50)) #tick labels
axis(2, seq(0,35, by=5),
     seq(0,35, by=5),
     las = 2)#show ticks at 90 degree angle

#adding legend to plot
legend("topleft", c("Mean","Days with 24hr precip"), #legend items
       lwd=c(2,1),#lines
       col=c("black","blue",#colors
       bty="n"))#no legend border; might have to run whole plot again to avoid
       #past legend showing up

#####  HYDROGRAPHS  ######################

#subsest discharge and precipitation within range of interest
##using 2007 with DOY=252 and 258
hydroD.2 <- datD[datD$doy >= 169 & datD$doy < 270 & datD$year == 2007,]
hydroP.2 <- datP[datP$doy >= 160 & datP$doy < 270 & datP$year == 2007,]



##scaling precip values
#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <- ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

###plotting discharge and precip

#start new plot
dev.new(width=8,height=8)

#adding size
par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge,
     type="l",
     ylim=c(yl,yh),
     lwd=2,
     xlab="Day of year",
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#add bars to indicate precipitation
for(i in 1:nrow(hydroP)){
        polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
                  hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
                c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)}

#adding legend to plot
legend("topright", c("Discharge","Precip"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
             pch=c(NA,15),
             bty="n")#no legend border; might have to run whole plot again to avoid
#past legend showing up
  
#####  Q8: My Hydrograph   ############################







#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

#minimun, if great than 0 than no reason to include 0 value
min(hydroD$discharge)

##sclaing precip values
#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <- ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

###plotting discharge and precip

#start new plot
dev.new(width=8,height=8)

#adding size
par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge,
     type="l",
     ylim=c(yl,yh),
     lwd=2,
     xlab="Day of year",
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#add bars to indicate precipitation
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)}

#adding legend to plot
legend("topright", c("Discharge","Precip"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),
       bty="n")#no legend border; might have to run whole plot again to avoid
#past legend showing up