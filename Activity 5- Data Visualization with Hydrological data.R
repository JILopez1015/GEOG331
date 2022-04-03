### Activity #5
##JL 4/2/2022

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
##labels
months<- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept",
"Oct","Nov","Dec","")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))

#make plot
plot(aveF$doy,aveF$dailyAve,
     type="l",
     xlab="Year",
     xlim=c(0,400),
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

#lines(aveF.2[,2],col="red")
#mtext to move ylab down
mtext(expression(paste("Discharge ft"^"3 ","sec"^"-1")), side=2, at=25, line=2)
      
#axis ticks      
axis(1, seq(0,400, by=33), #tick intervals
     lab=months) #tick labels
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

##piping filtering out each year and then grouping by day and 
#then counting number of days, days with count of 24 have 24hrs worth of data
dh.7 <- datP %>% filter(year == 2007)%>% group_by(doy) %>% summarize(n())

dh.8 <-  datP %>% filter(year == 2008)%>% group_by(doy) %>% summarize(n())

dh.9 <-  datP %>% filter(year == 2009)%>% group_by(doy) %>% summarize(n())

dh.10 <-  datP %>% filter(year == 2010) %>% group_by(doy) %>% summarize(n())

dh.11 <-  datP %>% filter(year == 2011) %>% group_by(doy) %>% summarize(n())

dh.12 <-  datP %>% filter(year == 2012) %>% group_by(doy) %>% summarize(n())

dh.13 <-  datP %>% filter(year == 2013) %>% group_by(doy) %>% summarize(n())

##dataframe for days with 24 hr data, year and date
df.p.twtyfr <- data.frame(x=as.factor(c(paste("2007", which(dh.7$`n()`==24)),
  paste("2008", which(dh.7$`n()`==24)),
  paste("2009", which(dh.7$`n()`==24)),
  paste("2010", which(dh.7$`n()`==24)),
  paste("2011", which(dh.7$`n()`==24)),
  paste("2012", which(dh.7$`n()`==24)),
  paste("2013", which(dh.7$`n()`==24)))))%>% separate(x, c('Year','Day'))


#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))

#make plot
plot(aveF$doy,aveF$dailyAve,
     type="l",
     xlab="Month",
     ylab="",
     lwd=2,
     ylim=c(0,45),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
 

abline(v=c(df.p.twtyfr$Day),
       col="blue",
       lty=2)

#mtext to move ylab down
mtext(expression(paste("Discharge ft"^"3 ","sec"^"-1")), side=2, at=25, line=2)
#axis ticks      
axis(1, seq(0,400, by=33), #tick intervals
     lab=months) #tick labels
axis(2, seq(0,80, by=10),
     seq(0,80, by=10),
     las = 2)#show ticks at 90 degree angle

#adding legend to plot
legend("topright", c("Mean","Days with 24hr precip"), #legend items
       lwd=c(2,1),#lines
       col=c("black","blue",#colors
       bty="n"))#no legend border; might have to run whole plot again to avoid
       #past legend showing up

#####  Q8: My Hydro graph   ############################

#subsest discharge and precipitation within range of interest
##using 2011 with DOY=37 and 42
hydroD.2 <- datD[datD$doy >= 37 & datD$doy < 42 & datD$year == 2011,]
hydroP.2 <- datP[datP$doy >= 37 & datP$doy < 42 & datP$year == 2011,]


#subsest discharge and precipitation within range of interest


#minimun, if great than 0 than no reason to include 0 value
min(hydroD.2$discharge)

##sclaing precip values
#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl.2 <- floor(min(hydroD.2$discharge))-1
#ceiling rounds up to the integer
yh.2<- ceiling(max(hydroD.2$discharge))+1
#minimum and maximum range of precipitation to plot
pl.2 <- 0
pm.2 <- ceiling(max(hydroP.2$HPCP))+.5
#scale precipitation to fit on the
hydroP.2$pscale <- (((yh.2-yl.2)/(pm.2-pl.2)) * hydroP.2$HPCP) + yl.2

###plotting discharge and precip

#start new plot
dev.new(width=8,height=8)

#adding size
par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD.2$decDay,
     hydroD.2$discharge,
     type="l",
     ylim=c(yl.2,8.5),
     lwd=2,
     xlab="Day of year",
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#add bars to indicate precipitation
for(i in 1:nrow(hydroP.2)){
  polygon(c(hydroP.2$decDay[i]-0.017,hydroP.2$decDay[i]-0.017,
            hydroP.2$decDay[i]+0.017,hydroP.2$decDay[i]+0.017),
          c(yl.2,hydroP.2$pscale[i],hydroP.2$pscale[i],yl.2),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)}

#adding legend to plot
legend("topright", c("Discharge","Precip"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),
       bty="n")#no legend border; might have to run whole plot again to avoid
                #past legend showing up

#####  Q9: CREATING VIOLIN PLOT FOR 2016 AND 2017   ####

## adding column with seasons as doy
## winter= 12,1,2
## spring= 3,4,5 (doy=61-151)
## summer= 6,7,8(152-243)
## fall= 9,10,11 (244-334)
x<-datD$doy

datD$Season<-ifelse(x>=61 & x<=151 ,"Spring",
                    ifelse(x>=152 & x<=243 , "Summer",
                           ifelse(x>=244 & x<=334 ,"Fall","Winter")))

##filtering out data from 2016 and 2017
ds16<-datD %>% filter(year== "2016")
ds17<-datD %>% filter(year == "2017")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))

###violiin plot of 2016 discharge
ggplot(data= ds16, aes(Season,discharge)) +
  geom_violin() +
  xlab("Season") +  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1"))) +
  ggtitle("2016 Season Discharge")+ 
  theme_classic()

#start new plot 2017 discharge
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))  
ggplot(data= ds17, aes(Season,discharge)) +
  geom_violin() +
  xlab("Season") +  ylab(expression(paste("Discharge ft"^"3 ","sec"^"-1"))) +
  ggtitle("2017 Seasonal Discharge")+ 
  theme_classic()



























