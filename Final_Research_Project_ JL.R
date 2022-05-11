
#####    Final Project #####
#####    5/6/2022                                  #####

#####   Loading packages                                     ####
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)



#####   Loading in Hourly Temp data                          #####

##metadata link https://ag.arizona.edu/azmet/raw2003.htm

T17 <- read.csv(url("https://ag.arizona.edu/azmet/data/0617rh.txt"), 
                header = FALSE)
colnames(T17) <- c("Year", "Day of Year (DOY)","Hour of Day", "Air Temperature",
                   "Rel. Humidity","Vapor Pressure Deficit","Solar Radiation",
                   "Precipitation","Soil Temperature  ( = 2in prior to 1999 )",
                   "20in Soil Temperature  ( = 4in prior to 1999 )", 
                   "Wind Speed (Ave)","Wind Vector Magnitude","Wind Vector Direction",
                   " Wind Direction Standard Deviation","Max Wind Speed",
                   "Reference Evapotranspiration (ETo)")


T18 <- read.csv(url("https://ag.arizona.edu/azmet/data/0618rh.txt"),
                header=FALSE)
colnames(T18) <- c("Year", "Day of Year (DOY)","Hour of Day", "Air Temperature",
                   "Rel. Humidity","Vapor Pressure Deficit","Solar Radiation",
                   "Precipitation","Soil Temperature  ( = 2in prior to 1999 )",
                   "20in Soil Temperature  ( = 4in prior to 1999 )", 
                   "Wind Speed (Ave)","Wind Vector Magnitude","Wind Vector Direction",
                   " Wind Direction Standard Deviation","Max Wind Speed",
                   "Reference Evapotranspiration (ETo)")

T19 <- read.csv(url("https://ag.arizona.edu/azmet/data/0619rh.txt"),
                header=FALSE)
colnames(T19) <- c("Year", "Day of Year (DOY)","Hour of Day", "Air Temperature",
                   "Rel. Humidity","Vapor Pressure Deficit","Solar Radiation",
                   "Precipitation","Soil Temperature  ( = 2in prior to 1999 )",
                   "20in Soil Temperature  ( = 4in prior to 1999 )", 
                   "Wind Speed (Ave)","Wind Vector Magnitude","Wind Vector Direction",
                   " Wind Direction Standard Deviation","Max Wind Speed",
                   "Reference Evapotranspiration (ETo)")

T20 <- read.csv(url("https://ag.arizona.edu/azmet/data/0620rh.txt"),
                header=FALSE)
colnames(T20) <- c("Year", "Day of Year (DOY)","Hour of Day", "Air Temperature",
                   "Rel. Humidity","Vapor Pressure Deficit","Solar Radiation",
                   "Precipitation","Soil Temperature  ( = 2in prior to 1999 )",
                   "20in Soil Temperature  ( = 4in prior to 1999 )", 
                   "Wind Speed (Ave)","Wind Vector Magnitude","Wind Vector Direction",
                   " Wind Direction Standard Deviation","Max Wind Speed",
                   "Reference Evapotranspiration (ETo)")

T21 <- read.csv(url("https://azmet.arizona.edu/azmet/data/0621rh.txt"),
                header=FALSE)
colnames(T21) <- c("Year", "Day of Year (DOY)","Hour of Day", "Air Temperature",
                   "Rel. Humidity","Vapor Pressure Deficit","Solar Radiation",
                   "Precipitation","Soil Temperature  ( = 2in prior to 1999 )",
                   "20in Soil Temperature  ( = 4in prior to 1999 )", 
                   "Wind Speed (Ave)","Wind Vector Magnitude","Wind Vector Direction",
                   " Wind Direction Standard Deviation","Max Wind Speed",
                   "Reference Evapotranspiration (ETo)")
T17.2 <- T17[,1:4]
T18.2 <- T18[,1:4]
T19.2 <- T19[,1:4]
T20.2 <- T20[,1:4]
T21.2 <- T21[,1:4]


#####   Loading in Daily Electricity Demand Data             ####

E17 <- (read.csv("Z:\\students/jlopez/Research Project/Data/EIA_Demand_5yr/EIA_1.17_12.17.csv", 
                header= TRUE )[-366,])

E18 <- (read.csv("Z:\\students/jlopez/Research Project/Data/EIA_Demand_5yr/EIA_1.18_12.18.csv", 
                   header= TRUE)[-366,])

E19 <- (read.csv("Z:\\students/jlopez/Research Project/Data/EIA_Demand_5yr/EIA_1.19_12.19.csv", 
                    header= TRUE)[-366,])

E20 <- (read.csv("Z:\\students/jlopez/Research Project/Data/EIA_Demand_5yr/EIA_1.20_12.20.csv", 
            header= TRUE)[-367,])

E21 <- (read.csv("Z:\\students/jlopez/Research Project/Data/EIA_Demand_5yr/EIA_1.21_12.21.csv", 
                header= TRUE)[-366,])


#####   Loading in Hourly Electricity Demand Data            #####

#2017 hourly data: Heat Wave classification found in "Finding Heat Waves"
E.hr.17 <- read.csv("Z:\\students/jlopez/Research Project/Data/EIA_Demand_5yr/AZPS_2017_heat_wave_hourly.csv", 
                                     header= TRUE)
 #removing MST from time column

E.hr.17$Timestamp..Hour.Ending. <- gsub("MST", "", as.character(E.hr.17$Timestamp..Hour.Ending.))

#2018 hourly data
E.hr.18 <- read.csv("Z:\\students/jlopez/Research Project/Data/EIA_Demand_5yr/AZPS_2018_heat_wave_hourly.csv", 
                    header= TRUE)

#removing MST from time column
E.hr.18$Timestamp..Hour.Ending. <- gsub("MST", "", as.character(E.hr.18$Timestamp..Hour.Ending.))

#2019 Hourly
E.hr.19 <- read.csv("Z:\\students/jlopez/Research Project/Data/EIA_Demand_5yr/AZPS_2019_heat_wave_hourly.csv", 
                    header= TRUE)
#removing MST from time column

E.hr.19$Timestamp..Hour.Ending. <- gsub("MST", "", as.character(E.hr.19$Timestamp..Hour.Ending.))


#2020 hourly
E.hr.20 <- read.csv("Z:\\students/jlopez/Research Project/Data/EIA_Demand_5yr/AZPS_2020_heat_wave_hourly.csv", 
                    header= TRUE)
#removing MST from time column

E.hr.20$Timestamp..Hour.Ending. <- gsub("MST", "", as.character(E.hr.20$Timestamp..Hour.Ending.))


#2021 hourly
E.hr.21 <- read.csv("Z:\\students/jlopez/Research Project/Data/EIA_Demand_5yr/AZPS_2021_heat_wave_hourly.csv", 
                    header= TRUE)

#removing MST from time column

E.hr.21$Timestamp..Hour.Ending. <- gsub("MST", "", as.character(E.hr.21$Timestamp..Hour.Ending.))

#####   Formatting Time on ED Daily Data                     ####

#convert date and time
datesD <- as.Date(E17$Timestamp..Hour.Ending., "%m/%d/%Y")
datesD.2 <- as.Date(E18$Timestamp..Hour.Ending., "%m/%d/%Y")
datesD.3 <- as.Date(E19$Timestamp..Hour.Ending., "%m/%d/%Y")
datesD.4 <- as.Date(E20$Timestamp..Hour.Ending., "%m/%d/%Y")
datesD.5 <- as.Date(E21$Timestamp..Hour.Ending., "%m/%d/%Y")

 

#get day of year
E17$doy <- yday(datesD)
E18$doy <- yday(datesD.2)
E19$doy <- yday(datesD.3)
E20$doy <- yday(datesD.4)
E21$doy <- yday(datesD.5)


#calculate year
E17$year <- year(datesD)
E18$year <- year(datesD.2)
E19$year <- year(datesD.3)
E20$year <- year(datesD.4)
E21$year <- year(datesD.5)



#defining month
E17$month<-month(datesD)
E18$month<-month(datesD.2)
E19$month<-month(datesD.3)
E20$month<-month(datesD.4)
E21$month<-month(datesD.5)

#####   Formatting Time on ED Hourly Data                    ####

# convert column to date class


datHE <- bind_rows(E.hr.17,E.hr.18,E.hr.19,E.hr.20,E.hr.21)
  
datesE <- as.Date(datHE$Timestamp..Hour.Ending., "%m/%d/%Y")

datHE$doy <- yday(datesE)

datHE$year <- year(datesE)




datesE.18 <- as.Date(E.hr.18$Timestamp..Hour.Ending., "%m/%d/%Y")
datesE.19 <- as.Date(E.hr.19$Timestamp..Hour.Ending., "%m/%d/%Y")
datesE.20 <- as.Date(E.hr.20$Timestamp..Hour.Ending., "%m/%d/%Y")
datesE.21 <- as.Date(E.hr.21$Timestamp..Hour.Ending., "%m/%d/%Y")

#doy for hourly

E.hr.18$doy <- yday(datesE.18)
E.hr.19$doy <- yday(datesE.19)
E.hr.20$doy <- yday(datesE.20)
E.hr.21$doy <- yday(datesE.21)


#year for hourly

E.hr.18$year <- year(datesE.18)
E.hr.19$year <- year(datesE.19)
E.hr.20$year <- year(datesE.20)
E.hr.21$year <- year(datesE.21)

#month for hourly
E.hr.17$month <- month(datesE)
E.hr.18$month <- month(datesE.18)
E.hr.19$month <- month(datesE.19)
E.hr.20$month <- month(datesE.20)
E.hr.21$month <- month(datesE.21)
  
  #hour for hourly
dates<-mdy_h(E.hr.17$Timestamp..Hour.Ending.)
dates.18<-mdy_h(E.hr.18$Timestamp..Hour.Ending.)
dates.19<-mdy_h(E.hr.19$Timestamp..Hour.Ending.)
dates.20<-mdy_h(E.hr.20$Timestamp..Hour.Ending.)
dates.21<-mdy_h(E.hr.21$Timestamp..Hour.Ending.)

datDdates <- mdy_h(datD$time)
datD$hod <- hour(datDdates)+(minute(datDdates)/60)

#getting hour 
E.hr.17$hour<- hour(dates)+(minute(dates)/60)
E.hr.18$hour<- hour(dates.18)+(minute(dates.18)/60)
E.hr.19$hour<- hour(dates.19)+(minute(dates.19)/60)
E.hr.20$hour<- hour(dates.20)+(minute(dates.20)/60)
E.hr.21$hour<- hour(dates.21)+(minute(dates.21)/60)

e.hr.17.2 <- as.data.frame(separate(E.hr.17,col= Timestamp..Hour.Ending., into=c('date','time',"AM/PM"),sep = "^\\S*\\K\\s+" ))
datD <- as.data.frame(separate(e.hr.17.2,col= time, into=c('hour',"locale"),sep = "^\\S*\\K\\s+" ))


#1=am, 2=pm
datD$AM.PM <-ifelse(datD$locale=="a.m. ",1,2)
datD$`AM/PM` <- ifelse(datD$AM.PM==1, "AM", "PM")

#time with proper formatting
datD$time <- paste(datD$date,datD$hour, datD$`AM/PM`)

                            
                            
                         
#####   Formatting Hourly T to combine with EDem             ####

#getting means of days from 24 hr data
T.avg.17 <- aggregate(T17.2$`Air Temperature`, 
                      by=list(Category=T17.2$`Day of Year (DOY)`), FUN=mean)
colnames(T.avg.17) <- c("doy", "Avg. Air Temp")

T.avg.18 <- aggregate(T18.2$`Air Temperature`, 
                      by=list(Category=T18.2$`Day of Year (DOY)`), FUN=mean)
colnames(T.avg.18) <- c("doy", "Avg. Air Temp")

T.avg.19 <- aggregate(T19.2$`Air Temperature`, 
                      by=list(Category=T19.2$`Day of Year (DOY)`), FUN=mean)
colnames(T.avg.19) <- c("doy", "Avg. Air Temp")

T.avg.20 <- aggregate(T20.2$`Air Temperature`, 
                      by=list(Category=T20.2$`Day of Year (DOY)`), FUN=mean)
colnames(T.avg.20) <- c("doy", "Avg. Air Temp")

T.avg.21 <- aggregate(T21.2$`Air Temperature`,by=list
                      (Category=T21.2$`Day of Year (DOY)`), FUN=mean)
colnames(T.avg.21) <- c("doy", "Avg. Air Temp")


#####   Binding Data                                         ####

TED17 <- cbind(T.avg.17, E17)
TED18 <- cbind(T.avg.18, E18)
TED19 <- cbind(T.avg.19, E19)
TED20 <- cbind(T.avg.20, E20)
TED21 <- cbind(T.avg.21, E21)

#all data combined
datT <- rbind(TED17,TED18,TED19,TED20,TED21)
view(datT)

#checking for NA's
which(is.na(datT), arr.ind = TRUE)
#no NA

##making fahrenheit from celsius
datT$Temp.F <- ((datT$`Avg. Air Temp`)*(9/5))+32

#####   Histograms Temp                                      #####

H17 <- hist(T17[,4]  , main= "'17 Temp Distribution", xlab= "Temp")

H18 <- hist(T18[,4]  , main= "18 Temp Distribution", xlab= "Temp")

H19 <- hist(T19[,4]  , main= "'19 Temp Distribution", xlab= "Temp")

H20 <- hist(T20[,4] , main= "'20 Temp Distribution", xlab= "Temp")

H21 <- hist(T21[,4]  , main= "'21 Temp Distribution", xlab= "Temp")


#####   Histogram Electricity Demand                         ####

#yearly
EH17 <- hist( E17$Demand..MWh., main= "'17 Electricity Demand", xlab= "Elec")

##testing normality of data
shapiro.test(E17$Demand..MWh.)

EH18 <- hist(E18$Demand..MWh. , main= "18 Electricity Demand", xlab= "Elec")
shapiro.test(E18$Demand..MWh.)

EH19 <- hist(E19$Demand..MWh. , main= "'19 Electricity Demand", xlab= "Elec")
shapiro.test(E19$Demand..MWh.)

EH20 <- hist(E20$Demand..MWh. , main= "'20 Electricity Demand", xlab= "Elec")
shapiro.test(E20$Demand..MWh.)

EH21 <- hist(E21$Demand..MWh. , main= "'21 Electricity Demand", xlab= "Elec")
shapiro.test(E21$Demand..MWh.)

#####yearly elc. demand is not a normal distribution because the p-value is less 
        #than .05, therefore we must reject the null hypothesis of normality


#####   Plotting Temp and Elec Demand for 5 yrs              ####

#plotting all of the T and Elec.Demand data
plot(x = datT$`Avg. Air Temp`, y=datT$Demand..MWh., xlab = "Avg. Temp.(C)", 
     ylab = "Elec. Demand(MWh)", 
     main= "Electricity Demand and Avg. Yr Temp (2017-2021)", pch=1)

#####   Adding Seasons to Data                               ####

x <- datT$doy
datT$Season<-ifelse(x>=61 & x<=151 ,"Spring",
                                ifelse(x>=152 & x<=243 , "Summer",
                                       ifelse(x>=244 & x<=334 ,"Fall","Winter")))
#####   Plotting Seasonal Data                               ####                  

# plotting only summer data and deleting duplicate column
s<- datT[ -c(4,6) ] %>% filter(Season== "Summer")
sum.T.Ed <- plot(x= s$`Avg. Air Temp`, y= s$Demand..MWh., xlab = "Avg. Temp. (C)", 
                 ylab = "Elec. Demand (MWh)", pch=1, 
                 main = "Summer Avg. Temp. change and AZ Electricity Demand") 



#plotting winter data 
#plotting only summer data and deleting duplicate column
w<- datT[ -c(4,6) ] %>% filter(Season== "Winter")
win.T.Ed <- plot(x= w$`Avg. Air Temp`, y= w$Demand..MWh., xlab = "Avg. Temp.(C)", 
                 ylab = "Elec. Demand (MWh)",
                 main = "Winter Avg. Temp. change and AZ Electricity Demand", pch=1) 

#####   Statistical Analysis: Seasonal                       #####

#summer data
sT <- s$`Avg. Air Temp`
sE <- s$Demand..MWh.
cor(sT,sE)
cor.test(sT,sE)
##strong positive correlation is statistically significant because p-value is greater 
#than 0.05, null hypothesis is 0 correlation
s.reg <- lm(sE~sT)
summary(s.reg)

#plotting residuals

plot(s.reg)
hist(s.reg$residuals)
##are residuals normally distributed?
##QQ plot looks normal
##Variance of residuals looks equal

## winter 
wT <- w$`Avg. Air Temp`
wE <- w$Demand..MWh.
cor(wT,wE)
cor.test(wT,wE)
##strong negative correlation is statistically significant because p-value is less  
#than 0.05, null hypothesis is 0 correlation
w.reg <- lm(wE~wT)
summary(w.reg)

#plotting residuals

plot(w.reg) 

hist(w.reg$residuals)

hist(w$Demand..MWh.)
shapiro.test(w$Demand..MWh.)

w$e.dem.log <- log(w$Demand..MWh.)

#####   Yearly Plotting                                      ####

#yearly seasonal data
##2017 plot
plot(x = TED17$`Avg. Air Temp`, y=TED17$Demand..MWh., xlab = "Avg. Temp.(C)", 
     ylab = "Elec. Demand(MWh)", 
     main= "2017 Electricity Demand and Avg. Yr Temp", pch=1)

##2017 with log
TED17$E.Dem.log <- log(TED17$Demand..MWh.)
hist(TED17$Demand..MWh.)
hist(TED17$E.Dem.log)

plot(x = TED17$`Avg. Air Temp`, y=TED17$E.Dem.log, xlab = "Avg. Temp.(C)", 
  ylab = "Elec. Demand(MWh)", 
  main= "2017 Electricity Demand (log) and Avg. Yr Temp", pch=1)


##2018 plot
plot(x = TED18$`Avg. Air Temp`, y=TED18$Demand..MWh., xlab = "Avg. Temp.(C)", 
     ylab = "Elec. Demand(MWh)", 
     main= "2018 Electricity Demand and Avg. Yr Temp", pch=1)

## 2019 plot
plot(x = TED19$`Avg. Air Temp`, y=TED19$Demand..MWh., xlab = "Avg. Temp.(C)", 
     ylab = "Elec. Demand(MWh)", 
     main= "2017 Electricity Demand and Avg. Yr Temp", pch=1)

## 2020 plot
plot(x = TED20$`Avg. Air Temp`, y=TED20$Demand..MWh., xlab = "Avg. Temp.(C)", 
     ylab = "Elec. Demand(MWh)", 
     main= "2020 Electricity Demand and Avg. Yr Temp", pch=1)

#####   Notes                                                ####

#seasonally is "cool"
#energy from whole state, representative

#just keep temp as indep (x) and energy as dep(y)

#when is temp and ac most related? heat waves?


# heat wave days
# what classifies a heat wave 
# hourly energy demand to look at heat wave data
###look at heat wave days and their distribution of energy, doing this for summer
####I NEED TO GET HOUR OF DAY OUT OF EHR17 IN ORDER TO MOVE FORWARD AND MATCH 
  #WITH THE HEATWAVE DATA
#seasonally to daily to hourly and seeing the relationship
##how is energy demand diff at diff time scales

####  Statistical Analysis of Seasonal























#####   Finding Heat Waves                                   #####

#heat waves is when temp has been unusually high for 2 or more days 
#using daily to see which days on average had higher temps

##2017 summer data##

s.17 <- s %>% filter(year== "2017")

#get top 
quantile(s.17$`Avg. Air Temp`, 0.95)

#95% = 37.06792 

b <- s.17$`Avg. Air Temp`
s.17$high.temp<-ifelse(b>=37,"Y","N")

##2018 summer data##

s.18 <- s %>% filter(year== "2018")

#get top 
quantile(s.18$`Avg. Air Temp`, 0.95)

#95% = 36.57896 

b.18 <- s.18$`Avg. Air Temp`
s.18$high.temp<-ifelse(b.18>=36,"Y","N")

#204-8, 217-8  

##2019 S data ##

s.19 <- s %>% filter(year== "2019")

#get top 
quantile(s.19$`Avg. Air Temp`, 0.95)

#95% = 36.23167 

b.19 <- s.19$`Avg. Air Temp`
s.19$high.temp<-ifelse(b.19>=36,"Y","N")

#207-10

## 2020 s data##

s.20 <- s %>% filter(year== "2020")

#get top 
quantile(s.20$`Avg. Air Temp`, 0.95)

#95% = 37.97687 

b.20 <- s.20$`Avg. Air Temp`
s.20$high.temp<-ifelse(b.20>=37,"Y","N")

#193-5, 211-214, 227-8

## 2021 s data##

s.21 <- s %>% filter(year== "2021")

#get top 
quantile(s.21$`Avg. Air Temp`, 0.95)

#95% = 37.16083 

b.21 <- s.21$`Avg. Air Temp`
s.21$high.temp<-ifelse(b.21>=37,"Y","N")

#168-71, 189-90

####    Extracting Summertime Hourly Data                    ####
#extracting summer from overall 2017 data
s.hr.17 <- T17.2 %>% filter(between(`Day of Year (DOY)`,152,243))


#looking at specific days that had heat waves from s.17 data, 171-2, 175-6,188-9
hw.17 <- filter(s.hr.17, `Day of Year (DOY)`==171|`Day of Year (DOY)` == 172|
                  `Day of Year (DOY)` == 175|`Day of Year (DOY)` == 176|
                  `Day of Year (DOY)` == 188|`Day of Year (DOY)` == 189)


#extracting summer from overall 2018 data
s.hr.18 <- T18.2 %>% filter(between(`Day of Year (DOY)`,152,243))

#looking at specific days that had heat waves from s.18 data, 204-8, 217-8
hw.18 <- filter(s.hr.18 , `Day of Year (DOY)`==204|`Day of Year (DOY)`==205|
                  `Day of Year (DOY)`==206|`Day of Year (DOY)`==207|
                  `Day of Year (DOY)`==208|`Day of Year (DOY)`==217|
                  `Day of Year (DOY)`==218)

#extracting summer from overall 2019
s.hr.19 <- T19.2 %>% filter(between(`Day of Year (DOY)`,152,243))

#looking at specific days that had heat waves from s.19 data,207-10
hw.19 <- filter(s.hr.19 , `Day of Year (DOY)`==207|`Day of Year (DOY)`==208|
                  `Day of Year (DOY)`==209|`Day of Year (DOY)`==210)

#extracting summer from overall 2020
s.hr.20 <- T20.2 %>% filter(between(`Day of Year (DOY)`,152,243))

#looking at specific days that had heat waves from s.20 data,193-5, 211-214, 227-8
hw.20 <- filter(s.hr.20 , `Day of Year (DOY)`==193|`Day of Year (DOY)`==194|
                  `Day of Year (DOY)`==195|`Day of Year (DOY)`==211|
                  `Day of Year (DOY)`==212|`Day of Year (DOY)`==213|
                  `Day of Year (DOY)`==214|`Day of Year (DOY)`==227|`Day of Year (DOY)`==228)

#extracting summer from overall 2020
s.hr.21 <- T21.2 %>% filter(between(`Day of Year (DOY)`,152,243))

#looking at specific days that had heat waves from s.21 data, 168-71, 189-90
hw.21 <- filter(s.hr.21 , `Day of Year (DOY)`==168|`Day of Year (DOY)`==169|
                  `Day of Year (DOY)`==170|`Day of Year (DOY)`==171|
                  `Day of Year (DOY)`==189|`Day of Year (DOY)`==190)

#####   Filtering out days from Electricity data to match HW ####

#looking at specific days that had heat waves from 17 data, 171-2, 175-6,188-9
HE17 <- filter(E.hr.17,doy==171| doy== 172|doy == 175| doy== 176| doy== 188|
                 doy== 189) 

#looking at specific days that had heat waves from s.18 data, 204-8, 217-8
HE18 <- filter(E.hr.18, doy==204 |doy ==205|doy==206| doy==207|doy==208|doy==217|
                 doy==218)

#looking at specific days that had heat waves from s.19 data,207-10
HE19 <- filter(E.hr.19, doy==207 |doy ==208|doy==209| doy==210)

#looking at specific days that had heat waves from s.20 data,193-5, 211-214, 227-8
HE20 <- filter(E.hr.20, doy==193 |doy ==194|doy==195| doy==211|doy==212|doy==213|
                 doy==214| doy==227| doy==228)

#looking at specific days that had heat waves from s.21 data, 168-71, 189-90
HE21 <- filter(E.hr.21, doy==168 |doy ==169|doy==170| doy==171|doy==189|doy==190)







































