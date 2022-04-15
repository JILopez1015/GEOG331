
#####    Final Project #####
#####    4/15/2021                              #####

#####   Loading packages                        ####
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)



#####   Loading in Temp data                    #####

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

T18.2 <- T18[,1:4]
T19.2 <- T19[,1:4]
T20.2 <- T20[,1:4]
T21.2 <- T21[,1:4]


#####   Loading in Electricity Demand Data      ####

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


#####   Formatting Time on ED Data              ####

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


#####   Formatting Tables to combine with EDem  ####
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


#####    Binding Data                           ####

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

#####   Histograms Temp                         #####

H17 <- hist(T17[,4]  , main= "'17 Temp Distribution", xlab= "Temp")

H18 <- hist(T18[,4]  , main= "18 Temp Distribution", xlab= "Temp")

H19 <- hist(T19[,4]  , main= "'19 Temp Distribution", xlab= "Temp")

H20 <- hist(T20[,4] , main= "'20 Temp Distribution", xlab= "Temp")

H21 <- hist(T21[,4]  , main= "'21 Temp Distribution", xlab= "Temp")


#####   Histogram Electricity Demand            ####
EH17 <- hist(E17$Demand..MWh. , main= "'17 Electricity Demand", xlab= "Elec")

EH18 <- hist(E18$Demand..MWh. , main= "18 Electricity Demand", xlab= "Elec")

EH19 <- hist(E19$Demand..MWh. , main= "'19 Electricity Demand", xlab= "Elec")

EH20 <- hist(E20$Demand..MWh. , main= "'20 Electricity Demand", xlab= "Elec")

EH21 <- hist(E21$Demand..MWh. , main= "'21 Electricity Demand", xlab= "Elec")

#####   Plotting Temp and Elec Demand over time ####

#plotting all of the T and Elec.Demand data
plot(x = datT$Demand..MWh.,y=datT$`Avg. Air Temp`,xlab = "Elec. Demand", 
     ylab = "Avg. Temp.", pch=1)

##adding seasons to data

x <- datT$doy
datT$Season<-ifelse(x>=61 & x<=151 ,"Spring",
                                ifelse(x>=152 & x<=243 , "Summer",
                                       ifelse(x>=244 & x<=334 ,"Fall","Winter")))
#plotting only summer data and deleting duplicate column
s<- datT[ -c(4,6) ] %>% filter(Season== "Summer")
sum.T.Ed <- plot(x= s$Demand..MWh., y= s$`Avg. Air Temp`, xlab = "Elec. Demand (MWh)", 
                 ylab = "Summer Avg. Temp. (C)", pch=1, 
                 main = "Summer Avg. Temp. change and AZ Electricity Demand") 

#plotting winter data 
#plotting only summer data and deleting duplicate column
w<- datT[ -c(4,6) ] %>% filter(Season== "Winter")
win.T.Ed <- plot(x= w$Demand..MWh., y= w$`Avg. Air Temp`, xlab = "Elec. Demand", 
                 ylab = "Win Avg. Temp.", pch=1) 
























