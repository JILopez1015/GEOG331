### Activity #4
##JL 3/23/2022

#load in lubridate
library(lubridate)

####reading in stream flow data######----
datH<- read.csv("Z://data/streamflow/stream_flow_data.csv",
                na.strings = c("Eqp"))
head(datH)


####reading in Precipitation (precip) data###----
datP<-read.csv("Z://data/streamflow/2049867.csv")

##hourly precip is in mm

head(datP)

####Only using the most reliable measurements (flagged as A by USGS)####----

datD<- datH[datH$discharge.flag == "A",]
