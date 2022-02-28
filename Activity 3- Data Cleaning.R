#Activity 3 Questions
#JL 2/28/22


#Testing Code with Assert----
#create a function. The names of the arguments for your function will be in 
#parentheses. Everything in curly brackets will be run each time the function 
#is run.
assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
}
#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

#Read in the data file----
#first 3 rows are additional column info so we are reading in file to not treat
#that description as a character or factor
#also data logger uses #N/A instead of NA so we have to translate this into R

datW<-read.csv("Z:\\students/jlopez/HW/Activity 3/bewkes/bewkes_weather.csv", 
               na.strings = c("#N/A"),skip = 3, header = FALSE )
#preview data
print(datW[1,])



#Getting Sensor info from file----
#this data table will contain all relevant units
sensorInfo<- read.csv("Z:\\students/jlopez/HW/Activity 3/bewkes/bewkes_weather.csv",
                      na.strings = c("#N/A"), nrows=2)

#checking data for sensor
print(sensorInfo)


#Getting column names from sensorInfo table to datW to be the same----

colnames(datW)<-colnames(sensorInfo)

#preview data
print(datW[1,])


#installing lubridate package----

#install.packages(c("lubridate"))
#library(lubridate)

#Standardizing date and time with lubridate package----

dates<-mdy_hm(datW$timestamp, tz="America/New_York")


#calculate day of year
datW$doy<- yday(dates)

#calculate hour in the day
datW$hour<- hour(dates)+(minute(dates)/60)

#calculate decimal day of year 
datW$DD<- datW$doy + (datW$hour/24)

#checking missing data----

#how many missing values each sensor observation has

#airtemp
length(which(is.na(datW$air.temperature)))

#wind speed
length(which(is.na(datW$wind.speed)))

#precipitation
length(which(is.na(datW$precipitation)))

#soil temp
length(which(is.na(datW$soil.temp)))

#soil moisture
length(which(is.na(datW$soil.moisture)))

#plotting Soil moisture and temp because there are a lot of NA's----

#plot with filled in points
#line lines
#soil moisture
plot(datW$DD, datW$soil.moisture, pch=19, type = "b", xlab = "Day of the Year",
     ylab = "Soil Moisture (cm3 water per cm3 soil")

#soil temp
plot(datW$DD, datW$soil.temp, pch=19, type = "b", xlab = "Day of the Year",
     ylab = "Soil Temp")

#Tests for QA/QC----

plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#converting unreliable data to NA, using air temp by taking out any temps that
#are below freezing, so any dat below freezing with be considered missing data
#making another column to indicate that I am doing a QA/QC as to not overwrite 
#data and get confused

#Using the ifelse function, the first argument is a logical statement to be 
#evaluated as true or false on a vector, the second argument is the value that 
#my air.tempQ1 column will be given if the statement is true. The last value is 
#the value that will be given to air.tempQ1 if the statement is false. In this 
#case it is just given the air temperature value

datW$air.tempQ1<- ifelse(datW$air.temperature <0, NA, datW$air.temperature)

#checking range of extreme values----
# this is to check if they are realistic
#0%= min 50%= median 100%-max
quantile(datW$air.tempQ1)


#looking at the days with really low air temp
datW[datW$air.tempQ1<8,]

#looking at the days with really high air temp
datW[datW$air.tempQ1>33,]

#pinpointing measurements that may be unreliable----

#heavy rain and strong wind affect reliability of wind and air temp

#plotting precip and air temp on same plot
#normalize lightning strikes to match precip

lightscale<- (max(datW$precipitation)/max(datW$lightning.acvitivy))* 
  datW$lightning.acvitivy

#make plot with precip and lightning marked
#make it empty to start and then add in features 
plot(datW$DD, datW$precipitation, xlab = "Day of the Year", 
     ylab = "Precip and Lightning", type = "n")

#plot precip points only when there is  precip
#points are semi-transparent

points(datW$DD[datW$precipitation>0], datW$precipitation[datW$precipitation>0],
       col=rgb(95/255,158/255,160/255,.5), pch=15)

#plotting points only when there is lightning

points(datW$DD[lightscale>0], lightscale[lightscale>0], col="tomato3", pch=19)

#question 5----
assert(length(lightscale)==length(datW$precipitation), "Error: unequal length")

#Filtering out storms in wind and air temp measurements----
#filtering out values with lightning that coincide with rainfall greater than 
#2mm or only rainfall over 5mm

#new air temp column
datW$air.tempQ2<- ifelse(datW$precipitation>=2 & datW$lightning.acvitivy>0, NA,
                         ifelse(datW$precipitation>5, NA, datW$air.tempQ1))

#question 6----

#taking out values where wind speed could have been effected by storms with excess
#precip 
datW$wind.speedQ1<- ifelse(datW$precipitation>=2 & datW$lightning.acvitivy>0, NA,
                           ifelse(datW$precipitation>5, NA, datW$wind.speed))
#assert function to see if the values with precip greater than 2 and lightning 
#activity greater than 2 were taken out
#data in wind speed Q1 falsely matches up with any data that has precip >2 and 
#lightning activity>0, so it is a positive false statement to see if our data
#made the right distinctions
assert(datW$wind.speedQ1==(datW$precipitation>2 & datW$lightning.acvitivy>0),
       "Data does not have precip greater than 2 or lightning greater than 0")

#Question 7: Checking Soil moisture and temp for invalid data----

#mid July weather station was tampered with, soil moisture and temp sensor were
#out of commission

#potting precip to see when it rained to compare with soil moisture
#plot with precip and air temp marked
#make it empty to start and then add in features 
plot(datW$DD, ylim = c(0,25), xlab = "Day of the Year", xlim = c(182,195), 
     ylab = "Precip and Air Temp", type = "n")

#plot precip points only when there is precip
#points are semi-transparent

points(datW$DD[datW$precipitation>0], datW$precipitation[datW$precipitation>0],
       col=rgb(95/255,158/255,160/255,.5), pch=15)


#plot air temp
points(datW$DD, datW$air.tempQ1,
       col="tomato3", pch=15)

#plotting soil temp and moisture


#plot with filled in points
#line lines
#soil moisture
plot(datW$doy, datW$soil.moisture, pch=19, type = "b", xlim = c(182,195),
     xlab = "Day of the Year",
     ylab = "Soil Moisture (cm3 water per cm3 soil")

#soil temp
plot(datW$doy, datW$soil.temp, pch=19, type = "b",xlim = c(182,195), 
     xlab = "Day of the Year",
     ylab = "Soil Temp (C)")

#Question 8: Avg and Total table with decimal consideration----

#average air temp, wind speed, soil moisture, and soil temp to appropriate decimals
airtemp.avg<-format(round((mean(datW$air.tempQ2, na.rm=TRUE)), 1), nsmall=1)
windspeed.avg<-format(round((mean(datW$wind.speedQ1, na.rm=TRUE)), 2), nsmall=2)
soilmoist.avg<-format(round((mean(datW$soil.moisture, na.rm=TRUE)), 7), nsmall=7)
soiltemp.avg<-format(round((mean(datW$soil.temp, na.rm=TRUE)), 2), nsmall=2)

#total precip to 3 decimals
precip.total<-format(round((sum(datW$precipitation)),3), nsmall=3)

#making table of results
datW.results<-data.frame(
  "Sensor and Calculation"=c("Avg. Air Temp", "Avg. Wind Speed", 
                             "Avg. Soil Moisture", "Avg. Soil Temp", 
                             "Total Precipitation"),
  "Results"=c(airtemp.avg, windspeed.avg, soilmoist.avg, soiltemp.avg,precip.total))

#determining how many observations there were

#soil moisture total observations (obs)
length(datW$soil.moisture)

#of Na's
length(which(is.na(datW$soil.moisture)))

#soil temp total obs
length(datW$soil.temp)

#of NA's
length(which(is.na(datW$soil.temp)))

#air temp total obs
length(datW$air.tempQ2)

#of the NA's
length(which(is.na(datW$air.tempQ2)))
#there are 13 NA in observations

#wind speed total obs
length(datW$wind.speedQ1)

#of NA's
length(which(is.na(datW$wind.speedQ1)))
#13 NA in observations

#Question 9: Trends in Data
#Plotting Soil Moisture, Air&Soil Temp, and precip 

#soil moisture ***REVISE TO REFLECT NA FROM Q7
plot(datW$DD, datW$soil.moisture, pch=19, type = "b", xlab = "Day of the Year",
     ylab = "Soil Moisture (cm3 water per cm3 soil", 
     main = "Soil Moisture for June-July 2018")

#air temp
plot(datW$DD, datW$air.tempQ2, pch=19, type = "b", xlab = "Day of the Year",
     ylab = "Air Temp (C)"
     ,main = "Air Temperature for June-July 2018")

#hist of air temp
hist(datW$air.tempQ2,
     freq=FALSE,
     main = "Air Temperature for June-July 2018",
     xlab = "Air Temp (C)"
     ,ylab="Relative frequency",
     col="grey50",
     border="white")

#soil temp***REVISE TO REFLECT NA FROM Q7
plot(datW$DD, datW$soil.temp, pch=19, type = "b", xlab = "Day of the Year",
     ylab = "Soil Temp (C)", main = "Soil Temperature for June-July 2018")

#hist of soil temp
hist(datW$soil.temp,
     freq=FALSE,
     main = "Soil Temperature for June-July 2018",
     xlab = "Soil Temp (C)"
     ,ylab="Relative frequency",
     col="grey50",
     border="white")

#Precip 
plot(datW$DD, datW$precipitation, pch=19, type = "b", xlab = "Day of the Year",
     ylab = "Precip (mm/hr)",main = "Precipitation for June-July 2018")

#hist of precip
hist(datW$precipitation,
     freq=FALSE,
     main = "Precipitation for June-July 2018",
     xlab = "Precip (mm/hr)"
     ,ylab="Relative frequency",
     col="grey50",
     border="white")


