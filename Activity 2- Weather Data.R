#Activity 22 Weather Data Analysis
#JL 2/1/2022

#set working directory to my noaa data folder
#start with net work drive and "\" tab to have auto fill
setwd("Z:/students/jlopez/HW/Activity 2/NOAA Weather Data/")

#see what my working directory is
getwd()

#read in weather station data from data folder

#PC file path

datW<- read.csv("Z:\\students\\jlopez\\HW\\Activity 2\\NOAA Weather Data\\2011124.csv", 
                stringsAsFactors=TRUE)

#get more info on dataframe

str(datW)
#--------------------------
#Q1: There are 9 columns, and 157849 rows

#specify a column with a proper date format
#note format here is dataframe$column

datW$dateF<- as.Date(datW$DATE, "%Y-%m-%d")

#CREATING date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data

datW$year<- as.numeric(format(datW$dateF, "%Y"))

#--------------------------
#Q2 Example of character, numeric, integer, factor data in example vectors with 
#5 objects each

#creating character 
Q2<- c("Thi$ is a character example.")
Q2
typeof(Q2)

#numeric example
num<- ((100*4)/2)-(4+5.65)
typeof(num)

#integer example
int<- c(2L, 4L, 5L, 1L)
typeof(int)

#creating income factor

class<- factor(c("upper","upper middle","middle","lower middle","lower"))

typeof(class)
attributes(class)
      #how R is storing income
unclass(class)

#--------------------------
#Descriptive stats and histograms

#Unique names of stations (METRIC Unit)
unique(datW$NAME)
##MORRISVILLE 6 SW, NY\ LIVERMORE, CA\ ABERDEEN, WA\ MORMON FLAT, AZ              
    #MANDAN EXPERIMENT STATION, ND 

#avg max temp for Aberdeen
mean(datW$TMAX[datW$NAME =="ABERDEEN, WA US"])

#avg max temp for Aberdeen, controlling for missing data by setting na.rm to 
      #true to ignore NA
mean(datW$TMAX[datW$NAME =="ABERDEEN, WA US"], na.rm=TRUE)

#calc avg daily temp TAVE
datW$TAVE<- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)


#getting the mean across all sites, FUN indicates function we want to use
#by function is a list of 1 or more variables to index over

averageTemp<- aggregate(datW$TAVE, by= list(datW$NAME), FUN="mean", na.rm=TRUE)


#change the automatic column names to be more meaningful
#note that MAAT is a common abbreviation for Mean Annual Air Temperature

colnames(averageTemp)<-c("NAME", "MAAT")


#convert level to number for factor data type
#will have to reference the level output or look at the row of data to see
    #the character designation.

datW$siteN<- as.numeric(datW$NAME)



#make a histogram for the first site in our levels
#main= is the title name argument.
#paste the actual name of the factor not the numeric index
#since that will be more meaningful.

hist(datW$TAVE[datW$siteN==1],
     freq=FALSE,
     main= paste(levels(datW$NAME)[1]),
     xlab = "Average Daily Temperature (C)",
     ylab = "Relative Freq",
     col = "grey50",
     border = "white")

#--------------------------
#Q3, explanation on GDoc
?hist

#--------------------------

#add mean line with red (tomato3) color
#and thickness of 3
#abline function allows us to add lines to a plot. The v argument in this 
    #function means add a vertical line.

abline(v=mean(datW$TAVE[datW$siteN==1],na.rm=TRUE),
       col="tomato3",
       lwd=3)

#add standard deviation line below the mean with red color
#and thickness of 3

abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN
       == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#--------------------------
#Q4 making 3 other histograms
#combining hist
par(mfrow=c(2,2))

######Livermore Avg Daily Temp
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="darksalmon",
     border="white")

#add mean line with grey (grey33) color
#and thickness of 3
#abline function allows us to add lines to a plot. The v argument in this 
#function means add a vertical line.

abline(v=mean(datW$TAVE[datW$siteN==2],na.rm=TRUE),
       col="grey33",
       lwd=3)

#add standard deviation line below the mean with grey (grey33)
#and thickness of 3

abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN
       == 2],na.rm=TRUE), 
       col = "grey33", 
       lty = 3,
       lwd = 3)

#add standard deviation line above the mean with grey color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN
       == 2],na.rm=TRUE), 
       col = "grey33", 
       lty = 3,
       lwd = 3)


###### Mandan Exp. Station Avg Daily Temp
hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="deepskyblue2",
     border="white")

#add mean line with grey (grey33) color
#and thickness of 3
#abline function allows us to add lines to a plot. The v argument in this 
#function means add a vertical line.

abline(v=mean(datW$TAVE[datW$siteN==3],na.rm=TRUE),
       col="grey33",
       lwd=3)

#add standard deviation line below the mean with grey (grey33)
#and thickness of 3

abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN
       == 3],na.rm=TRUE), 
       col = "grey33", 
       lty = 3,
       lwd = 3)

#add standard deviation line above the mean with grey color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN
       == 3],na.rm=TRUE), 
       col = "grey33", 
       lty = 3,
       lwd = 3)

###### Mormon Flat Avg Daily Temp
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="forestgreen",
     border="white")

#add mean line with orange (goldenrod3) color
#and thickness of 3
#abline function allows us to add lines to a plot. The v argument in this 
#function means add a vertical line.

abline(v=mean(datW$TAVE[datW$siteN==4],na.rm=TRUE),
       col="goldenrod3",
       lwd=3)

#add standard deviation line below the mean with orange (goldenrod3)
#and thickness of 3

abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN
       == 4],na.rm=TRUE), 
       col = "goldenrod3", 
       lty = 3,
       lwd = 3)

#add standard deviation line above the mean with orange color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN
       == 4],na.rm=TRUE), 
       col = "goldenrod3", 
       lty = 3,
       lwd = 3)

######Aberdeen Avg Daily Temp

hist(datW$TAVE[datW$siteN==1],
     freq=FALSE,
     main= paste(levels(datW$NAME)[1]),
     xlab = "Average Daily Temperature (C)",
     ylab = "Relative Freq",
     col = "grey50",
     border = "white")

#add mean line with red (tomato3) color
#and thickness of 3
#abline function allows us to add lines to a plot. The v argument in this 
#function means add a vertical line.

abline(v=mean(datW$TAVE[datW$siteN==1],na.rm=TRUE),
       col="tomato3",
       lwd=3)

#add standard deviation line below the mean with red color
#and thickness of 3

abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN
                                                                      == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)









