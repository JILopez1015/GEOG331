#Activity 22 Weather Data Analysis
#JL 2/1/2022


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

