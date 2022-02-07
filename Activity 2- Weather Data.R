#Activity 22 Weather Data Analysis
#JL 2/7/2022

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
Q2<- c("Thi$", "is", "a", "character", "example.")
Q2Q2
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
#par(mfrow=c(2,2))

######Livermore Avg Daily Temp
h2<-hist(datW$TAVE[datW$siteN == 2],
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
h3 <-hist(datW$TAVE[datW$siteN == 3],
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
h4<-hist(datW$TAVE[datW$siteN == 4],
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

h1 <-hist(datW$TAVE[datW$siteN==1],
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
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN
       == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)


##plotting distribution

###the seq function generates a sequence of numbers that we can use to plot the 
    #normal across the range of temperature values
x.plot <- seq(-10,30, length.out =100)

##For h1
#the dnorm function will produce the probability density based on a mean and 
    #standard deviation.
y.plot <- dnorm(seq(-10,30,length.out =100),
mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
y.plot


#create a density that is scaled to fit in the plot since the density has a 
#     different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot 
    #is always the same between the two datasets on the plot. Here both plots 
    #share zero as a minimum.
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot
y.scaled

#points function adds points or lines to a graph
#the first two arguements are the x coordinates and the y coordinates.
points(x.plot,
       y.scaled, 
       type ="l",
       col = "royalblue3",
       lwd =4,
       lty =2)

#--------------------------
##Q5: Not all locations hav a normal distribution. Livermore and Andan both have 
    #significantly higher values after mean. Normally distributed data would 
    #have lower values before and after mean. 

##Pnorm getting values BELOW X
#pnorm(value to evaluate at (note this will evaluate for all values and below),
  #mean, standard deviation)
#probability of getting below 0 temps
pnorm(0, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE), 
      sd(datW$TAVE[datW$siteN==1], na.rm=TRUE))

#probability of getting below 5 temps
pnorm(5, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE), 
      sd(datW$TAVE[datW$siteN==1], na.rm=TRUE))

#probability of getting between 0 and 5 temp; range from 0-5
(pnorm(5, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE), 
      sd(datW$TAVE[datW$siteN==1], na.rm=TRUE)))-
  (pnorm(0, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE),
         sd(datW$TAVE[datW$siteN==1], na.rm=TRUE)))

##Pnorm getting values ABOVE X
#getting probability of getting temps above 20
1- (pnorm(20, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE),
          sd(datW$TAVE[datW$siteN==1], na.rm=TRUE)))
##qnorm- getting values associated with the probability within curve
qnorm(.95, mean(datW$TAVE[datW$siteN==1], na.rm=TRUE),
                sd(datW$TAVE[datW$siteN==1], na.rm=TRUE))
#--------------------------
#Q6
qnorm(.95, ((mean(datW$TAVE[datW$siteN==1], na.rm=TRUE))+4),
      sd(datW$TAVE[datW$siteN==1], na.rm=TRUE))
##If mean increased by 4 degrees C, then the probability of getting extreme event
  #rises to 22.51% compared to 18.51% from before 4 degree increase

#--------------------------
#Q7
##making histogram for daily precipitation in Aberdeen, siteN=1
  #precipitation=PRCP

#precipitation data for site 1- Aberdeen, not averaged in any way
prcp.data1<-datW$PRCP[datW$siteN==1]

h1.p <-hist(prcp.data1,
          freq=FALSE,
          main= paste(levels(datW$NAME)[1]),
          xlab = "Daily Percipitation(mm)",
          ylab = "Relative Freq",
          col = "grey50",
          border = "white")
##The histogram for Aberdeen precipitation looks like the exponential distribution. 
  #The exponential distribution can start with a large number and drop very quickly. 

#--------------------------
#Q8

##Sum of each year and each station
prcpyrsum<-aggregate(datW$PRCP,list(datW$siteN,datW$NAME,datW$year), sum)

#labeling columns for prcpyrsum
#PRCPTBL<-setNames((prcpyrsum),c("St.#","St.Name","Year","Precipitation Sum"))
PRCPTBL<-colnames(prcpyrsum)<-c("St.#","St.Name","Year","Precipitation Sum")

##sum of each year for station 4
#AnnualPRCP4<-aggregate(PRCPTBL,by=list(PRCPTBL['St.#'=="4"],
        #PRCPTBL['St.Name'=="MORMON FLAT, AZ US"],PRCPTBL["Year"]),sum)

AnnualPRCP4<-getElement(prcpyrsum, "")
  
  prcpyrsum[prcpyrsum$"St.#"=="4"]




#WHAT CLASS?--Character
class(PRCPTBL["St.#"])

#making station # numeric
PRCPTBL$siteN<- as.numeric(PRCPTBL["St. #"])



##recalling only station 4, AZ, from prcpyrsum= AnnualPRCP4
#AnnualPRCP4<-prcpyrsum$`St. #`==4
###???returns back a lot of true and false???




##making histogram for site 4, annual precipitation
AnnualPrcpHist4<-hist(AnnualPRCP4,
          freq=FALSE,
          main= paste(levels(datW$NAME)[1]),
          xlab = "Annual Percipitation (mm)",
          ylab = "Relative Freq",
          col = "grey50",
          border = "white")

AnnualPrcpHist4<-hist(PRCPTBL$`St. #`[],
                      freq=FALSE,
                      main= paste(levels(datW$NAME)[1]),
                      xlab = "Annual Percipitation (mm)",
                      ylab = "Relative Freq",
                      col = "grey50",
                      border = "white")

datW$PRCPTBL[PRCPTBL$St. #==4]
                
datW$TAVE[datW$siteN==1]





























