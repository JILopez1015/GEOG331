
#####    Final Project #####
#####    4/1/2021      #####

####     loading packages ####
library(dplyr)
library(tidyverse)
library(ggplot2)


#####    Loading in Temp data #####

##metadata link https://ag.arizona.edu/azmet/raw2003.htm

T17 <- read.csv(url("https://ag.arizona.edu/azmet/data/0617rh.txt"), 
                   header = FALSE)

T18 <- read.csv(url("https://ag.arizona.edu/azmet/data/0618rh.txt"),
                    header=FALSE)

T19 <- read.csv(url("https://ag.arizona.edu/azmet/data/0619rh.txt"),
                header=FALSE)

T20 <- read.csv(url("https://ag.arizona.edu/azmet/data/0620rh.txt"),
                header=FALSE)

T21 <- read.csv(url("https://azmet.arizona.edu/azmet/data/0621rh.txt"),
                header=FALSE)

#####   Formatting Raw datT   #####
 
datT<-dataT %>% seperate(X2017.1.6.13.6.9.3.11.8.92.7.69.2.83.5..23.7.52.1.78.14.5.12.5.13.6.14.8.14.7.14.8.3.5.2.6.188.43.10.9..2.1.1.2.1.16.9)


