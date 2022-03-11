### Activity #4
##JL 3/10/2022


#use built in iris dataset
#take a look at it 
head(iris)
#install and load in some tidyverse packages
install.packages(tidyverse)
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships

View(iris)


#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# hint: consider using a list, and also new vectors for regression variables
#just do this 3 times in for loop
lm(data= iris.versi, formula= Sepal.Length~Sepal.Width)

##extracting versicolor
iris.versi<-iris %>% filter(Species=="versicolor")
###help online: https://statisticsglobe.com/r-multiple-regressions-in-for-loop
#create lists
a<-list(iris.versi$Sepal.Length)
b<-list(iris.versi$Sepal.Width)
c<-list(iris.versi$Petal.Length)
d<-list(iris.versi$Petal.Width)

#empty list
versi.lists<-list()
#making loop for making lists
for(i in 1:ncol(iris.versi)){
  versi.lists[[i]]<- iris.versi[,i]}

#list is versi.lists

#creating regression loop
  
for (r in 1:3) {iris.summ[r]<- lm(iris)
  
  
}
  
#lm(formula = versi.lists[1]~versi.lists[2])
  

#####################################
##### Part 2: data in dplyr- Answered#####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

#joining height table to the iris table to make new data frame
iris.height<-full_join(iris, height, by= "Species")
#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot

ggplot(iris, aes(x = Sepal.Length, y= Sepal.Width))+ geom_point()

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(iris, aes(x = Sepal.Length, y= Sepal.Width))+ geom_point()+theme_classic()


#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length

iris.3c<- ggplot(data=iris, aes(x = Sepal.Length, y = Sepal.Width)) 
iris.3c + geom_point(aes(color=Species, shape=Species, size=Petal.Length)) +
  xlab("Sepal Length") +  ylab("Sepal Width") +
  ggtitle("Relationship for Sepal Length-Width by Species: Proportional to Petal Length")+ 
  theme_classic()



#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		
#Answer In GDoc 
  
 






 
  
####trash code
  #creating empty list
  #versi.lists<-list()
  
  #making loop for making lists
  #for(i in 1:ncol(iris.versi)){
  #  versi.lists[[i]]<- iris.versi[,i]}
  
  #making loop for making lists
  #for(i in 1:ncol(iris.versi)){
  #versi.lists[[i]]<- summary(lm(data=versi.lists, formula =() ))
  
  #instructions
  #https://intro2r.com/loops.html
  
  #making loop for regressions
  #for(i in 1:(ncol(versi.lists)){versilm[[i]]<-lm()}
  
  