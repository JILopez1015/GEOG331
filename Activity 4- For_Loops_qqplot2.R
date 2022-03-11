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

##extracting versicolor
iris.versi<-iris %>% filter(Species=="versicolor")

#making lists for x and y variables of regressions
##just saying which columns: 1,3,1
versi.x<- c(1,3,1)

##just saying which columns: 2,4,3
versi.y<- c(2,4,3)

#defining versi.summ as list with 0 
versi.summ<-list(0)
  
#making loop for regression
#made reference with which columns to use (versi.x+.y) but added the data frame to 
#reference at the beginning so it can reference specific columns I want

for (i in 1:3) { versi.summ[[i]]<- 
  lm(iris.versi[,versi.y[i]]~iris.versi[,versi.x[i]])  #using the comma before 
}                                     #versi.x[i] to say that I want to compare 
                                      #those columns, not the same as subsetting
  

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
  
 





  
  