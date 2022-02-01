install.packages("ggplot2")
library("ggplot2")
qplot
x<-c(-1, -.5, 0, .5, 1)
Y<-x^3
qplot(x,Y)
x<-c(1,2,2,2,3,3)
qplot(x, binwidth=1)
x2<-c(1,1,1,1,1,2,2,2,2,3,3,4)
qplot(x2, binwidth=1)
x3<-c(0,1,1,2,2,2,3,3,4)
qplot(x3, binwidth=1)
#above is practice examples for chpt 3
#will do die now
die<-1:6
roll<-function(){die<-1:6
dice<-sample(die,size=2, replace=2)
sum(dice)
}
#chpt3 examples
rolls<-replicate(10000, roll())
qplot(rolls,binwidth=1)
#histogram of random rolls of dice
#accesing help pages: ?"name of function, no quotations"
#weighing 6 higher in prob
roll<-function(){die<-1:6
dice<-sample(die,size=2, replace=TRUE,
prob=c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
sum(dice)
}
rolls<-replicate(10000, roll())
qplot(rolls, binwidth=1)


#chpt 5 examples below





hand<-c("ace", "king", "queen", "jack", "ten")
suit<-c("spades","spades","spades","spades","spades")
hand1<-c(hand, suit)
matrix(hand1, nrow=5, ncol=2)



#pg17 activity 5.5


card<- list("ace","hearts", 1)
card



#creating data frame for clubs playing card (clubs.pc)

clubs.pc<- data.frame(face= c("ace", "two", "six"), 
                      suit= c("clubs", "clubs", "clubs"), value= c(1:3))


#creating deck
#loading file for card deck
# file location within PC Z:\\students\\jlopez\\HW\\CHpt 5\\deck

deck <- read.csv("Z:/students/jlopez/HW/CHpt 5/deck.csv")
View(deck)

#tocheck if it worked 
head(deck)


#saving as csv

write.csv(deck, file= "cards.csv", row.names=FALSE)



