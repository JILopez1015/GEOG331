+install.packages("ggplot2")
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
#
#
#
#
