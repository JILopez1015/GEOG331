#Participation Activity in Class
#JIL 2/28/2022 1:21


#assigning working database
setwd("Z:/students/jlopez/GitHub/")

iris

#subset for Iris Virginica
flower<- iris[iris$Species == "virginica",]

#linear model relating petal length to sepal length
fit<- lm(flower$Petal.Length~flower$Sepal.Length)

#view results
summary(fit)

#creating scatterplot, comes first before linear model to see if the relationship
  #is even linear
plot(flower$Sepal.Length, flower$Petal.Length,
     main="Iris Virginica",
     xlab= "Sepal Length",
     ylab= "Petal Length",
     col= "purple", pch=16)

#does seem to have some kind of relationship based on plot

#residual obj
residuals<-summary(fit)$residuals

#plot residuals, stores in regressions summary
plot(flower$Sepal.Length, residuals,
     xlab = "Sepal Length",
     ylab = "Residuals",
     col= "purple",
     pch=16)

#add a horizontal line to reference
abline(h=0, lty= "dashed")

#Histogram of residuals
hist(residuals,
     main= "Regression Residuals",
     xlab = "Residuals",
     col= "purple")

#shapiro wilks test
shapiro.test(residuals)

#qq plot
qqnorm(residuals, pch=16)

qqline(residuals, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7, pch=16)
















