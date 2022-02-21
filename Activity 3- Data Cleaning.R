#Activity 3 Questions
#JL 2/21/22----


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
























































