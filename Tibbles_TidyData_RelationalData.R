#HW Tibbles, Tidy Data, Relational Data
#JIL 3/1

library(tidyverse)

#creating tibbles----

as_tibble(iris)

tibble(x= 1:5,
       y=1,
       z=x^2+y)
#does not have to be syntactic names as long as they are inside `x`
tb <- tibble(
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)

tb

#transposed tribble

tribble(
  ~x, ~y, ~z,
  #--|--|----#
  "a", 2, 3.6,
  "b", 1, 8.5)

#tibbles vs data.frame

#printing of data type 
tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE))

#explicitly printing from data.frame

nycflights13::flights %>% 
  print(n = 10, width = Inf)

library(nycflights13)


#10.3.2 sUBSETTING

df <- tibble(
  x = runif(5),
  y = rnorm(5))

#EXTRACT BY NAME
df$x
df[["x"]] #different from df["x"]

#extract by position
df[[1]]

df %>% .$x

df %>% .[["x"]]


#interacting with older code

#turning a tibble into a data.frame
class(as.data.frame(tb))

#exercises

#telling if it is a tibble

mtcars  #(regular data.frame)
mtcars1<-class(as_tibble(mtcars))

#2, comparing df and t
df

# A tibble: 5 x 2
#   x        y
#  <dbl>  <dbl>
#1 0.380 -0.877  
#2 0.951 -1.30   
#3 0.907 -0.00908
#4 0.338 -0.797  
#5 0.142 -1.09

df %>% .$x

df %>% .[["x"]]

df1 <- data.frame(df)
df1$x
df1[, "xyz"]
df1[, c("abc", "xyz")]
#needs to have defined columns

#5- enframe converts atomic vectors or lists to one or two data.frames
#tibble::enframe()

#Tidy Data----

#only table 1 is tidy because it has each variable in its own column, each 
#observation in its own row,each value has its own cell

table1
table2
table3
table4a
table4b

#compute rate per 10,000
table1 %>% mutate(rate = cases / population * 10000)

#compute cases by year
table1 %>% 
  count(year, wt = cases)

#Visualise changes over time
library(ggplot2)
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

###rate for table 2
as_tibble(table2)

new.t2<-spread(table2,type,count)

#compute rate per 10,000
new.t2 %>% mutate(rate = cases / population * 10000)

#compute cases by year
new.t2 %>% 
  count(year, wt = cases)

#Visualise changes over time
library(ggplot2)
ggplot(new.t2, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))


###table4a Gather and Pivot_longer----

table4a
new.t4a<-gather(table4a, "1999", "2000",key= "year", value="cases")

#another method
pivot_longer(c(`1999`,`2000`), names_to = "year", values_to = "cases")


###table4b
table4b
new.t4b<-gather(table4b, "1999", "2000",key= "year", value="population")
#another method
pivot_longer(c(`1999`,`2000`), names_to = "year", values_to = "population")

#complete table 4, joining different table columns ----
#where the new column will be added %>% add_column(name of new column
#=data from column being added)
new.t4<- new.t4a %>% add_column(population=new.t4b$population)

#another method
left_join(new.t4a, new.t4b)


#Pivot_Wider Table 2----
table2
#where do we take values from(count) where do we take variables from (type)
table2 %>% pivot_wider(names_from= type,values_from=count)

#tidying data
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)

preg.2<-preg %>% pivot_longer(c("male", "female"), names_to = "sex", 
                              values_to= "Total")

#table 3 sep= where the separation should occur, convert=true is good when
#vectors are numbers, integers
table3
new.t3<-table3 %>% separate(rate, into=c("cases", "population"), 
                            sep = "/", convert = TRUE)

#Table 5
table5
#separating columns----
new.t5<-table5 %>% separate(rate, into=c("cases", "population"), 
                            sep = "/", convert = TRUE)

#uniting 2 columns sep= "" means we do not want anything to separate the values----
new.t5a<- new.t5 %>% unite("Year",c("century", "year"), remove = TRUE, sep = "")


#application of above skills----

view(who)

#making all columns into observations/values
who1<- who %>% pivot_longer(
  cols = new_sp_m014:newrel_f65,
  names_to = "key",
  values_to = "cases",
  values_drop_na = TRUE)

#Counting what is in each row
who1 %>% count(key)

#what the key means
#new= new cases
#new_##_S^^= ##= what type of TB s=sex ^^=age group

#changing all keys with newrel to new_rel
who2<- who1 %>% mutate(key=stringr::str_replace(key, "newrel","new_rel"))

#separating values from key
who3<- who2 %>% separate(key, c("new", "type", "sexage"), sep= "_", remove=TRUE)

#dropping new since they are all new
who4<- who3 %>% select(-new, -iso2, -iso3)

#Separating sex and age
who5<- who4 %>% separate(sexage,c("sex","age"), sep = 1)

###complex pipe of code for above procedures
#%>% used to seperate functions

master_who <- who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)














