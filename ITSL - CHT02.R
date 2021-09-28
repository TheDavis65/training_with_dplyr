pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder", 
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr", 
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR2", "MASS")



x <- 1:10
y <- 10:-10
z <- 1

length(x)
length(y)
length(z)

rm(y)

ls()
rm(list=ls())                  

x <- rnorm (50)
y <- x + rnorm (50, mean = 50, sd = .1)
cor(x, y)

set.seed (1303)
rnorm (50)

set.seed (3)
y <- rnorm (100)
mean(y)
var(y)
sqrt(var(y))
sd(y)


x <- rnorm (100)
y <- rnorm (100)
plot(x, y)
plot(x, y, xlab = "this is the x-axis",
       ylab = "this is the y-axis",
       main = "Plot of X vs Y")

pdf("Figure.pdf")
plot(x, y, col = "green")


Auto <- read.table("data/Auto.data")
glimpse(Auto)

Auto <- read.table("data/Auto.data", header = T, na.strings = "?",
                   stringsAsFactors = T)
glimpse(Auto)
str(Auto)

dim(Auto)

Auto <- na.omit(Auto)
dim(Auto)

names(Auto)


plot(cylinders, mpg)

plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)


str(cylinders)
cylinders <- as.factor(cylinders)
str(cylinders)

xxx <- Auto
xxx$cylinders <- as.factor(xxx$cylinders)
str(xxx)

rm(xxx)

plot(cylinders , mpg)
plot(cylinders , mpg , col = "red")
plot(cylinders , mpg , col = "red", varwidth = T)
plot(cylinders , mpg , col = "red", varwidth = T,
     horizontal = T)
plot(cylinders , mpg , col = "red", varwidth = T,
       xlab = "cylinders", ylab = "MPG")


hist(mpg)
hist(mpg , col = 2)
hist(mpg , col = 2, breaks = 15)
pairs(Auto)
pairs( ~ mpg + displacement + horsepower + weight + acceleration, data = Auto)

plot(horsepower , mpg)
identify(horsepower , mpg , name)


