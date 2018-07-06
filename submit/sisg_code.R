install.packages("gee") 
install.packages("multcomp") 
install.packages("lmtest")

library(gee)
library(multcomp)
library(lmtest)

cholesterol = read.csv("https://raw.githubusercontent.com/rhubb/SISG2018/master/data/SISG-Data-cholesterol.csv", header=T)
str(cholesterol)

