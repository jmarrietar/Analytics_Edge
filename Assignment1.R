################
#QUICK QUESTION#  
################
setwd("/Users/josemiguelarrieta/Documents/Analytics_Edge")

sd(c(5,8,12))
which.min(c(4,1,6))
Sys.setlocale("LC_ALL", "C")

WHO<-read.csv("WHO.csv")

which.min(WHO$Over60)
WHO[183,]

which.max(WHO$LiteracyRate)
WHO[44,]

#tapply 
#So tapply splits the data by the second argument you give,
#and then applies the third argument function
#to the variable given as the first argument.

tapply(WHO$Over60,WHO$Region,mean)

#Which region has the lowest average child mortality rate across all countries in that region?
tapply(WHO$ChildMortality,WHO$Region,mean)

################
#ASSIGNMENT 1### 
################

