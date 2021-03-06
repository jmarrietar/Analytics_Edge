################
#QUICK QUESTION#  
################
setwd("/Users/josemiguelarrieta/Documents/Analytics_Edge")

sd(c(5,8,12))
which.min(c(4,1,6))
Sys.setlocale("LC_ALL", "C")

WHO<-read.csv("Data/WHO.csv")

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

#######################
#Analytical Detective## 
#######################

#This problem is focus on "GTA CRIME"-> This is the act of stealing, or attempting to steal, a car. 

# DataFrame Columns Description. 

# D: a unique identifier for each observation
# Date: the date the crime occurred
# LocationDescription: the location where the crime occurred
# Arrest: whether or not an arrest was made for the crime (TRUE if an arrest was made, and FALSE if an arrest was not made)
# Domestic: whether or not the crime was a domestic crime, meaning that it was committed against a family member (TRUE if it was domestic, and FALSE if it was not domestic)
# Beat: the area, or "beat" in which the crime occurred. This is the smallest regional division defined by the Chicago police department.
# District: the police district in which the crime occured. Each district is composed of many beats, and are defined by the Chicago Police Department.
# CommunityArea: the community area in which the crime occurred. Since the 1920s, Chicago has been divided into what are called "community areas", of which there are now 77. The community areas were devised in an attempt to create socially homogeneous regions.
# Year: the year in which the crime occurred.
# Latitude: the latitude of the location at which the crime occurred.
# Longitude: the longitude of the location at which the crime occurred.



############
#Load Data##
############

mvt<-read.csv("Data/mvtWeek1.csv")

nrow(mvt)
mvt$ID[which.max(mvt$ID)]
max(mvt$ID)
min(mvt$Beat)
summary(mvt$Arrest)
summary(mvt$LocationDescription)

############
#Dates in R#
############

#Characters to to Date Objects. 
DateConvert=as.Date(strptime(mvt$Date,"%m/%d/%y %H:%M"))

summary(DateConvert)

#Extract Month and Day from Dates. 
mvt$Month=months(DateConvert)
mvt$Weekday=weekdays(DateConvert)

#Replace old Date variable with DateConvert

mvt$Date=DateConvert

#In which month did the fewest motor vehicle thefts occur?
min(table(mvt$Month))

#On which weekday did the most motor vehicle thefts occur?
max(table(mvt$Weekday))

#Which month has the largest number of motor vehicle thefts for which an arrest was made?
table(mvt$Month,mvt$Arrest)

##########################
#VISUALIZING CRIME TRENDS#
##########################

#Histogram of the variable Date

hist(mvt$Date, breaks=100)

#let's see how arrests have changed over time. 

# boxplot of the variable "Date", sorted by the variable "Arrest"
boxplot(mvt$Date ~ mvt$Arrest)
#For what proportion of motor vehicle thefts in 2001 was an arrest made?
require(lubridate)
mvt$Year=year(DateConvert)

table(mvt$Year[mvt$Year==2001],mvt$Arrest[mvt$Year==2001])

table(mvt$Arrest, mvt$Year)

####################
#POPULAR LOCATIONS##  
####################

#Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category? 
sort(table(mvt$LocationDescription))

Top5<-subset(mvt,mvt$LocationDescription=="STREET"|mvt$LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" 
             |mvt$LocationDescription=="ALLEY"|mvt$LocationDescription=="GAS STATION"|mvt$LocationDescription=="DRIVEWAY - RESIDENTIAL")


#Another way 
TopLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")
Top5 = subset(mvt, LocationDescription %in% TopLocations)

table(Top5$LocationDescription) #Have a lot of unnecesary Variables, is needed it to refresh factor Variable. 

Top5$LocationDescription=factor(Top5$LocationDescription) #Now only have 5 variables. 
table(Top5$LocationDescription) 

#Which of these Top5 locations has the higher arrest Rate?. 
table(Top5$Arrest, Top5$LocationDescription)

#On which day of the week do the most motor vehicle thefts at gas stations happen?
table(Top5$LocationDescription=="GAS STATION", Top5$Weekday)

table(Top5$LocationDescription, Top5$Weekday)





