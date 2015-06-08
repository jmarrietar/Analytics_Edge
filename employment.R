setwd("/Users/josemiguelarrieta/Documents/Analytics_Edge/")

CPS<-read.csv("Data/CPSData.csv")


str(CPS)

#what is the most common industry of employment? 
max((table(CPS$Industry)))
summary(CPS)

sort(table(CPS$State)) 

#What proportion of interviewees are citizens of the United States?
summary(CPS$Citizenship)

#For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity? 
(table(CPS$Race,CPS$Hispanic)) 

#Which variables have at least one interviewee with a missing (NA) value?
summary(CPS)

################
#MISSING VALUES#
################

is.na(CPS$Married)
#We can see the breakdown of whether Married is missing based on the reported value of the Region variable with the function 
table(CPS$Region, is.na(CPS$Married))

table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))

#How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)
table(is.na(CPS$MetroAreaCode),CPS$State)

#Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
table(is.na(CPS$MetroAreaCode),CPS$Region)

#Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
sort(tapply(is.na(CPS$MetroAreaCode),CPS$State,mean))

####################################
#INTEGRATING METROPOLITAN AREA DATA#
####################################

MetroAreaMap<-read.csv("Data/MetroAreaCodes.csv")
CountryMap<-read.csv("Data/CountryCodes.csv")

#######
#MERGE#
#######
#The following command merges the two data frames on these columns, 
#overwriting the CPS data frame with the result:

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

#How many interviewees have a missing value for the new metropolitan area variable? 
#Note that all of these interviewees would have been removed from the merged data frame 
#if we did not include the all.x=TRUE parameter.
sort(table(CPS$MetroArea))
summary(CPS)

#Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity? 
sort(tapply(CPS$Hispanic,CPS$MetroArea,mean))

#Determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian.
sort(tapply(CPS$Race=="Asian",CPS$MetroArea,mean))

#Determine which metropolitan area has the smallest proportion of interviewees who have received no high school diploma.
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE))

###################################
#INTEGRATING COUNTRY OF BIRTH DATA#  
###################################

CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
summary(CPS)

#What proportion of the interviewees from the "New York-Northern New Jersey-Long Island, NY-NJ-PA" 
#metropolitan area have a country of birth that is not the United States?

table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")

#Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India?
(table(CPS$MetroArea , CPS$Country == "India"))
table(CPS$MetroArea , CPS$Country == "Brazil")
table(CPS$MetroArea , CPS$Country == "Somalia")