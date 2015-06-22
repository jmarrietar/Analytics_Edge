pisaTrain<-read.csv("Data/pisa2009train.csv")
pisaTest<-read.csv("Data/pisa2009test.csv")

#what is the average reading test score of males?
tapply(pisaTrain$readingScore,pisaTrain$male,mean)

#########################
#REMOVING MISSING VALUES#
#########################
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

summary(pisaTrain)
str(pisaTrain)

#Change reference level for regression. 
#Reference level shoud be the most common factor variable.
#R take the first one alphabetically 

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

#Building a Prediction Model [Regression Model]
lmScore<-lm(readingScore ~ .,data=pisaTrain)
summary(lmScore)

#############################################
#training-set root-mean squared error (RMSE)#
#############################################
sqrt(mean((lmScore$residuals)^2))

#Alternative way to get RMSE 
#The training-set RMSE can be computed by first computing the SSE:
SSE = sum(lmScore$residuals^2)
#and then dividing by the number of observations and taking the square root:
RMSE = sqrt(SSE / nrow(pisaTrain))


#########
#PREDICT#
#########
predTest<-predict(lmScore,newdata=pisaTest)
summary(predTest)

SSE = sum((predTest - pisaTest$readingScore)^2)
RMSE = sqrt(SSE / nrow(pisaTest))


######################################
#BASELINE PREDICTION AND TEST-SET SSE#  
######################################
#What is the predicted test score used in the baseline model? 
baseline = mean(pisaTrain$readingScore)

#What is the sum of squared errors of the baseline model on the testing set?
#HINT: We call the sum of squared errors for the baseline model the total sum of squares (SST).
sum((baseline-pisaTest$readingScore)^2).


####################
#TEST-SET R-SQUARED#
####################

#What is the test-set R-squared value of lmScore?
SSE = sum((predTest - pisaTest$readingScore)^2)
SST = sum( (mean(pisaTrain$readingScore) - pisaTest$readingScore)^2)
R2 = 1 - SSE/SST

