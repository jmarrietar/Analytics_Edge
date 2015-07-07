Claims<-read.csv("Data/ClaimsData.csv")
spl=sample.split(Claims$bucket2009,SplitRatio=0.6)
ClaimsTrain=subset(Claims,spl=TRUE)
ClaimsTest=subset(Claims,spl=FALSE)

mean(ClaimsTrain$age)
table(Claims$diabetes)

#So let's create a classification matrix to compute the accuracy
#for the baseline method on the test set.
table(ClaimsTest$bucket2009,ClaimsTest$bucket2008)


################
#Penalty Matrix#
################
PenaltyMatrix=matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow=TRUE,nrow=5)

#Multiply matrix by penalty Matrix. 
sum(as.matrix(table(ClaimsTest$bucket2009,ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)
table(ClaimsTest$bucket2009)


