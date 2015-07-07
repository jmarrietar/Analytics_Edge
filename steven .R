stevens<-read.csv("Data/stevens.csv")
set.seed(3000)

install.packages("caTools") 
library(caTools) 

spl=sample.split(stevens$Reverse,SplitRatio=0.7)

Train<-subset(stevens,spl==TRUE)
Test<-subset(stevens,spl==FALSE)

install.packages("rpart") 
install.packages("rpart.plot") 
library(rpart)
library(rpart.plot)
StevensTree=rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=Train,method="class",minbucket=25)

prp(StevensTree)

#Predictions 

PredictCART= predict (StevensTree,newdata=Test,type="class")

###################
#Consussion Matrix#
###################

table(Test$Reverse,PredictCART)

###########################
#ROC Curve! and Prediction#
###########################

install.packages("ROCR") 
library(ROCR)

predictROC=predict(StevensTree,newdata=Test)
pred=prediction(predictROC[,2],Test$Reverse)
perf=performance(pred,"tpr","fpr")
plot(perf)

#AUC of the CART model 

as.numeric(performance(pred, "auc")@y.values)

################
#Random Forests#
################
install.packages("randomForest")
library("randomForest")

Train$Reverse<-as.factor(Train$Reverse)
Test$Reverse<-as.factor(Test$Reverse)
StevensForest=randomForest(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=Train,nodesize=25,ntree=200,na.action = na.omit)

PredictForest=predict(StevensForest,newdata=Test)
table(Test$Reverse,PredictForest)

##################
#cross-validation#
##################

# VIDEO 6

# Install cross-validation packages
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 

# Perform the cross validation
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

# Create a new CART model
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", cp = 0.18)

# Make predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64)

prp(StevensTreeCV)
