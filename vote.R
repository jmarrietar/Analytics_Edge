gerber<-read.csv("Data/gerber.csv")

table(gerber$voting,gerber$civicduty)

#Which of the four "treatment groups" had the largest percentage of people who actually voted (voting = 1)?
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

########USING LOGISTIC##################
#Logistic Regression 
LogRegression=glm(voting~civicduty+hawthorne+self+neighbors, data = gerber, family = "binomial")

#First compute predictions:
predictLog = predict(LogRegression, type="response")

#Then, use the table function to make a confusion matrix:
table(gerber$voting, predictLog > 0.3)

#Accuracy 
(134513+51966)/(134513+100875+56730+51966)

#Using a threshold of 0.5, what is the accuracy of the logistic regression model?
table(gerber$voting, predictLog > 0.5)
library(ROCR)

ROCRpred = prediction(predictLog, gerber$voting)

#AUC of the CART model 
as.numeric(performance(ROCRpred, "auc")@y.values)

#########USING TREES#######################



