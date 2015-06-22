setwd("/Users/josemiguelarrieta/Documents/Analytics_Edge/")
data<-read.csv("Data/climate_change.csv")

test<-subset(data,Year<=2006)
train<-subset(data,Year>2006)

#Regression Model 
model1<-lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols ,data=train)
summary(model1)

cor(data)

model2<-lm(Temp~MEI+N2O+TSI+Aerosols ,data=train)
summary(model2)

#Select Variables with Step Function
climateStep=step(model1)

summary(climateStep)

#Predict on test 
tempPredict = predict(climateStep, newdata = test)

SSE = sum((tempPredict - test$Temp)^2)

SST = sum( (mean(train$Temp) - test$Temp)^2)

R2 = 1 - SSE/SST

