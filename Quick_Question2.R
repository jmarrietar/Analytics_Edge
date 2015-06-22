setwd("/Users/josemiguelarrieta/Documents/Analytics_Edge/")

############
#Regression#
############

wine<-read.csv("Data/wine.csv")
str(wine)
summary(wine)

model1<-lm(Price~AGST,data=wine)
summary(model1)

model2<-lm(Price~HarvestRain+WinterRain,data=wine)
summary(model2)

#############
#Correlation#
#############

cor(wine$Price,wine$WinterRain)
cor(wine)

############
#Prediction#
############
wineTest<-read.csv("Data/wine_test.csv")

predicTest<-predict(model2,newdata=wineTest)

#Sacar el SSE SST y R de la BD de pruebas 
SSE<-sum((wineTest$Price-predicTest)^2)
SST<-sum((wineTest$Price-mean(wine$Price))^2)
1-SSE/SST  #mayor que 1 (en el TEST )por lo cual creo que es mal modelo. 

