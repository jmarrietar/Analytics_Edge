setwd("/Users/josemiguelarrieta/Documents/Analytics_Edge/")

#A stock market is where buyers and sellers trade shares of a company, 
#and is one of the most popular ways for individuals and companies to invest money.

IBM<-read.csv("Data/IBMStock.csv")
GE<-read.csv("Data/GEStock.csv")
ProcterGamble<-read.csv("Data/ProcterGambleStock.csv")
CocaCola<-read.csv("Data/CocaColaStock.csv")
Boeing<-read.csv("Data/BoeingStock.csv")

####################
#SUMMARY STATISTICS#
####################

#Convert Date to Date Objects in all DataSets. 
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

summary(IBM)
summary(GE)
summary(CocaCola)
summary(Boeing)
sd(ProcterGamble$StockPrice)

############################
#VISUALIZING STOCK DYNAMICS#
############################

plot(CocaCola$Date, CocaCola$StockPrice,type="l",col="red")
lines(ProcterGamble$Date,ProcterGamble$StockPrice,col="blue")

#To see a date, draw Vertical Line. 
abline(v=as.Date(c("2000-03-01")),lwd=2)

abline(v=as.Date(c("1983-03-01")),lwd=2)

#This will plot the CocaCola stock prices from 1995 through 2005, which are the observations numbered from 301 to 432. The additional argument, 
#ylim=c(0,210), makes the y-axis range from 0 to 210

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))



