library(dplyr)
library(quantmod)
library(ggplot2)
library(stringi)
library(data.table)
library(moments)

data<- read.csv("C:\\Users\\jashb\\Desktop\\RELIANCE-BSE.csv")
data
View(data)
str(data)


#calculating Mean of index

mean(data$NIFTY.50)
mean(data$RELIA)
mean(data$INFY)
mean(data$TCS)
mean(data$ITC)


#standard deviation
sd(data$NIFTY.50,na.rm=TRUE)
sd(data$RELIA,na.rm=TRUE)
sd(data$INFY,na.rm=TRUE)
sd(data$TCS,na.rm=TRUE)
sd(data$ITC,na.rm=TRUE)

#calculating median

median(data$NIFTY.50)
median(data$RELIA)
median(data$INFY)
median(data$TCS)
median(data$ITC)

#calculating Skewness and Kurtosis
skewness(data$NIFTY.50,na.rm=T)
kurtosis(data$NIFTY.50,na.rm=T)
skewness(data$RELIA,na.rm=T)
kurtosis(data$RELIA,na.rm=T)
skewness(data$INFY,na.rm=T)
kurtosis(data$INFY,na.rm=T)
skewness(data$TCS,na.rm=T)
kurtosis(data$TCS,na.rm=T)
skewness(data$ITC,na.rm=T)
kurtosis(data$ITC,na.rm=T)


# Producing a histogram of the Price Data
price <- select(data,NIFTY.50)
(price)
hist(as.vector(price$NIFTY.50),
     xlab='NIFTY.50',
     main='Stock Data',
     col='steelblue')

price1 <- select(data,RELIA)
(price1)
hist(as.vector(price1$RELIA),
     xlab='RELIA',
     main='Stock Data',
     col='steelblue')

price2 <- select(data,INFY)
(price2)
hist(as.vector(price2$INFY),
     xlab='INFY',
     main='Stock Data',
     col='steelblue')

price3 <- select(data,TCS)
(price3)
hist(as.vector(price3$TCS),
     xlab='TCS',
     main='Stock Data',
     col='steelblue')

price4 <- select(data,ITC)
(price4)
hist(as.vector(price4$ITC),
     xlab='ITC',
     main='Stock Data',
     col='steelblue')

# Plotting a line graph of the data

class(data$Date)
as.character(data$Date)
Mutateddf <- data %>% mutate(Date=stri_sub(Date, -2))
Mutateddf
plot(Mutateddf$NIFTY.50,
     type='l',
     axes=FALSE,
     xlab='Year',
     ylab='Stock Price',
     main='NIFTY 50',
     col='blue')
axis(1, at=1: 1129, labels=Mutateddf$Date,
     cex.axis=0.9)
axis(2)


class(data$Date)
as.character(data$Date)
Mutateddf <- data %>% mutate(Date=stri_sub(Date, -2))
Mutateddf
plot(Mutateddf$RELIA,
     type='l',
     axes=FALSE,
     xlab='Year',
     ylab='Stock Price',
     main='RELIA',
     col='blue')
axis(1, at=1: 1129, labels=Mutateddf$Date,
     cex.axis=0.9)
axis(2)

class(data$Date)
as.character(data$Date)
Mutateddf <- data %>% mutate(Date=stri_sub(Date, -2))
Mutateddf
plot(Mutateddf$INFY,
     type='l',
     axes=FALSE,
     xlab='Year',
     ylab='Stock Price',
     main='INFY',
     col='blue')
axis(1, at=1: 1129, labels=Mutateddf$Date,
     cex.axis=0.9)
axis(2)

class(data$Date)
as.character(data$Date)
Mutateddf <- data %>% mutate(Date=stri_sub(Date, -2))
Mutateddf
plot(Mutateddf$TCS,
     type='l',
     axes=FALSE,
     xlab='Year',
     ylab='Stock Price',
     main='TCS',
     col='blue')
axis(1, at=1: 1129, labels=Mutateddf$Date,
     cex.axis=0.9)
axis(2)

class(data$Date)
as.character(data$Date)
Mutateddf <- data %>% mutate(Date=stri_sub(Date, -2))
Mutateddf
plot(Mutateddf$ITC,
     type='l',
     axes=FALSE,
     xlab='Year',
     ylab='Stock Price',
     main='ITC',
     col='blue')
axis(1, at=1: 1129, labels=Mutateddf$Date,
     cex.axis=0.9)
axis(2)


# Plotting the data in a bar chart

groupeddf <- group_by(Mutateddf,Date)
maxdf <- summarize(groupeddf,max(NIFTY.50))
barplot(maxdf$`max(NIFTY.50)`,
        main='NIFTY 50 ',
        xlab='Maximum Stock Price',
        ylab='Year',
        names.arg=maxdf$Date,
        col=c('blue','green','pink','yellow',
              'red','purple','orange'))

groupeddf <- group_by(Mutateddf,Date)
maxdf <- summarize(groupeddf,max(RELIA))
barplot(maxdf$`max(RELIA)`,
        main='RELIA',
        xlab='Maximum Stock Price',
        ylab='Year',
        names.arg=maxdf$Date,
        col=c('blue','green','pink','yellow',
              'red','purple','orange'))

groupeddf <- group_by(Mutateddf,Date)
maxdf <- summarize(groupeddf,max(INFY))
barplot(maxdf$`max(INFY)`,
        main='INFY',
        xlab='Maximum Stock Price',
        ylab='Year',
        names.arg=maxdf$Date,
        col=c('blue','green','pink','yellow',
              'red','purple','orange'))

groupeddf <- group_by(Mutateddf,Date)
maxdf <- summarize(groupeddf,max(TCS))
barplot(maxdf$`max(TCS)`,
        main='TCS',
        xlab='TCS',
        ylab='Year',
        names.arg=maxdf$Date,
        col=c('blue','green','pink','yellow',
              'red','purple','orange'))

groupeddf <- group_by(Mutateddf,Date)
maxdf <- summarize(groupeddf,max(ITC))
barplot(maxdf$`max(ITC)`,
        main='ITC',
        xlab='Maximum Stock Price',
        ylab='Year',
        names.arg=maxdf$Date,
        col=c('blue','green','pink','yellow',
              'red','purple','orange'))

library(plotrix)
piechart=c(9262,8792,5977,3427)
names1=c('NIFTY 50','RELIA','INFY','TCS')
piepercent<- round(100*piechart/sum(piechart), 1)
pie3D(piechart,labels=piepercent,explode = 0.001,main ="Growth",col = rainbow(4))          
legend("topright",c("NIFTY 50","RELIA","INFY","TCS"),cex = 0.7,fill =rainbow(length(names1)))

#corelation of IT 
library("ggpubr")
ggscatter(data, x = "INFY", y = "TCS", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "INFY PRICE", ylab = "TCS PRICE",col="blue")


#linear regresssion
model <- lm(INFY ~ TCS, data = data)
summary(model)
#this means INFY=-13.023 + 0.404*TCS




plot(density(data$ITC),col="blue")
plot(density(data$NIFTY.50),col="blue")
plot(density(data$ITC),col="blue")
lines(density(data$RELIA),col="green")

########################################################################################

library(plotrix)
piechart=c(9262,8792)
piepercent<- round(100*piechart/sum(piechart), 1)
names1=c('NIFTY 50','RELIA')
pie3D(piechart,labels=piepercent,explode = 0.001,main ="Growth",col = rainbow(2))          
legend("topright",c("NIFTY 50","RELIA"),cex = 0.7,fill =rainbow(length(names1)))


