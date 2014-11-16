install.packages("lattice")
library(lattice)
##read file and remove NAs
aDataOrig<-read.csv("activity.csv",colClasses=c('numeric','Date','numeric'))
aData<-aDataOrig[complete.cases(aDataOrig),]

## Aggregate by date
aDatabyday<-aggregate(aData$steps ~ aData$date,data=aData,FUN=sum)
colnames(aDatabyday)<-c("Date","TotalSteps")

##Plot histogram
par(mfrow=c(1,1))
hist(as.numeric(aDatabyday$TotalSteps), xlab="TotalSteps by Date", main="Histogram of Total Steps by Date", col="light blue")

##Calc mean by date
aDataMeanbyday<-aggregate(aData$steps ~ aData$date,data=aData,FUN=mean)
colnames(aDataMeanbyday)<-c("Date","MeanSteps")

##Calc median by date
aDataMedianbyday<-aggregate(aData$steps ~ aData$date,data=aData,FUN=median)
colnames(aDataMedianbyday)<-c("Date","MedianSteps")

##Calc total mean and Median
mean(aDatabyday$TotalSteps)
median(aDatabyday$TotalSteps)

##Calc mean by 5-min interval across all days
aDataMeanbyInterval<-aggregate(aData$steps ~ aData$interval,data=aData,FUN=mean)
colnames(aDataMeanbyInterval)<-c("Interval","MeanSteps")

##time series plot of mean by 5-min interval across all days
plot(aDataMeanbyInterval$Interval,aDataMeanbyInterval$MeanSteps,type="l", xlab="Interval", ylab="Average steps", main="Average Steps by Interval across all days", col="blue")

## Find the 5-min interval that has the max mean across all days
maxinterval<-aDataMeanbyInterval[aDataMeanbyInterval$MeanSteps==max(aDataMeanbyInterval$MeanSteps),1]

##total number of missing values
nrow(subset(aDataOrig, is.na(aDataOrig$steps)))

##Replace NAs with average by 5 min interval across all days
aDataImput<-merge(aDataMeanbyInterval,aDataOrig, by.x="Interval",by.y="interval",all.y)
aDataImput[is.na(aDataImput$steps),]$steps<-aDataImput[is.na(aDataImput$steps),]$MeanSteps

##histogram with imputed values
aDataImputbyday<-aggregate(aDataImput$steps ~ aDataImput$date,data=aDataImput,FUN=sum)
colnames(aDataImputbyday)<-c("Date","TotalSteps")
hist(aDataImputbyday$TotalSteps, xlab="TotalSteps by Date", main="Histogram of Total Steps by Date with Imputation", col="pink")

##Calc mean by date with imputed values
aDataImputMeanbyday<-aggregate(aDataImput$steps ~ aDataImput$date,data=aDataImput,FUN=mean)
colnames(aDataImputMeanbyday)<-c("Date","MeanSteps")

##Calc median by date
aDataImputMedianbyday<-aggregate(aDataImput$steps ~ aDataImput$date,data=aDataImput,FUN=median)
colnames(aDataImputMedianbyday)<-c("Date","MedianSteps")

##Calc total mean and Median
mean(aDataImputbyday$TotalSteps)
median(aDataImputbyday$TotalSteps)

##Add a factor column for weekdays vs weekend
aDataImput$type<-paste("weekday")
aDataImput[which(weekdays(aDataImput$date)==c("Saturday","Sunday")),]$type<-"weekend"

##Calc mean by 5-min interval across all weekdays
aDataImputMeanbyInterval<-aggregate(aDataImput$steps ~ aDataImput$type + aDataImput$Interval,data=aDataImput,FUN=mean)
colnames(aDataImputMeanbyInterval)<-c("type","Interval","MeanSteps")

##time series plot of mean by 5-min interval by type
xyplot(aDataImputMeanbyInterval$MeanSteps~aDataImputMeanbyInterval$Interval | type ,type="l",data=aDataImputMeanbyInterval,layout=c(1,2), col="red")
