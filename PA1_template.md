---
title: "**Peer Assessment 1 in Reproducible Research**"
output: html_document
---

- Author: GL
- Date: 05/16/2015


## 1. Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the ¡°quantified self¡± movement ¨C a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.


## 2. Solutions
### 2.1 Loading and preprocessing the data

Load the data and transform the data (if necessary) into a format suitable the analysis.


```r
mydata <- read.csv("activity.csv")
```

### 2.2 Mean total number of steps taken per day

Calculate the total number of steps taken per day and analyze the results by drawing histograms.


```r
## Sort data in terms of date
dataPerDay <- split(mydata, mydata$date)
stepsPerDay <- lapply(dataPerDay, function(xx) 
        { sum( xx$steps, na.rm = TRUE ) })

## Draw the histogram
library(ggplot2)
stepsPerDay <- t( as.data.frame(stepsPerDay))
stepsPerDay <- as.data.frame(stepsPerDay)
colnames(stepsPerDay) <- "totalSteps"
q <- ggplot(stepsPerDay, aes(x = totalSteps)) 
q + geom_histogram(position = 'identity', alpha = 0.5, fill = "red") +
        labs(x = "Total Steps per Day", y = "Freqency") +
        labs(title = "Histogram of the total number of steps taken each day")
```

![plot of chunk stepsPerDay](figure/stepsPerDay-1.png) 

```r
## Calculate mean and median steps per day
mymean <- round( mean( t(as.vector(stepsPerDay)) ) )
mymedian <- median(t(as.vector(stepsPerDay)))
```
**The mean and median of the total number of steps taken per day are 9354 and 10395, respectively.**

### 2.3 The average daily activity pattern

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
## Sort data in terms of date and calculate the average per interval
dataDaily <- split(mydata, mydata$interval)
stepsDaily <- lapply(dataDaily, function(xx) 
        { mean( xx$steps, na.rm = TRUE ) })

## build a data.frame of myDailyPattern as the output
myDailyPattern <- rbind( as.data.frame(stepsDaily), 
                         as.integer(names(stepsDaily)))
myDailyPattern <- t(myDailyPattern)
colnames(myDailyPattern) <- c("average","interval")
myDailyPattern <- as.data.frame(myDailyPattern)

## Draw a plot using ggplot2
library(ggplot2)
q <- ggplot(myDailyPattern, aes(x = interval, y = average)) 
q + geom_line() + labs( title = "Averaged Step Numbers as a Function of Interval", x = "5-min Interval", y = "Averaged Step Numbers")
```

![plot of chunk dailyPattern](figure/dailyPattern-1.png) 

```r
## Determine the interval corresponding to the maximum averaged step number
maxIndex <- which.max(myDailyPattern$average)
maxInterval <- myDailyPattern$interval[maxIndex]
```

**The interval of 835, on average across all the days in the dataset, contains the maximum number of steps.**

### 2.4 Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.


```r
rowNA <- is.na( mydata$steps )
numNA <- sum( as.integer(rowNA) )

## The strategy is to use the averaged step values calcualted in Section 2.3 to replace the missing values
filledData <- lapply(dataDaily, function(xx) {
        dateChecker <- which( is.na(xx$steps) == TRUE )
        b <- as.integer( (xx$interval[1])/5 + 1)
        xx[dateChecker,"steps"] <-  round( myDailyPattern[ b,"average"] )
        xx
})

## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
library(ggplot2)

myfilledData <- unsplit(filledData, mydata$interval)
dataPerDay2 <- split(myfilledData, myfilledData$date)
stepsPerDay2 <- lapply(dataPerDay2, function(xx) 
        { sum( xx$steps, na.rm = TRUE ) })
stepsPerDay2 <- t( as.data.frame(stepsPerDay2))
stepsPerDay2 <- as.data.frame(stepsPerDay2)
colnames(stepsPerDay2) <- "totalSteps"
q <- ggplot(stepsPerDay2, aes(x = totalSteps)) 
q + geom_histogram(position = 'identity', alpha = 0.5, fill = "red") +
        labs(x = "Total Steps per Day", y = "Freqency") +
        labs(title = "Histogram of the total number of steps taken each day")
```

![plot of chunk imputMissing](figure/imputMissing-1.png) 

```r
mymean2 <- round( mean( t(as.vector(stepsPerDay2)) ) )
mymedian2 <- median(t(as.vector(stepsPerDay2)))
```

**The mean and median of the total number of steps taken per day are 1.0282 &times; 10<sup>4</sup> and 1.0395 &times; 10<sup>4</sup>, respectively.**

### 2.5 Differences in activity patterns between weekdays and weekends


```r
## Create a new factor variable in the dataset with two levels ¨C ¡°weekday¡± and ¡°weekend¡± indicating whether a given date is a weekday or weekend day.
myWeekdays <- weekdays(as.Date(myfilledData$date))
dayChecker <- myWeekdays == "ÐÇÆÚÁù" | myWeekdays == "ÐÇÆÚÈÕ"
activity <- rep(x = "Weekday", times = length(dayChecker))
a <- which( dayChecker == TRUE )
activity[a] <- "Weekend"
activity <- as.factor(activity)
myfilledData2 <- cbind(myfilledData, activity)

## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
dataA <- subset(myfilledData2, dayChecker)
dataB <- subset(myfilledData2, !dayChecker)

dataDailyA <- split(dataA, dataA$interval)
stepsDailyA <- lapply(dataDailyA, function(xx) 
        { mean( xx$steps ) })
dataDailyB <- split(dataB, dataB$interval)
stepsDailyB <- lapply(dataDailyB, function(xx) 
        { mean( xx$steps ) })

## build a data.frame of myDailyPattern as the output
myDailyPatternA <- rbind( as.data.frame(stepsDailyA), 
                         as.integer(names(stepsDailyA)))
myDailyPatternA <- t(myDailyPatternA)
colnames(myDailyPatternA) <- c("average","interval")
myDailyPatternA <- as.data.frame(myDailyPatternA)

myDailyPatternB <- rbind( as.data.frame(stepsDailyB), 
                         as.integer(names(stepsDailyB)))
myDailyPatternB <- t(myDailyPatternB)
colnames(myDailyPatternB) <- c("average","interval")
myDailyPatternB <- as.data.frame(myDailyPatternB)



## Draw a plot
par(mfrow = c(2,1))
plot(x = myDailyPatternA$interval, y = myDailyPatternA$average, type = "l")
plot(x = myDailyPatternB$interval, y = myDailyPatternB$average, type = "l")
```

![plot of chunk diffs](figure/diffs-1.png) 

