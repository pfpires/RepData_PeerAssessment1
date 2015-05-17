# Reproducible Research: Peer Assessment 1

This report presents the analysis of a dataset, collected over a period of two months, from a personal activity monitoring device, containing step counts. It answers the following questions:

 - What is mean total number of steps taken per day?
 - What is the average daily activity pattern?
 - Are there differences in activity patterns between weekdays and weekends?

The report was created as an assignment for the Reproducible Research Course on Coursera.


## Loading and preprocessing the data

The dataset was downloaded (cloned) in May 15, 2015, from [github](https://github.com/rdpeng/RepData_PeerAssessment1/blob/master/activity.zip). It can also be found [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip).

It consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data was read using read.csv, making sure the steps counts and interval labels were read as integers, and the dates as Date.



```r
outDir <- "output"

if (!file.exists(outDir)) {
    dir.create(outDir)
}

unzip("activity.zip", exdir = outDir, overwrite = TRUE)

dt <- read.csv(file.path(outDir,"activity.csv"), colClasses = c("integer","Date","integer"), stringsAsFactors = FALSE)
```

No other special treatments were applied to the data at this point.

## What is mean total number of steps taken per day?

Following are the total number of steps taken per day. Notice that days with all missing values are excluded.



```r
stepsPerDay <- aggregate(steps ~ date, dt, sum, na.rm = TRUE)
stepsPerDay
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```


And a histogram of the same data:


```r
hist(stepsPerDay$steps, xlab="Steps per day", main="Histogram of the total number of steps per day", breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

The mean and median of the total number of steps taken per day are:


```r
stepsPerDayMean <- mean(stepsPerDay$steps)
stepsPerDayMedian <- median(stepsPerDay$steps)
```

The mean of total steps per day is 10766.19 and the median is 10765.

## What is the average daily activity pattern?

Following is a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):


```r
stepsPerInterval <- aggregate(steps ~ interval, dt, mean, na.rm = TRUE)

plot(stepsPerInterval, type="l", main = "Average Number of Steps per Interval", xlab = "5-Minute Interval", ylab = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

The 5-minute interval that contains the maximum number of steps, on average across all the days in the dataset is:  


```r
stepsPerInterval[which.max(stepsPerInterval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

The total number of missing values in the dataset (i.e. the total number of rows with NAs) is given by:


```r
sum(!complete.cases(dt))
```

```
## [1] 2304
```

Next, the missing values in each 5-minute interval, will be replaced by the mean (rounded to 0 decimal places) of each 5-minute interval across all the dataset. Dataset dt2 will be created for this purpose.


```r
dt2 <- dt
for (i in 1:nrow(dt2) ){
    if(is.na(dt2$steps[i])) {
        dt2$steps[i] <- as.integer(round(mean(dt$steps[dt$interval == dt$interval[i]], na.rm = T)))
    }
}
```

Following are the total number of steps taken per day for the new dataset:


```r
stepsPerDay2 <- aggregate(steps ~ date, dt2, sum)
stepsPerDay2
```

```
##          date steps
## 1  2012-10-01 10762
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08 10762
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01 10762
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04 10762
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09 10762
## 41 2012-11-10 10762
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14 10762
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30 10762
```

And a histogram of the same data:


```r
hist(stepsPerDay2$steps, xlab="Steps per day", main="Histogram of the total number of steps per day\n(NAs replaced with mean of 5-min interval)", breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

The mean and median of the total number of steps taken per day, considering the new dataset are:


```r
stepsPerDay2Mean <- mean(stepsPerDay2$steps)
stepsPerDay2Median <- median(stepsPerDay2$steps)
```

The mean of total steps per day is 10765.64 and the median is 10762.

The mean of total steps per day changed slightly from 10766.19 to 10765.64.

The median also changed slightly from 10765 to 10762.

Since the dataset was missing full days, and since NA values were replaced by the mean of the 5-minute interval, it is expected that the newly added missing days have a total step count close to the mean step count per day. This was reflected in the histogram, where the column representing the number of observations with between 10000 and 12000 steps per day (where the mean falls), showed some increase.


## Are there differences in activity patterns between weekdays and weekends?

In order to represent which values fall on weekdays and which fall on weekends, the column wday will be added to the dataset:


```r
## NOTE: weekend day names are in Portuguese, please replace them with the apropriate names for your loccale if required

dt2$wday <- factor((weekdays(dt2$date) == "sábado" | weekdays(dt2$date) == "domingo")+1L,labels=c("weekday","weekend"))
```

Next is a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis):


```r
library(lattice)

# Split the dataset between weekdays and weekends and calculate the mean separately
dtmwend <- tapply(dt2$steps[dt2$wday=="weekend"],dt2$interval[dt2$wday=="weekend"],mean)
dtmwday <- tapply(dt2$steps[dt2$wday=="weekday"],dt2$interval[dt2$wday=="weekday"],mean)

# Create one data.frame with the results
dtmean <- data.frame(avgsteps=c(dtmwday,dtmwend), interval=c(names(dtmwday),names(dtmwend)),wday=c(rep("weekday",length(dtmwday)), rep("weekend",length(dtmwend))),stringsAsFactors=F)
dtmean$wday <- factor(dtmean$wday)
dtmean$interval <- as.integer(dtmean$interval)

# Plot the results
xyplot(avgsteps ~ interval | wday, dtmean, type = "l", layout=c(1,2))
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

Following is a simpler way of producing the same plot. It was not used because the assignment explicitly stated to create a plot of type="l", and it requires the use of type="a".


```r
xyplot(steps ~ interval | wday, dt2, type = "a", layout=c(1,2),ylim=c(-20,220))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

Both in weekdays and weekends there is a peak of activity between 8am (800) and 10am (1000).

During the weekdays periods of high activity start earlier in the morning, but also finish earlier in the evening.

During weekends there is a higher number of steps taken between 10am (1000) and 8pm (2000).
