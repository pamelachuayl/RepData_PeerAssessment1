# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
# 1. Load the data
activityfile <- read.csv(unz("activity.zip", "activity.csv"))
```

## What is mean total number of steps taken per day?

```r
# 1. Histogram of the total number of steps taken each day
activityWithoutNA <- na.omit(activityfile)
aggStepsEachDay <- aggregate(activityWithoutNA[,'steps'], list(activityWithoutNA[,'date']), sum)
colnames(aggStepsEachDay) <- c('date','totalNumSteps')
hist(aggStepsEachDay[,'totalNumSteps'],breaks=10,xlab="Number of steps",ylab="Frequency",main="Histogram of total number of steps taken each day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
# 2. Mean and median total number of steps taken per day
mean(aggStepsEachDay[,'totalNumSteps'])
```

```
## [1] 10766
```

```r
median(aggStepsEachDay[,'totalNumSteps'])
```

```
## [1] 10765
```
The mean of the total number of steps taken per day is 10766 (rounded to whole number) and the median is 10765.

## What is the average daily activity pattern?

```r
# 1. Time series plot 
agg <- aggregate(activityWithoutNA[,'steps'], list(activityWithoutNA[,'interval']), mean)
colnames(agg) <- c('interval', 'avgSteps')
plot(agg$interval,agg$avgSteps,type="l",xlab="Interval",ylab="Avg number of steps",main="Time series plot of avg number of steps for each interval")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
# 5-minute interval with the maximum number of steps
agg[which(agg$avgSteps == max(agg[,'avgSteps'])),]
```

```
##     interval avgSteps
## 104      835    206.2
```
The 835 minutes interval contains the maximum number of steps.

## Imputing missing values

```r
# 1. Total number of missing values in dataset
sum(is.na(activityfile))
```

```
## [1] 2304
```

```r
# 2. Fill in missing values in dataset and 3. Create new dataset with missing data filled in

# Plan is to replace NA's with the mean for that particular 5-minute interval, as stored in agg file in above step
activityImputMissingVal <- activityfile
for(i in 1:nrow(activityImputMissingVal)){
  if(is.na(activityImputMissingVal[i,'steps'])){
    interval <- activityImputMissingVal[i,'interval']
    activityImputMissingVal[i,'steps'] = agg[which(agg$interval == interval),'avgSteps']
  }
}

# 4. Histogram of total number of steps taken each day
aggActivityImputMissingVal <- aggregate(activityImputMissingVal[,'steps'], list(activityImputMissingVal[,'date']), sum)
colnames(aggActivityImputMissingVal) <- c('date', 'totalNumSteps')
hist(aggActivityImputMissingVal[,'totalNumSteps'],breaks=10,xlab="Number of steps",ylab="Frequency",main="Histogram of total number of steps taken each day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
mean(aggActivityImputMissingVal[,'totalNumSteps'])
```

```
## [1] 10766
```

```r
median(aggActivityImputMissingVal[,'totalNumSteps'])
```

```
## [1] 10766
```
The mean remains the same as before since we are using the averaged values to fill in the missing data. The median is slightly different, now it is the same value as the mean value.

## Are there differences in activity patterns between weekdays and weekends?

```r
# 1. Create new factor variable
dayOfWeek <- weekdays(as.Date(activityImputMissingVal[,'date']))
weekDayEnd <- ifelse(dayOfWeek=='Saturday' | dayOfWeek=='Saturday', 'Weekend', 'Weekday')
activityImputMissingVal['dayType'] <- weekDayEnd

# 2. Panel plot
aggWeekdayEnd <- aggregate(activityImputMissingVal[,'steps'], list(activityImputMissingVal[,'interval'],activityImputMissingVal[,'dayType']), mean)
colnames(aggWeekdayEnd) <- c('interval','dayType','steps')
library(lattice)
xyplot(steps ~ interval | dayType, data=aggWeekdayEnd, layout=c(1,2),type='l', xlab="Interval", ylab="Number of steps", main="Activity patterns between weekdays and weekends")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


The peaks for the number of steps seem to be at the same intervals for both weekends and weekdays but there are larger peaks (more number of steps taken) during the weekends.
