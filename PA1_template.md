# Reproducible Research: Peer Assessment 1
======================================================

## step 1. Load the data 


```r
activity.data <- read.csv("activity.csv")
apply(activity.data, 2, class)
```

```
##       steps        date    interval 
## "character" "character" "character"
```

```r
summary(activity.data)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```

Missing values: How many records with missing data?
total number of records, missing values for each variable

```r
nrow(activity.data); sum(is.na(activity.data$steps)); sum(is.na(activity.data$date)); sum(is.na(activity.data$interval))
```

```
## [1] 17568
```

```
## [1] 2304
```

```
## [1] 0
```

```
## [1] 0
```

## step 2 preprocess the data
convert steps to numeric might be useful later.
convert date to type Date

```r
activity.data$steps <- as.numeric(activity.data$steps)
activity.data$date <- as.Date(activity.data$date)
class(activity.data$steps)
```

```
## [1] "numeric"
```

```r
class(activity.data$date)
```

```
## [1] "Date"
```

# Key results

## What is mean total number of steps taken per day?

the mean number of steps taken per day regardless of missing values:

```r
sum(activity.data$steps, na.rm=TRUE)/61
```

```
## [1] 9354
```
Histogram of the total number of steps taken each day

```r
sum.steps.day <- aggregate(activity.data$steps, list(activity.data$date), sum)
library(lattice)
histogram(~x, data = sum.steps.day, xlab = "steps",ylab = "freqeuncy - days", main = "Histogram of total number of steps taken each day" )
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

mean steps taken per day

```r
steps.day <- aggregate(activity.data$steps, list(activity.data$date), sum)
head(steps.day)
```

```
##      Group.1     x
## 1 2012-10-01    NA
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
mean(steps.day$x, na.rm=TRUE)
```

```
## [1] 10766
```
median steps taken per day

```r
median(steps.day$x, na.rm=TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
complete.data <- activity.data[complete.cases(activity.data), ]
sum(is.na(complete.data$steps))  ## check that NAs hv been removed
```

```
## [1] 0
```

```r
nrow(complete.data)
```

```
## [1] 15264
```

```r
steps.interval <- aggregate(complete.data$steps, list(complete.data$interval), mean)
colnames(steps.interval) <- c("interval","mean.steps")
nrow(steps.interval)
```

```
## [1] 288
```

```r
str(steps.interval)
```

```
## 'data.frame':	288 obs. of  2 variables:
##  $ interval  : int  0 5 10 15 20 25 30 35 40 45 ...
##  $ mean.steps: num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

```r
xyplot(mean.steps~interval, steps.interval, type ="l")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

the 5 minute interval that accounts for the max no of steps on average across all days

```r
tail(steps.interval[order(steps.interval$mean.steps), ], 1)
```

```
##     interval mean.steps
## 104      835      206.2
```

Answer: The 0835 to 0840 5 minute interval sees the most steps This is also shown in the time series plot

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
