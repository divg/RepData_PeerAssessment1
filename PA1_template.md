# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
Lets read the Activity data into a data frame.   

```r
activity = read.csv("activity.csv")
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?
1. Lets calculate the total number of steps per day. 

```r
StepsPerDay = tapply(activity$steps, activity$date, sum, na.rm=T)
StepsPerDay
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```

2. Histogram of the total number of steps taken each day.

```r
hist(StepsPerDay)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

3. Lets calculate mean and median. 

```r
avg1 = mean(StepsPerDay)
med1 = median(StepsPerDay)
```
*Mean of total number of steps taken per day is **9354.2295082**.*

*Median of total number of steps taken per day is 10395.*



## What is the average daily activity pattern?

1. Here's a time series plot of 5-minute intervals and average number of steps taken, averaged across all days.

```r
AvgStepsPerInterval = tapply(activity$steps, activity$interval, mean, na.rm=T)
plot(names(AvgStepsPerInterval), AvgStepsPerInterval )
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Lets check which interval has max. avg. number of steps. 

```r
MaxAvgStepsInterval = names(AvgStepsPerInterval)[which.max(AvgStepsPerInterval)]
```
** The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps is 835. **


## Imputing missing values

1. Lets calculate total number of missing values. 

```r
RowsMissingValues=sum(is.na(activity))
```
** Total number of missing values is 2304. **

2. Lets look at the boxplots to devise a simple imputation strategy for missing values. 

```r
boxplot(activity$steps ~ activity$date)
abline(lm(activity$steps ~ activity$date))
```

```
## Warning in abline(lm(activity$steps ~ activity$date)): only using the first
## two of 53 regression coefficients
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
boxplot(activity$steps ~ activity$interval)
abline(lm(activity$steps ~ activity$interval))
```

![](PA1_template_files/figure-html/unnamed-chunk-8-2.png) 

* We will impute missing values with the median for the interval as steps per interval values seems to have less variability as compared to steps per date.  * 

Next we will calculate the median steps per interval as per above strategy.

```r
MedianStepsPerInterval = tapply(activity$steps, activity$interval, median, na.rm=T)
```

3. Now we will create a new dataset that is equal to the original dataset but with the missing data filled in. 

```r
activityNew = activity
for (i in seq(0,2355,by=5)) {
  activityNew$steps[is.na(activityNew$steps) & activityNew$interval==i] = MedianStepsPerInterval[names(MedianStepsPerInterval)==i]
}
```

4. Histogram of the total number of steps taken each day.

```r
StepsPerDay_New = tapply(activityNew$steps, activityNew$date, sum, na.rm=T)
StepsPerDay_New
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##       1141        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015       1141      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414       1141      10600      10571       1141      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219       1141       1141      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336       1141         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##       1141
```

```r
hist(StepsPerDay_New)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

Now lets check how steps per day have changed due to imputation. 

```r
newMean = mean(StepsPerDay_New)
newMedian = median(StepsPerDay_New)
diffMean = mean(StepsPerDay_New) - mean(StepsPerDay)
diffMedian = median(StepsPerDay_New) - median(StepsPerDay)
```

* The new mean and median total number of steps taken per day are 9503.8688525 and 10395 respectively. * 

* By imputing missing data, the mean has changed by 149.6393443 and the median has changed by 0. * 

The mean and the standard deviation for total daily number of steps have changed as seen below: 

```r
summary(StepsPerDay)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```

```r
summary(StepsPerDay_New)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    6778   10400    9504   12810   21190
```

```r
sd(StepsPerDay)
```

```
## [1] 5405.895
```

```r
sd(StepsPerDay_New)
```

```
## [1] 5150.567
```

## Are there differences in activity patterns between weekdays and weekends?

1. Lets create a new factor variable indicating whether a given date is a weekday or weekend. 

```r
activityNew$dayofweek = ifelse((weekdays(x = strptime(activityNew$date,"%Y-%m-%d")) %in% 
                              c("Saturday", "Sunday")), 
                            "weekend",
                            "weekday")
```

2. Here's the plot of 5-min interval and average number of steps taken, averaged across all weekdays or weekends. 

```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```r
g = ggplot(data = activityNew, aes(interval, steps, group=dayofweek))
g + stat_summary(fun.y=mean, geom="line") + 
  facet_grid(dayofweek ~ .) +  
  xlab("Interval") +
  ylab("No. of steps") +
  ggtitle("Average no. of steps taken on weekday vs. weekend") 
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png) 

### --- End of report ---
