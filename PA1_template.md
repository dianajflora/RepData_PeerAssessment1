# Reproducible Research: Course Project 1

## Task 1: Load data

```r
setwd("~/R/Reproducible_Research/Assignment_1")
data <- read.csv("activity.csv")
```

## Task 2: What is mean total number of steps taken per day?

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.3
```

```r
totalsteps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(totalsteps, binwidth=1000, xlab="number of steps per day")
```

![Image 1](https://github.com/dianajflora/RepData_PeerAssessment1/blob/master/unnamed-chunk-2-1.png)

```r
mean(totalsteps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(totalsteps, na.rm=TRUE)
```

```
## [1] 10395
```

## Task 3: What is the average daily activity pattern?

```r
library(ggplot2)
avg <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=avg, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average total steps")
```

![Image 2](https://github.com/dianajflora/RepData_PeerAssessment1/blob/master/unnamed-chunk-3-1.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avg[which.max(avg$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Task 4: Imputing missing values


```r
missingvalues <- is.na(data$steps)
table(missingvalues)
```

```
## missingvalues
## FALSE  TRUE 
## 15264  2304
```

Replace missing value with the mean value of its 5-minute interval. 


```r
replace.func <- function(steps, interval) {
    replaced <- NA
    if (!is.na(steps))
        replaced <- c(steps)
    else
        replaced <- (avg[avg$interval==interval, "steps"])
    return(replaced)
}
replaced.data <- data
replaced.data$steps <- mapply(replace.func, replaced.data$steps, replaced.data$interval)
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
totalsteps <- tapply(replaced.data$steps, replaced.data$date, FUN=sum)
qplot(totalsteps, binwidth=1000, xlab="number of steps per day")
```

![Image 3](https://github.com/dianajflora/RepData_PeerAssessment1/blob/master/unnamed-chunk-7-1.png)

```r
mean(totalsteps)
```

```
## [1] 10766.19
```

```r
median(totalsteps)
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps? 

The mean and median are both higher after imputing missing data, because we're replacing `NA` values, which are set to 0s by default, with the mean value of the 5-minute interval.

## Task 5: Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
id.weekday <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
replaced.data$date <- as.Date(replaced.data$date)
replaced.data$day <- sapply(replaced.data$date, FUN=id.weekday)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avg <- aggregate(steps ~ interval + day, data=replaced.data, mean)
ggplot(avg, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![Image 4](https://github.com/dianajflora/RepData_PeerAssessment1/blob/master/unnamed-chunk-9-1.png)


