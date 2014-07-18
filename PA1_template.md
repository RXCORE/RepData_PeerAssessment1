# Reproducible Research: Peer Assessment 1
# By Ron Chua

## Loading and preprocessing the data

We are Downloading, extracting and storing the **activity** data into a data frame. 
Format the **Date field to Date Type** and sample the values to be able to understand the Activity data.


```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = "Dataset.zip", method = "curl")
```

```
## Warning: running command 'curl  "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"  -o "Dataset.zip"' had status 127
## Warning: download had nonzero exit status
```

```r
unzip("Dataset.zip")
```

```
## Warning: error 1 in extracting from zip file
```

```r
activity <- read.csv("./activity.csv")
activity$date <- as.Date(activity$date)
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(activity)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```

## What is mean total number of steps taken per day?

This is a histogram of the total number of steps taken **each** day.



```r
library(ggplot2)
q<-qplot(date, weight=activity$steps, data=activity, geom="histogram")  + geom_bar(fill="#FF9999", colour="blue")
print(q)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Calculation of the mean and median total number of steps taken **per** day:


```r
mean(tapply(activity$steps, activity$date, sum, na.rm = TRUE))
```

```
## [1] 9354
```

```r
median(tapply(activity$steps, activity$date, sum, na.rm = TRUE))
```

```
## [1] 10395
```

## What is the average daily activity pattern?

A **Time Series** plot of the 5-minute interval and the average number of steps taken that are averaged across all days.


```r
average_steps<-data.frame(cbind(activity$interval,tapply(activity$steps, activity$interval, mean, na.rm = TRUE)))
colnames(average_steps) <- c("interval","steps")
q<-ggplot(data=average_steps,aes(x=interval,y=steps)) +
  geom_line(colour="#000099")
print(q)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Five minute interval that contains the maximum number of steps across all the days in the dataset.


```r
maxtime<-average_steps[which.max(average_steps$steps),"interval"]
strftime( as.POSIXct(Sys.Date()) + as.difftime(round(maxtime/100), units="hours")+ as.difftime(maxtime%%100, units="mins"), "%r",tz="UTC")
```

```
## [1] "08:35:00 AM"
```

## Imputing missing values

Total number of  **NA's** in the dataset.


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

The mean for five minute interval, to replacing the missing values.


```r
fixed<-activity
fixed[is.na(fixed[, 1]), 1]<-average_steps[is.na(fixed[, 1]),2]
```

The histogram of the new dataset, with the NAs replaced with the five  minute means.


```r
qplot(date, weight=fixed$steps, data=fixed, geom="histogram") + geom_bar(fill="green", colour="blue")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

Report the mean and media values. 
The observations has higher values.  As seen in the graph above. The increased number of mean of steps, 
and have NAs have been replaced by the five minute interval mean.


```r
mean(tapply(fixed$steps, fixed$date, sum, na.rm = TRUE))
```

```
## [1] 10766
```

```r
median(tapply(fixed$steps, fixed$date, sum, na.rm = TRUE))
```

```
## [1] 10766
```

## Are there differences in activity patterns between weekdays and weekends?

Adding **Weekend  or  Weekday** to a new field.


```r
library(lattice)
fixed$day<-as.factor(ifelse(weekdays(fixed$date) %in% c("Saturday","Sunday"),"Weekend","Weekday"))
```

Plot the five minute average of steps, by weekday/weekend.


```r
xyplot(steps ~ interval | day, aggregate(steps ~ interval + day, fixed, FUN = mean), layout = c(1, 2), type = "l", group=day)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
