---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

This report documents the steps taken to complete the first peer-assessed assignment in week 2 of the "Reproducible Research" course by R. Peng from Johns Hopkins University. It consists of the following sections:

- [Loading and preprocessing the data] 
- [What is the mean total number of steps taken per day?]  
- [What is the average daily activity pattern?]  
- [Imputing missing values]  
- [Are there differences in activity patterns between weekdays and weekends?]  

## Loading and Preprocessing the Data
The first step is to unzip and load the data set into R as well as preprocess the data. 

### 1) Unzipping and Loading the Data
We unzip the given data set as follows:

```r
unzip("activity.zip")
```
Then we load the data set into a variable called *activityData*:

```r
activityData <- read.csv("activity.csv", header = TRUE)
```
  
### 2) Preprocessing the Data
There are three columns in the data set: steps, date and interval. The entries in the date column are stored as character values in YYYY-MM-DD format and therefore need to be transformed into date format. 

```r
activityData$date <- as.Date(activityData$date, format = "%Y-%m-%d")
```
The entries in the interval column come as integers in the format 5, 10, 15, 20, ... 100, 105, 110, ... 2355 and have to be transformed into time format.

```r
activityData$interval <- sprintf("%04d", activityData$interval)
activityData$interval <- format(strptime(activityData$interval,"%H%M"), format = "%H:%M") 
```
This is what the preprocessed data set looks like:

```r
head(activityData)
```

```
##   steps       date interval
## 1    NA 2012-10-01    00:00
## 2    NA 2012-10-01    00:05
## 3    NA 2012-10-01    00:10
## 4    NA 2012-10-01    00:15
## 5    NA 2012-10-01    00:20
## 6    NA 2012-10-01    00:25
```
  
## What is the mean total number of steps taken per day?
In a second step we look at the total number of steps taken per day. We create a histogram and compute mean and median values.
### 1) Make a histogram of the total number of steps taken each day
In preparation for the histogram, we first calculate the total number of steps taken per day.

```r
totalSteps <- with(activityData, tapply(steps, date, sum, na.rm = TRUE))
```
And then plot them as a histogram using the base plotting system:

```r
hist(totalSteps, breaks = 20, main = "Total number of steps taken each day")
abline(v=mean(totalSteps), col="blue")
abline(v=median(totalSteps), col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
  
The mean total number of steps per day is indicated by the blue line, the median total number of steps per day by the red line.

### 2) Calculate and report the mean and median total number of steps taken per day
The mean of the total number of steps taken per day amounts to:

```r
mean1 <- mean(totalSteps)
mean1
```

```
## [1] 9354.23
```
The median of the total number of steps taken per day amounts to:

```r
median1 <- median(totalSteps)
median1
```

```
## [1] 10395
```
  
## What is the average daily activity pattern?
In this section we look at the average daily activity pattern. To obtain the average daily activity pattern, we calculate the average number of steps taken for each time interval.

```r
library(reshape2)
```

```
## Warning: package 'reshape2' was built under R version 4.0.4
```

```r
dailyActivityPattern <- with(activityData, tapply(steps, interval, mean, na.rm = TRUE))
dailyActivityPattern <- melt(dailyActivityPattern)
colnames(dailyActivityPattern) <- c("interval", "steps")
```
  
### 1) Time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Next, we create a time series plot of the average daily activity pattern using the base plotting system:

```r
plot(format(strptime(dailyActivityPattern$interval,"%H:%M"), format="%H%M"), dailyActivityPattern$steps, type = "l", col = "blue", main = "Daily Activity Pattern", xlab = "Time Interval", ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
  
### 2) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
The following code allows us to compute the time interval that contains the maximum number of steps across all the days in the data set:

```r
intervalMax <- dailyActivityPattern$interval[dailyActivityPattern$steps==max(dailyActivityPattern$steps)]
```
The 5-minute interval, which on average across all the days in the data set contains the maximum number of steps is **08:35**.

## Imputing missing values
In a next step, we impute missing values in the data set.  
### 1) Calculate and report the total number of missing values in the dataset
First, we calculate the total number of missing values.

```r
missingValues <- is.na(activityData$steps)
missing <- sum(missingValues)
```
There are **2304** values missing in the data set.

### 2) Devise a strategy for filling in all of the missing values in the dataset
We will use the average number of steps across all days in the data set we calculated for the time intervals as part of the daily activity pattern to replace the missing values in the data set.

### 3) Create a new dataset that is equal to the original dataset but with the missing data filled in.
The following code creates a new data set as a copy of the original data set and substitutes missing values with the computed average number of steps for the time interval.

```r
activityData_complete <- activityData
for(i in 1:nrow(activityData_complete)){
    if(missingValues[i]){ 
        activityData_complete$steps[i] <- dailyActivityPattern[dailyActivityPattern$interval == activityData_complete$interval[i], "steps"]
    }
}
```
The new data set looks as follows:

```r
head(activityData_complete)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01    00:00
## 2 0.3396226 2012-10-01    00:05
## 3 0.1320755 2012-10-01    00:10
## 4 0.1509434 2012-10-01    00:15
## 5 0.0754717 2012-10-01    00:20
## 6 2.0943396 2012-10-01    00:25
```
### 4) Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
In the following we compare the original and the new complete data set with each other and determine the impact the imputing of missing data has had.

First we compute the total number of steps taken each day for the new data set:

```r
totalSteps2 <- with(activityData_complete, tapply(steps, date, sum, na.rm = TRUE))
```
Then we create a histogram of total number of steps taken each day for the new data set and compare it to the original:

```r
par(mfrow = c(1,2))

hist(totalSteps2, breaks = 20, ylim = c(0, 20), main = "Dataset w/o Missing Values")

hist(totalSteps, breaks = 20, ylim = c(0, 20), main = "Original Dataset")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->
  
We can see that the distribution of total number of steps taken per day has changed. There are less days with a low number of steps taken and more days with a total number of steps taken close to the average and median values. Replacing missing values with the average number of steps for respective time interval means that the total number of steps per day for days that had missing values in the original data set have moved from the left towards the center of the distribution.

The calculation of the new mean and median values returns the following results:

```r
mean2 <- mean(totalSteps2)
mean2
```

```
## [1] 10766.19
```

```r
median2 <- median(totalSteps2)
median2
```

```
## [1] 10766.19
```
The new mean and median total number of steps taken per day amount to **1.0766189\times 10^{4}** respectively. In comparison, the mean and median total number of steps taken per day for the original data set were calculated to be **9354.2295082** and **10395**. This means that imputing missing values increased both the mean and median total daily number of steps, which is little surprising. However, our strategy of replacing missing values with the average number of steps taken per time interval not only changed the distribution but also resulted in conflating the median and mean total number of steps.


## Are there differences in activity patterns between weekdays and weekends?
In the last section of the assignment we take a look at the differences in activity patterns between weekdays and weekends.

### 1) Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
First, we create a new variable called *day* and compute whether a given date is a weekday or weekend. 

```r
activityData$interval <- format(strptime(activityData$interval,"%H:%M"), format = "%H%M") 
activityData$day <- weekdays(activityData$date)
```
Next, we create two subsets from the original data set, one for weekday and one for weekend days.

```r
subset_weekday <- activityData[activityData$day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),]
subset_weekend <- activityData[activityData$day %in% c("Saturday", "Sunday"),] 
```
Based on the two data subsets we compute the respective daily activity patterns for weekdays and weekends. Before recombining the data, we reintroduce the column *day*, which indicates whether the entry belongs to a weekday or weekend day.

```r
library(reshape2)

# activity pattern for weekday days
subset_weekday <- with(subset_weekday, tapply(steps, interval, mean, na.rm = TRUE))
subset_weekday <- melt(subset_weekday)
subset_weekday <- cbind(subset_weekday, "weekday")
colnames(subset_weekday) <- c("interval", "steps", "day")

# activity pattern for weekend days
subset_weekend <- with(subset_weekend, tapply(steps, interval, mean, na.rm = TRUE))
subset_weekend <- melt(subset_weekend)
subset_weekend <- cbind(subset_weekend, "weekend")
colnames(subset_weekend) <- c("interval", "steps", "day")

# recombine data
activityPatterns <- rbind(subset_weekday, subset_weekend)
```

### 2) Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
The following code plots time series plots for weekday and weekend days using the lattice plotting system:

```r
library(lattice)

activityPatterns <- transform(activityPatterns, day = factor(day))
xyplot(steps ~ interval | day, data = activityPatterns, type = "l", layout = c(1,2), xlab = "Time Interval", ylab = "Number of Steps", main = "Comparison of Daily Activity Patterns on Weekdays and Weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-21-1.png)<!-- -->
  
As can be seen in the time series plots, the activity patterns differ considerably between weekday and weekend days. Average activity on weekday days peaks in the morning hours and is lower during the rest of the day with additional smaller peaks around midday, in between 3 and 4pm as well as in the evenings between 5 and 7pm, whereas activity is distributed more diffusely in between 8am and 8pm on weekends. In addition, activity levels rise much steeper and earlier on weekday days from about 5am than on weekend days. In contrast, activity levels drop off later during evenings on weekends than they do during the week with the evening peak also shifting backward by approximately an hour.
