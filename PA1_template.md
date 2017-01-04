---
title: "PA1_template.rmd"
author: "Andrea"
date: "January 1, 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
```{r load_packages, warning=FALSE, echo=FALSE, message=FALSE}
library(broman)
library(VIM)
library(mice)
library(lattice)
library(ggplot2)
```
# Activity Monitoring Devices


This assignment uses data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data

The variables included in this dataset are:

    steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
    date: The date on which the measurement was taken in YYYY-MM-DD format
    interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

```{r dataset}
# check to see if data files exist, if not retrieve
if(!file.exists("activity.csv") | !file.exists("activity.zip")) {
  temp <- tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
  file <- unzip(temp)
  unlink(temp)
}
```
```{r load-data}
activity <- read.csv("activity.csv", header = TRUE, sep=",", stringsAsFactor=FALSE, na.strings="NA")
str(activity)
```

## The mean (average) total number of steps taken per day
```{r descriptive-stats}
# sum of the total number of steps taken per day
tot_steps <- aggregate(steps~date, data=activity, sum, na.rm=TRUE)
# store the mean and median number of steps taken per day
results.mean <- mean(tot_steps$steps)
results.median <- median(tot_steps$steps)
```
### histogram of the total number of steps taken each day
```{r steps-hist}
hist(tot_steps$steps, xlab=" Total Steps per Day", main="Histogram")  
abline(v = mean(tot_steps$steps), col="red", lwd=2)
abline(v = median(tot_steps$steps), col="green", lwd=1.2)
legend(x="topright", c("Mean", "Median"), col = c("red","green"), lwd=c(2,1.2))  
```


Ignoring missing values, the mean number of steps taken per day is `r myround(results.mean, 2)` and the median is
`r myround(results.median, 2)`.

## Average daily activity pattern
The 5-minute interval that, on average, contains the maximum number of steps
```{r average-pattern}
# calculate the average number of steps taken across all days
average_steps <- aggregate(steps~interval, data=activity, mean, na.rm=TRUE)
# identify the maximum value
max_value <- max(average_steps$steps)
# determine the interval that has the value for the maximum number of steps
max_interval <-average_steps[which.max(average_steps$steps),1]
```
### Time series plot of the average number of steps taken
```{r plot-average}
plot(average_steps$interval,average_steps$steps, type="l", xlab="Intervals", ylab="Number of Steps",main="Average Number of Steps per Day \n by Interval")
axis(side =1, at=c(0,500,835,1000,1500,2000),labels=c("0","500", "835","1000","1500","2000"))
abline(v = max_interval, col="green", lwd=2)
legend(x="topright", c("Max Number of Steps"), col = c("green"), lwd=c(2)) 
```



The maximum frequency of the number of steps is `r max_value`.  The 5-minute interval that contains the value for the maximum frequency of the number of steps is `r max_interval`

# Missing Values


There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

```{r missing-values, echo=FALSE}
NA_col <- colSums(is.na(activity))
NACol <- NA_col[NA_col>0]
activity_Miss <- function(activity){sum(is.na(activity))/length(activity)*100}
aggr_plot <- aggr(activity, col=c('navyblue', 'red'), numbers=TRUE, labels=names(activity), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
```


The plot above helps us understand that 87% of the data are not missing any information and that the 13% that are missing are all in the steps column (`r NACol` rows).
```{r NA-stats}
apply(activity,2,activity_Miss)
colSums(is.na(activity))
```
## Impute 'fill in' missing values
The strategy used to impute missing values is predictive mean matching (PMM) in which missing data are imputed using the observed values with the closest predictive mean; PMM is less sensitive to violation of the normality assumption (Rezvan, Lee, & Simpson, 2015).
```{r impute-data}
#activity[, 2] <- factor(activity[, 2])
tempData <- mice(activity, m=5, maxit=10, meth='pmm', seed=1, printFlag=FALSE)
imputedData <- complete(tempData,1)
```
### Verify equality after data has been imputed
```{r verify-imputed, cache=TRUE}
xyplot(tempData,steps ~ interval,pch=18, cex=1)
```


Verify that the shape of the magenta points (imputed) matches the shape of the blue ones (observed). 
The matching shape in the above plot tells us that the imputed values are plausible values.

## What is the mean total number of steps taken per day?
```{r imp-descr-stats}
# sum of the total number of steps taken per day
imp_tot_steps <- aggregate(steps~date, data=imputedData, sum)
# calculate the average number of steps taken across all days
imp_results.mean <- mean(imp_tot_steps$steps)
imp_results.median <- median(imp_tot_steps$steps)
```
```{r imp-max}
# identify the maximum value
imp_average_steps <- aggregate(steps~interval, data=imputedData, mean)
imp_max_value <- max(imp_average_steps$steps)
# determine the interval that has the value for the maximum number of steps
imp_max_interval <-imp_average_steps[which.max(imp_average_steps$steps),1]
```
### Histogram of the total number of steps taken each day after missing values are imputed
```{r hist-imputed}
hist(imp_tot_steps$steps, xlab="Steps (imputed)", main="Histogram Total Steps per Day \n (with imputed values)")  
abline(v = mean(imp_tot_steps$steps), col="green", lwd=2.2)
abline(v = median(imp_tot_steps$steps), col="red", lwd=1.7)
legend(x="topright", c("Mean", "Median"), col = c("green","red"), lwd=c(2.2,1.7))  
```


The maximum frequency of the number of steps is `r myround(imp_max_value, 3)`.  The 5-minute interval that contains the value for the maximum frequency of the number of steps is `r imp_max_interval`

The mean number of steps taken per day is `r myround(imp_results.mean, 2)` and the median is
`r myround(imp_results.median, 2)`.

## Look for differences in activity patterns between weekdays and weekends

```{r}
#create a factor variable for weekend and weekday classification
imputedData[, 2] <- as.Date(imputedData[, 2])
week_days <- c('Monday','Tuesday','Wednesday','Thursday','Friday')
imputedData$daytype <- factor((weekdays(imputedData$date) %in% week_days), levels=c(FALSE,TRUE), labels=c('weekend','weekday'))
```
```{r imp-averages}
# calculate average steps by interval averaged across all weekdays and weekends
imp_averages <- aggregate(steps ~ interval + daytype, data = imputedData, mean)
imp_sum_day <- aggregate(steps~date+daytype, data=imputedData, sum)
imp_averages_daytype <-aggregate(steps~daytype, data=imp_sum_day, mean)
```
```{r, echo=FALSE}
mean_steps_we <-imp_averages_daytype[1,2]
mean_steps_wd <-imp_averages_daytype[2,2]
```
```{r}
# identify the maximum value by day type
imp_max_value_type <- aggregate(steps ~daytype, data=imp_averages, FUN=max)
# determine the interval that has the value for the maximum number of steps weekend
imp_max_interval_we <- imp_averages[which(grepl(imp_max_value_type$steps[1], imp_averages$steps)),1]
# determine the interval that has the value for the maximum number of steps weekend
imp_max_interval_wd <- imp_averages[which(grepl(imp_max_value_type$steps[2], imp_averages$steps)),1]
```
### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
```{r }
# time series panel plot
# (x-axis is the interval, y-axis is the avg steps) (by weekend by weekday)
qplot(interval, steps, data=imp_averages, color=daytype, geom="line", facets=.~daytype) + ggtitle("Avg Steps per 5-minute interval \n across weekdays and weekends (after imputation)") + theme(legend.position="none")
```
```{r interval-vertline, echo=FALSE}
#imp_interval <- data.frame(daytype = c("weekend", "weekday"), interval=c(imp_max_interval_we,imp_max_interval_wd))
#p+geom_vline(aes(xintercept=interval), imp_interval)
```

On the weekdays days, even though one interval shows a spike in activity, overall the activity patterns of average number of steps taken per 5-minute interval support that there is more activity on the weekend days. 


This panel plot is an additional resource shows that more activity appears to occur on the weekend days; the average number of total steps for the 16 weekend days is (`r myround(mean_steps_we,2)`). The average number of total steps for the 45 weekday days is (`r myround(mean_steps_wd, 2)`).



# References


Rezvan, P. H., Lee, K. J., & Simpson, J. A. (April 7, 2015). The rise of multiple imputation: a review of the reporting and implementation of the method in medical research. BMC Medical Research Methodology, 2015, 15:30. doi:10.1186/s12874-015-0022-1