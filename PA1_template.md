# Reproducible Research: Peer Assessment 1

This is the course project 1 for the course reproducible research.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Part 1: Loading and preprocessing the data

```{r}
#reading data
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
str(data)
```

## Part 2: What is mean total number of steps taken per day?

We calculate the total number of steps taken per day and then we plot a histogram of (steps per day)
```{r}
library(ggplot2)
s <- tapply(data$steps, as.factor(data$date), sum)
qplot(s) + geom_histogram() + xlab("Steps per day")
me <- c(mean(s, na.rm = T), median(s, na.rm = T))

```

**The mean for total steps per day is `r me[1]` and the median is `r me[2]`.**

## Part 3: What is the average daily activity pattern?

We make a plot for the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r}
m <- tapply(data$steps, as.factor(data$interval)
            , mean, na.rm = T)

qplot(data$interval[1:288], m) + 
            geom_line() + 
            xlab("interval") + 
            ylab("average number of steps")

ma <- max(m)
inter <- data$interval[1:288]
max <- inter[m == ma]
```

**The 5-minute interval that contains the maximum number of steps is `r max`.**

## Part 4: Imputing missing values

In this part we're going to deal with missing values, we'll replace the missing values by the mean of the corresponding interval, **the number of missing values is 2304**

```{r}
m1 <- rep(m,61)
for (i in 1:length(data$steps)){
            if (is.na(data$steps[i])){
                    data$steps[i] <- m1[i]
            }
}
```
a histogram for the steps per day:
```{r}
s <- tapply(data$steps, as.factor(data$date), sum)
qplot(s) + geom_histogram() + xlab("Steps per day")
```

It's clear that the histogram changes from the first part.

## Part 5:Are there differences in activity patterns between weekdays and weekends?

We Created a new factor variable in the data set with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day, and then We make a plot for the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) to see the deference.

```{r}
for (i in 1:length(data$date)){
            if (weekdays(data$date[i]) == "Saturday" |
                weekdays(data$date[i]) == "Sunday")
                      data$weekday[i] <- "Weekend"
            
            else data$weekday[i] <- "Weekday"
}
m <- tapply(data$steps, as.factor(data$interval)
            , mean, na.rm = T)

qplot(data$interval[1:288], m) + 
  facet_grid(~data$weekday) +
  geom_line() + 
  xlab("interval") + 
  ylab("average number of steps")
```

