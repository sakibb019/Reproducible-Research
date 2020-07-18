---
title: "Peer Assignment - 1"
author: "Shikhar"
date: "18/07/2020"
output: html_document
---



##Loading and preprocessing the data
```{r}
activity <- read.csv("D:/R/Class/5Reproducible Research/activity.csv")
activity$date <- as.Date(as.character(activity$date))
head(activity)
```

##What is mean total number of steps taken per day?
```{r}
steps_by_day <- aggregate(steps ~ date, activity, sum)
head(steps_by_day)
hist(steps_by_day$steps,xlab="Steps",col="maroon")
mean(steps_by_day$steps)
median(steps_by_day$steps)
```

##What is the average daily activity pattern?
```{r}
steps_by_i <- aggregate(steps ~ interval, activity, mean)
plot(steps_by_i$interval,steps_by_i$steps,type="l",xlab="Interval",ylab="Steps")
m <- steps_by_i[which.max(steps_by_i$steps),1]
```

##Imputing missing values
```{r}
missingvalues <- sum(is.na(activity))
StepsAverage <- aggregate(steps ~ interval, activity,mean)
f <- numeric()
for (i in 1:nrow(activity)) {
  obs <- activity[i, ]
  if (is.na(obs$steps)) {
    steps <- subset(StepsAverage, interval == obs$interval)$steps
  } else {
    steps <- obs$steps
  }
  f <- c(f,steps)
}
new_activity <- activity
new_activity$steps <- f
StepsTotalUnion <- aggregate(steps ~ date, new_activity, sum, na.rm = TRUE)
hist(StepsTotalUnion$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="purple", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "purple"), lwd=10)
rmean <- mean(StepsTotalUnion$steps)
rmean
rmedian <- median(StepsTotalUnion$steps)
rmedian
dme <- mean(StepsTotalUnion$steps) - mean(steps_by_day$steps,na.rm = T)
dme
dmed <- median(StepsTotalUnion$steps) - median(steps_by_day$steps)
dmed
```

##Are there differences in activity patterns between weekdays and weekends?
```{r}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
new_activity$wowd = as.factor(ifelse(is.element(weekdays(as.Date(new_activity$date)),weekdays), "Weekday", "Weekend"))
StepsTotalUnion <- aggregate(steps ~ interval + wowd, new_activity, mean)
library(lattice)
xyplot(StepsTotalUnion$steps ~ StepsTotalUnion$interval|StepsTotalUnion$wowd, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```
