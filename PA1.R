setwd("./data/")
## Loading and preprocessing data
activity.data <- read.csv("activity.csv", stringsAsFactors=FALSE, comment.char="")
activity.data$date <- as.Date(activity.data$date, "%Y-%m-%d")
## Calculate total number of steps taken each day
steps.per.day <- tapply(activity.data$steps, activity.data$date, sum, na.rm=TRUE)
## Histogram of total number of steps taken each day
hist(steps.per.day, main="Histogram of total steps per day", xlab="Total steps per day",
     breaks=10)
## Mean and Median of total number of steps taken per day
mean.before.imputation <- mean(steps.per.day, na.rm=TRUE)
median.before.imputation <- median(steps.per.day, na.rm=TRUE)

## Average daily activity pattern
steps.per.interval <- aggregate(steps ~ interval, data = activity.data,
                                FUN = mean)
## Time series plot
plot(steps ~ interval, data = steps.per.interval, type = "l",
     main = "Time series plot of average no. of steps",
     xlab = "5-minute interval",
     ylab = "Average number of steps taken")
## Calculating the interval with maximum number of steps
steps.per.interval$interval[steps.per.interval$steps==max(steps.per.interval$steps)]
## Total number of missing values in the dataset
sum(is.na(activity.data))
## Creating new dataset with no missing values
mean.steps.per.interval <- aggregate(activity.data$steps, by=list(activity.data$interval),
                                     mean, na.rm=TRUE)
names(mean.steps.per.interval) <- c("interval", "meansteps")
new.activity.data <- activity.data
head(new.activity.data)
sum(is.na(new.activity.data))
for (i in 1:nrow(new.activity.data)) {
    if(is.na(new.activity.data$steps[i])) {
        new.activity.data$steps[i] <- mean.steps.per.interval[which(new.activity.data$interval[i]==mean.steps.per.interval$interval), ]$meansteps
    }
}
head(new.activity.data)
sum(is.na(new.activity.data))
steps.each.day <- tapply(new.activity.data$steps, new.activity.data$date, sum)
hist(steps.each.day, main="Histogram of total steps taken each day", xlab="Total steps taken each day",
     breaks=10)
mean.after.imputation <- mean(steps.each.day)
median.after.imputation <- median(steps.each.day)
## COmparing estimates before and after imputation
estimates <- data.frame(mean=c(mean.before.imputation, mean.after.imputation), 
                        median=c(median.before.imputation, median.after.imputation))
rownames(estimates) <- c("before", "after")
estimates
## Differences between activity patterns between weekday and weekends
new.activity.data$day <- weekdays(new.activity.data$date)
for (i in 1:nrow(new.activity.data)) {
    if(new.activity.data$day[i] %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        new.activity.data$indicator[i] <- "Weekday"
    else
        new.activity.data$indicator[i] <- "Weekend"
}
new.activity.data$indicator <- as.factor(new.activity.data$indicator)
table(new.activity.data$indicator)
avg.steps <- aggregate(steps ~ interval + indicator, data=new.activity.data, mean)
## Panel plot
library(lattice)
xyplot(avg.steps$steps ~ avg.steps$interval | avg.steps$indicator, layout=c(1,2), 
       type="l", main="Panel plot of average number of steps", 
       xlab="interval", ylab="average number of steps")
