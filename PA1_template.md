Reproducible Research - Peer Assignment 1
=========================================

## Loading and preprocessing the data

Load the data (i.e. read.csv())

Process/transform the data (if necessary) into a format suitable for your analysis


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.2
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

activity <- read.csv("./activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file './activity.csv': No such
## file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```


## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

2. Make a histogram of the total number of steps taken each day

3. Calculate and report the mean and median of the total number of steps taken per day


```r
# Calcualate steps taken per day
activity_by_date <- group_by(activity, date)
```

```
## Error in group_by_(.data, .dots = lazyeval::lazy_dots(...), add = add): object 'activity' not found
```

```r
steps_per_date <- summarize(activity_by_date, steps_per_date = sum(steps, na.rm = T))
```

```
## Error in summarise_(.data, .dots = lazyeval::lazy_dots(...)): object 'activity_by_date' not found
```

```r
# Plot a histogram of steps taken per day
with(steps_per_date, hist(steps_per_date, main = "Steps taken per day", col = "light blue", breaks = 10))
```

```
## Error in with(steps_per_date, hist(steps_per_date, main = "Steps taken per day", : object 'steps_per_date' not found
```

```r
# Calculate and report the mean and median of the total number of steps taken per day
mean_steps_per_day <- round(mean(steps_per_date$steps_per_date), digits = 2)
```

```
## Error in mean(steps_per_date$steps_per_date): object 'steps_per_date' not found
```

```r
median_steps_per_day <- round(median(steps_per_date$steps_per_date), digits = 2)
```

```
## Error in median(steps_per_date$steps_per_date): object 'steps_per_date' not found
```

```r
abline(v=mean_steps_per_day, lwd = 3, col = 'red')
```

```
## Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...): object 'mean_steps_per_day' not found
```

```r
abline(v=median_steps_per_day, lwd = 3, col = 'dark violet')
```

```
## Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...): object 'median_steps_per_day' not found
```

```r
#create legend
legend("topright", lty = 1, lwd = 2, col = c("red", "dark violet"), cex = 0.75, 
       legend = c(paste('Mean:    ', mean_steps_per_day), 
                  paste('Median: ', median_steps_per_day)))
```

```
## Error in paste("Mean:    ", mean_steps_per_day): object 'mean_steps_per_day' not found
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Find which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps


```r
# Calculate average steps taken per 5 min interval
steps_by_interval <- group_by(activity, interval)
```

```
## Error in group_by_(.data, .dots = lazyeval::lazy_dots(...), add = add): object 'activity' not found
```

```r
steps_by_interval <- summarise(steps_by_interval, avg_steps_by_interval = 
                                   mean(steps, na.rm = T))
```

```
## Error in summarise_(.data, .dots = lazyeval::lazy_dots(...)): object 'steps_by_interval' not found
```

```r
# Plot time series average of steps taken Vs interval
with(steps_by_interval, plot(interval, avg_steps_by_interval, type = "l", 
                             main = "Average daily activity pattern",
                             xlab = "5-minute interval",
                             ylab = "Average number of steps"))
```

```
## Error in with(steps_by_interval, plot(interval, avg_steps_by_interval, : object 'steps_by_interval' not found
```

```r
# Calculate and report which interval has maximum number of average steps
tmp <- steps_by_interval[steps_by_interval$avg_steps_by_interval==max(steps_by_interval$avg_steps_by_interval), ]    
```

```
## Error in eval(expr, envir, enclos): object 'steps_by_interval' not found
```

```r
points(tmp$interval, tmp$avg_steps_by_interval, col = "red", pch = 19)
```

```
## Error in plot.xy(xy.coords(x, y), type = type, ...): plot.new has not been called yet
```

```r
legend_text <- paste("5-min interval at: ", tmp$interval, " has \n",
                     "maximum steps of ", round(tmp$avg_steps_by_interval, digits = 2))
```

```
## Error in round(tmp$avg_steps_by_interval, digits = 2): non-numeric argument to mathematical function
```

```r
legend("topright", legend = legend_text, text.col = "red", bty = "n", cex = 0.75)
```

```
## Error in as.graphicsAnnot(legend): object 'legend_text' not found
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
print_text <- paste("total records having NA: ", table(complete.cases(activity))[1])
```

```
## Error in complete.cases(activity): object 'activity' not found
```

```r
print(print_text)
```

```
## Error in print(print_text): object 'print_text' not found
```

```r
print_text <- paste("Steps column having NA: ", table(is.na(activity$steps))[2])
```

```
## Error in table(is.na(activity$steps)): object 'activity' not found
```

```r
print(print_text)
```

```
## Error in print(print_text): object 'print_text' not found
```

```r
print_text <- paste("Date column having NA: ", table(is.na(activity$date))[2])
```

```
## Error in table(is.na(activity$date)): object 'activity' not found
```

```r
print(print_text)
```

```
## Error in print(print_text): object 'print_text' not found
```

```r
print_text <- paste("Interval column having NA: ", table(is.na(activity$interval))[2])
```

```
## Error in table(is.na(activity$interval)): object 'activity' not found
```

```r
print(print_text)
```

```
## Error in print(print_text): object 'print_text' not found
```



2. Fill in all of the missing values in the dataset by the mean for that 5-minute interval.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# Function to replace NA with a given value
replaceNA <- function(x,y){
    if(is.na(x)){
        
        return(y)
    }
    return(x)
}

# Calcualte average steps per interval
avg_steps_by_interval <- aggregate(x = activity$steps , by = list(activity$interval), FUN = mean ,na.rm=TRUE)
```

```
## Error in aggregate(x = activity$steps, by = list(activity$interval), FUN = mean, : object 'activity' not found
```

```r
names(avg_steps_by_interval) <- c("interval","avg_steps")
```

```
## Error in names(avg_steps_by_interval) <- c("interval", "avg_steps"): object 'avg_steps_by_interval' not found
```

```r
merged_activity <- merge(x = activity, y = avg_steps_by_interval, by = "interval", all.x = TRUE)
```

```
## Error in merge(x = activity, y = avg_steps_by_interval, by = "interval", : object 'activity' not found
```

```r
# Replace steps with "NA" value with average per inerval calculated above
activity$steps2 <- mapply(replaceNA, merged_activity$steps, merged_activity$avg_steps)
```

```
## Error in mapply(replaceNA, merged_activity$steps, merged_activity$avg_steps): object 'merged_activity' not found
```

```r
# Plot histogram along with new mean and median Vs. old mean and median
activity_by_date2 <- group_by(activity, date)
```

```
## Error in group_by_(.data, .dots = lazyeval::lazy_dots(...), add = add): object 'activity' not found
```

```r
steps_per_date2 <- summarize(activity_by_date2, steps_per_date = sum(steps2, na.rm = T))
```

```
## Error in summarise_(.data, .dots = lazyeval::lazy_dots(...)): object 'activity_by_date2' not found
```

```r
mean_steps_per_day2 <- round(mean(steps_per_date2$steps_per_date), digits = 2)
```

```
## Error in mean(steps_per_date2$steps_per_date): object 'steps_per_date2' not found
```

```r
median_steps_per_day2 <- round(median(steps_per_date2$steps_per_date), digits = 2)
```

```
## Error in median(steps_per_date2$steps_per_date): object 'steps_per_date2' not found
```

```r
with(steps_per_date2, hist(steps_per_date, main = "Steps taken per day", col = "light gray", breaks = 10))
```

```
## Error in with(steps_per_date2, hist(steps_per_date, main = "Steps taken per day", : object 'steps_per_date2' not found
```

```r
abline(v=mean_steps_per_day2, lwd = 3, col = 'red')
```

```
## Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...): object 'mean_steps_per_day2' not found
```

```r
abline(v=median_steps_per_day2, lwd = 3, col = 'dark violet')
```

```
## Error in int_abline(a = a, b = b, h = h, v = v, untf = untf, ...): object 'median_steps_per_day2' not found
```

```r
legend("topright", lty = 1, lwd = 2, col = c("red", "brown", "dark violet", "purple"), 
       cex = 0.75, legend = c(paste('Mean:      ', mean_steps_per_day2), 
                              paste('Mean Old:  ', mean_steps_per_day),
                              paste('Median:    ', median_steps_per_day2),
                              paste('Median Old:', median_steps_per_day)))
```

```
## Error in paste("Mean:      ", mean_steps_per_day2): object 'mean_steps_per_day2' not found
```


## Differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data


```r
# Find whether a date is weekday or weekend
activity$date <- as.Date(activity$date)
```

```
## Error in as.Date(activity$date): object 'activity' not found
```

```r
activity$weekday <- as.factor(ifelse(weekdays(activity$date) %in% c("Saturday","Sunday"),
                                     "Weekend", "Weekday"))
```

```
## Error in weekdays(activity$date): object 'activity' not found
```

```r
# Find steps by time interval and weekday (weekdays or weekend)
activity_by_interval_weekday <- group_by(activity, interval, weekday)
```

```
## Error in group_by_(.data, .dots = lazyeval::lazy_dots(...), add = add): object 'activity' not found
```

```r
activity_by_interval_weekday <- summarise(activity_by_interval_weekday, 
                                          steps_by_interval_weeday = mean(steps2, na.rm = T));    
```

```
## Error in summarise_(.data, .dots = lazyeval::lazy_dots(...)): object 'activity_by_interval_weekday' not found
```

```r
# plot a time series of aggragsteps 
plot_obj <- ggplot(activity_by_interval_weekday,aes(interval, steps_by_interval_weeday)) +
    ggtitle("Activity patterns between weekdays and weekends") +
    facet_grid(. ~ weekday) +
    geom_line(size = 1)
```

```
## Error in ggplot(activity_by_interval_weekday, aes(interval, steps_by_interval_weeday)): object 'activity_by_interval_weekday' not found
```

```r
plot_obj
```

```
## Error in eval(expr, envir, enclos): object 'plot_obj' not found
```
