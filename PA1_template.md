---
output: 
  html_document:
          keep_md: yes
---



```r
library("tidyverse")
```

# Loading and preprocessing the data

## 1 Load the data

```r
df0 <- read.csv("activity.csv")
```



## 2Process / Transform

```r
df <- na.omit(df0)
rownames(df) <- NULL
df$date <- lubridate::as_date(df$date)
head(df)
```

```
##   steps       date interval
## 1     0 2012-10-02        0
## 2     0 2012-10-02        5
## 3     0 2012-10-02       10
## 4     0 2012-10-02       15
## 5     0 2012-10-02       20
## 6     0 2012-10-02       25
```

# What is mean total number of steps taken per day?

## 1 Calculate the total number of steps taken per day

```r
sm1 <- df %>% group_by(date) %>% summarise(tsteps = sum(steps))
```

## 2 Plot Histogram

```r
hist(sm1$tsteps, xlab = "Total steps per day", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->



```r
print (paste0("Mean Total steps per day: ", round(mean(sm1$tsteps),2), " Median total steps per day: ", median(sm1$tsteps) ))
```

```
## [1] "Mean Total steps per day: 10766.19 Median total steps per day: 10765"
```

# What is the average daily activity pattern?

## 1 Make a time series plot 

```r
sm2 <- df %>% group_by(interval) %>% summarise(asteps = mean(steps))
plot(x = sm2$interval, y= sm2$asteps, type = "l", xlab = "5 Minute Inteval", ylab = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
print (paste0( "Maximum Steps =  ", sm2[sm2$asteps == max(sm2$asteps),]$interval))
```

```
## [1] "Maximum Steps =  835"
```


# Imputing missing values

## 1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA NAs)

```r
sum(complete.cases(df0) == FALSE)
```

```
## [1] 2304
```

## Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
df1 <- df0 %>% group_by(interval) %>%
mutate(steps=ifelse(is.na(steps),median(steps,na.rm=TRUE),steps))
```


## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
sm3 <- df0 %>% group_by(date) %>% summarise(tsteps = sum(steps))
sm4 <- df1 %>% group_by(date) %>% summarise(tsteps = sum(steps))
```



```r
hist(sm4$tsteps, xlab = "Total steps per day (Imputed)", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->



```r
print (paste0("Mean Total steps per day: ", round(mean(sm3$tsteps, na.rm = TRUE),2), " Median total steps per day: ", median(sm3$tsteps, na.rm = TRUE) ))
```

```
## [1] "Mean Total steps per day: 10766.19 Median total steps per day: 10765"
```

```r
print (paste0("[Imputed]  Mean Total steps per day: ", round(mean(sm4$tsteps, na.rm = TRUE),2), " Median total steps per day: ", median(sm4$tsteps, na.rm = TRUE) ))
```

```
## [1] "[Imputed]  Mean Total steps per day: 9503.87 Median total steps per day: 10395"
```

# Are there differences in activity patterns between weekdays and weekends?

## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
df1$date <- lubridate::as_date(df1$date)
df1$weekday <- as.factor(ifelse(weekdays(df1$date) %in% c("Saturday","Sunday"), "weekend","weekday"))

levels(df1$weekday)
```

```
## [1] "weekday" "weekend"
```

## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.




```r
sm3 <- df1 %>% group_by(weekday,interval) %>% summarise(asteps = mean(steps))
```

```
## `summarise()` has grouped output by 'weekday'. You can override using the `.groups` argument.
```

```r
ggplot(data=sm3) + geom_line(aes(x = interval, y = asteps)) + facet_wrap(~weekday, ncol = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->



