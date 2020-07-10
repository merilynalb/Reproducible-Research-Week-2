---
title: "Reproducible Research Week 2"
author: "Meri"
date: "10/07/2020"
output: 
  html_document: 
    keep_md: true
---



##Loading and preparing the data


```r
unzip("Activity Monitoring Data.zip")
mydata <- read.csv("activity.csv", header=TRUE)
summary(mydata)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

##Task 1: What is the mean total number of steps taken per day?

Firstly, caclulate the total number of steps taken per day

```r
StepsEachDay <- aggregate(mydata$steps, list(mydata$date), FUN=sum)
colnames(StepsEachDay) <- c("Date", "Steps")
```

Following this, create a histogram of the total number of steps per day

```r
g <- ggplot(StepsEachDay, aes(Steps))
g+geom_histogram(fill="light blue", col="dark blue", binwidth = 2500)+ggtitle("Total Number of Steps Per Day")+xlab("Steps")+ylab("Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

Lastly, calculate the mean and median values of total number of steps each day

```r
mean(StepsEachDay$Steps, na.rm=TRUE)
```

```
## [1] 10766.19
```


```r
median(StepsEachDay$Steps, na.rm=TRUE)
```

```
## [1] 10765
```

##Task 2: What is the average daily activity pattern?

Create a time series plot of 5-minute interval (x axis) and average number of steps taken each day, across all days (y axis)

```r
StepsPerInterval <- aggregate(data=mydata, steps~interval, FUN=mean)
colnames(StepsPerInterval) <- c("Interval", "Steps")
with(StepsPerInterval, plot(Steps, type="l", main="Average Number of Steps per 5-minute Interval", xlab = "Time", ylab="Steps", col="red"))
```

![](PA1_template_files/figure-html/time series-1.png)<!-- -->

Following this, calculate the time interval which contains the maximum number of steps

```r
StepsPerInterval %>% select(Interval, Steps) %>% filter(Steps==max(Steps))
```

```
##   Interval    Steps
## 1      835 206.1698
```

##Task 3: Imputing missing values

Calculate how many missing values are there in the entire dataset

```r
sum(is.na(mydata))
```

```
## [1] 2304
```

Then, devise a strategy to fill in the missing values in the dataset. In this case, I will be filling the NA values with mean number of steps for that day

```r
mydata$newsteps <- ifelse(is.na(mydata$steps), mean(mydata$steps, na.rm=TRUE), mydata$steps)
```

Create a new dataset which is equal to the original dataset, but with the missing values filled in

```r
new_mydata <- data.frame(steps=mydata$newsteps, date=mydata$date, interval=mydata$interval)
head(new_mydata, n=6)
```

```
##     steps       date interval
## 1 37.3826 2012-10-01        0
## 2 37.3826 2012-10-01        5
## 3 37.3826 2012-10-01       10
## 4 37.3826 2012-10-01       15
## 5 37.3826 2012-10-01       20
## 6 37.3826 2012-10-01       25
```

Make a histogram of the total number of steps per day and calculate and report the mean and median number of steps per day

```r
NewStepsEachDay <- aggregate(new_mydata$steps, list(new_mydata$date), FUN=sum)
colnames(NewStepsEachDay) <- c("Date", "Steps")

g <- ggplot(NewStepsEachDay, aes(Steps))
g+geom_histogram(fill="light blue", col="dark blue", binwidth = 2500)+ggtitle("Total Number of Steps Per Day")+xlab("Steps")+ylab("Frequency")
```

![](PA1_template_files/figure-html/histogram 2-1.png)<!-- -->

Lastly, I calculated the mean and median values of total number of steps each day, for this new dataset

```r
mean(NewStepsEachDay$Steps)
```

```
## [1] 10766.19
```


```r
median(NewStepsEachDay$Steps)
```

```
## [1] 10766.19
```
Replacing the missing values with the mean number of steps for that day has little change on the mean and median values calculated. However, the frequency of steps has increased in the new dataset, with replaced missing values. It has gone from a frequency of 17 to 25. 

##Task 4: Are there any differences in activity patterns between weekdays and weekends?

Firstly, create a new factor variable with two levels indicating 'weekday' and 'weekend'

```r
new_mydata$date <- as.Date(strptime(new_mydata$date, format="%Y-%m-%d"))
new_mydata$day <- weekdays(new_mydata$date)

for (i in 1:nrow(new_mydata)) {
  if (new_mydata[i,]$day %in% c("Saturday","Sunday")) {
    new_mydata[i,]$day<-"weekend"
  }
  else{
    new_mydata[i,]$day<-"weekday"
  }}
head(new_mydata, n=6)
```

```
##     steps       date interval     day
## 1 37.3826 2012-10-01        0 weekday
## 2 37.3826 2012-10-01        5 weekday
## 3 37.3826 2012-10-01       10 weekday
## 4 37.3826 2012-10-01       15 weekday
## 5 37.3826 2012-10-01       20 weekday
## 6 37.3826 2012-10-01       25 weekday
```

Then, form a panel plot of 5-minute time interval (x axis) and average number of steps taken, averaged across all weekdays and weekends (y axis)

```r
NewStepsPerInterval <- aggregate(new_mydata$steps ~ new_mydata$interval + new_mydata$day, FUN=mean)
colnames(NewStepsPerInterval) <- c("Interval", "Day", "Steps")

xyplot(Steps ~ Interval | Day, NewStepsPerInterval, type="l", layout=c(1,2), xlab="Interval", ylab="Steps")
```

![](PA1_template_files/figure-html/panel plot-1.png)<!-- -->
