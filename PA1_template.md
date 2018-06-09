---
title: "PA1_template.Rmd"
author: "Tim Norton"
date: "June 8, 2018"
output:
  html_document:
    keep_md: yes
---
## The following document takes time series activity measurents and performs analysis on it.

## The first analysis is the total number of steps per day

```r
df<-read.csv(file="activity.csv", header=TRUE, sep=',')
df$steps<-as.numeric(df$steps)
df$date<-as.factor(df$date)
df1<-aggregate(df$steps, by=list(Day=df$date), FUN=sum)
names(df1)<-c("Day", "Steps")
df1$Steps<-as.numeric(df1$Steps)
hist(df1$Steps, col="red", xlab="Total Steps", main="Frequency of Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

## Below are the mean and median steps per day over two months


```r
mean(df1$Steps, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(df1$Steps, na.rm=TRUE)
```

```
## [1] 10765
```

## Below is the plot that shows the average number of steps per five minute interval over two months


```r
library(ggplot2)
df2<-read.csv(file="activity.csv", header=TRUE, sep=',')
df2$steps<-as.numeric(df2$steps)
df2$date<-as.factor(df2$date)
df2<-aggregate(df2$steps, by=list(Interval=df2$interval), FUN=mean, na.rm=TRUE)
pl<-ggplot(data=df2, aes(x=Interval, y=x))+
    labs(xlab="Daily five minute interval", ylab="Number of Steps Per Interval", title="Average number of Steps per Interval")+
    geom_line()
pl
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## Below is the five minute interval with the highest number of steps 


```r
df2$ave=df2$x
df2$x<-NULL
df2[which.max(df2$ave),]
```

```
##     Interval      ave
## 104      835 206.1698
```

## For every NA value in the data set, I took the average number of steps per five minute interval and substituted the average for the NA value. Immediately below is the number of NAs in the dataset.


```r
df3<-read.csv(file="activity.csv", header=TRUE, sep=',')
df3$steps<-as.numeric(df3$steps)
df3$date<-as.factor(df3$date)
sum(rowSums(is.na(df3)))
```

```
## [1] 2304
```

## I created a new dataset with the substitutions for the NAs, and created a histogram of the total number of steps per day over the two months


```r
df3$interval=as.factor(df3$interval)
df3[is.na(df3$steps), "steps"] <- tapply(df3$steps, df3$interval, mean, na.rm=TRUE)[ df3[is.na(df3$steps),"interval"] ]
df3$steps<-as.numeric(df3$steps)
df3$date<-as.factor(df3$date)
df4<-aggregate(df3$steps, by=list(Day=df3$date), FUN=sum)
names(df4)<-c("Day", "Steps")
df4$Steps<-as.numeric(df4$Steps)
hist(df4$Steps, col="red", xlab="Sum of Steps per Day", main="Histogram of Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
## The mean and median value for the sum of steps per day is below. You can compare this with the first analysis but the mean and median below include substituted values for NAs. As you can see, there is hardly any differences in the results.


```r
mean(df4$Steps)
```

```
## [1] 10766.19
```

```r
median(df4$Steps)
```

```
## [1] 10766.19
```

## Below is a time-series plot of average steps per five minute interval, but the data is split by weekday and weekend day. This plot takes the average steps per interval over two months divided by weekday and weekend day.


```r
library(ggplot2)
df5<-read.csv(file="activity.csv", header=TRUE, sep=',')

df5$steps<-as.numeric(df5$steps)
df5$date<-as.Date(df5$date)

df5$interval=as.factor(df5$interval)
df5[is.na(df5$steps), "steps"] <- tapply(df5$steps, df5$interval, mean, na.rm=TRUE)[ df5[is.na(df5$steps),"interval"] ]

df5$week <- ifelse(weekdays(df5$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
df6<-aggregate(.~interval+week, df5, mean)

p<-ggplot(data=df6, aes(x=interval, y=steps, group=week))+geom_line(color="blue")+facet_grid(week~.)+
    labs(xlab="Interval", ylab="Number of Steps", title="Average Number of Steps per Interval on Weekends and Weekdays")
p
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

