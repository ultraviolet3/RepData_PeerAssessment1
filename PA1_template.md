---
#Reproducible Research: Peer Assessment 1
output: 
html_document:
keep_md: true
---

## Loading and preprocessing the data

```r
#libraries
    library(knitr)
#Set variables
    url="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    fil="activity.zip"
    dest= paste(getwd(),fil,sep="/")
#Check if file exists. If not, then download the data file
    if (!file.exists(fil)) {
        download.file(url,dest)
    }
#Unzip the data file
    unzip(dest)
#Read the csv file into a data frame
    fil=paste(getwd(),"activity.csv",sep="/")
    actdf=as.data.frame(read.csv(fil,header=TRUE, na.strings="NA",blank.lines.skip=TRUE))
    head(actdf)
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
#Removing missing values
    actdf=actdf[complete.cases(actdf),]
    head(actdf)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

```r
#Adding days column, Week column and week/weekend determination
    actdf$day=weekdays(as.Date(actdf$date))
    actdf$month = strftime(actdf$date, format = "%b %Y")
    actdf$odate=strftime(actdf$date, format = "%d")
    actdf$wday[actdf$day %in% c("Saturday","Sunday")]= "Weekend"
    actdf$wday[actdf$day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday")]= "Weekday"
    head(actdf)
```

```
##     steps       date interval     day    month odate    wday
## 289     0 2012-10-02        0 Tuesday Oct 2012    02 Weekday
## 290     0 2012-10-02        5 Tuesday Oct 2012    02 Weekday
## 291     0 2012-10-02       10 Tuesday Oct 2012    02 Weekday
## 292     0 2012-10-02       15 Tuesday Oct 2012    02 Weekday
## 293     0 2012-10-02       20 Tuesday Oct 2012    02 Weekday
## 294     0 2012-10-02       25 Tuesday Oct 2012    02 Weekday
```


## What is mean total number of steps taken per day?

```r
#libraries
    library(plyr)
#Finding average activity
    dailymean=ddply(actdf,.(date,month,odate),summarize,d_mean=mean(steps))
#Display results
    head(dailymean)
```

```
##         date    month odate   d_mean
## 1 2012-10-02 Oct 2012    02  0.43750
## 2 2012-10-03 Oct 2012    03 39.41667
## 3 2012-10-04 Oct 2012    04 42.06944
## 4 2012-10-05 Oct 2012    05 46.15972
## 5 2012-10-06 Oct 2012    06 53.54167
## 6 2012-10-07 Oct 2012    07 38.24653
```


## What is the average daily activity pattern?


```r
#libraries
    library(ggplot2)
#Plotting
    x<-ggplot(dailymean,aes(odate,d_mean),fill=month)
    x<-x+geom_bar(stat="identity",color="yellow",fill="violet")
    x<-x+facet_grid(.~month)
    x<-x+xlab("Date")
    x<-x+ylab("Activity (Steps)")
    x<-x+ggtitle("Daily Activity Pattern")
    #Display plot
    x
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

## Imputing missing values

```r
    head(actdf)
```

```
##     steps       date interval     day    month odate    wday
## 289     0 2012-10-02        0 Tuesday Oct 2012    02 Weekday
## 290     0 2012-10-02        5 Tuesday Oct 2012    02 Weekday
## 291     0 2012-10-02       10 Tuesday Oct 2012    02 Weekday
## 292     0 2012-10-02       15 Tuesday Oct 2012    02 Weekday
## 293     0 2012-10-02       20 Tuesday Oct 2012    02 Weekday
## 294     0 2012-10-02       25 Tuesday Oct 2012    02 Weekday
```

```r
    actdf=actdf[complete.cases(actdf),]
    head(actdf)
```

```
##     steps       date interval     day    month odate    wday
## 289     0 2012-10-02        0 Tuesday Oct 2012    02 Weekday
## 290     0 2012-10-02        5 Tuesday Oct 2012    02 Weekday
## 291     0 2012-10-02       10 Tuesday Oct 2012    02 Weekday
## 292     0 2012-10-02       15 Tuesday Oct 2012    02 Weekday
## 293     0 2012-10-02       20 Tuesday Oct 2012    02 Weekday
## 294     0 2012-10-02       25 Tuesday Oct 2012    02 Weekday
```

## Are there differences in activity patterns between weekdays and weekends?

```r
    #libraries
    library(plyr)
    library(ggplot2)
#Finding mean
    wdaymean=ddply(actdf,.(wday,day),summarize,wk_mean=mean(steps))
#Plotting
    y<-ggplot(wdaymean,aes(day,wk_mean,fill=wday))
    y<-y+geom_bar(stat="identity",colour="black")
    y<-y+xlab("Day")
    y<-y+ylab("Activity (Steps)")
    y<-y+ggtitle("Weekday vs Weekend Activity Pattern")
    y<-y+scale_x_discrete(limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
    y<-y+scale_fill_hue(l=40)
#Display plot
    y
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
