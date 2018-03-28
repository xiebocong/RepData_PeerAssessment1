# Reproducible Research: Peer Assessment 1

### 1 Loading and preprocessing the data
#### 1.1 set work directory and install zip
```{r, results="hide",cache=TRUE}
setwd("/Users/dalinzhu/JanuaryR/Mycoursera")
install.packages("zip", repos = "http://cran.us.r-project.org")
```
#### 1.2 loading and read csv file
```{r, results="asis",cache=TRUE}
library("zip")
if(!file.exists("./RepW2"))
{dir.create("./RepW2")}
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL, destfile = "./RepW2/act.zip")
act <- read.csv(unzip ("./RepW2/act.zip", exdir="./RepW2/"),header = TRUE)
act$date <- as.Date(as.character(act$date))
```
### 2 What is mean total number of steps taken per day?
#### 2.1 process data for histogram
```{r results="asis",cache=TRUE}
library("RColorBrewer")
library("plyr")
col1 <- brewer.pal(11, "BrBG")
step_day <- ddply(act, .(date),summarise, daysteps= sum(steps))
hist(step_day$daysteps,col=col1[8],main="Histogram of Total Steps taken per day",
     xlab="Steps",ylab="Frequency",breaks = 5)
```

#### 2.2 mean of total steps taken per day
```{r results="asis",cache=TRUE}
mean(step_day$daysteps,na.rm=T)
median(step_day$daysteps,na.rm=T)
```

### 3 What is the average daily activity pattern?
#### 3.1 process data and plot daily activity pattern based on interval
```{r results="asis",cache=TRUE}
step_int <- ddply(act, .(interval),summarise, intsteps= mean(steps,na.rm=T))
plot(step_int$interval,step_int$intsteps,col=col1[9],main="Average Steps taken per 5-min interval ",xlab="Interval",ylab="Steps", type="l")
```

#### 3.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r results="asis",cache=TRUE}
step_int[which.max(step_int$intsteps),"interval"]
```

### 4 Imputing missing values
#### 4.1 calculating number of missing values
```{r results="asis",cache=TRUE}
sum(is.na(act$steps))
```

#### 4.2 use median of that 5-minute interval to fill in the missing values in the dataset
```{r results="asis",cache=TRUE}
act1 <- act
for (i in 1:nrow(act1)) {
if (is.na(act1$steps)[i]) 
{act1$steps[i] <- median(act1[act1$interval==act1$interval[i],]$steps,na.rm = TRUE)
}
        i=i+1
}
```
#### 4.3 process completed data and plot it 
```{r results="asis",cache=TRUE}
step1_day <- ddply(act1, .(date),summarise, daysteps= sum(steps))
hist(step1_day$daysteps,col=col1[8],main="Histogram of Total Steps taken per day",
     xlab="Total Steps taken per day",breaks = 5)
```

### 5 Are there differences in activity patterns between weekdays and weekends?
#### 5.1 Create a new factor variable in the dataset with two levels(weekday and weekend)
```{r results="asis",cache=TRUE}
weekday <- weekdays(act1$date)
for (i in 1:length(weekday)) {
if (weekday[i] %in% c("Saturday","Sunday")){
        weekday[i] <- "weekend"}
else{weekday[i] <- "weekday"
}
i=i+1
}
weekd<- as.factor(weekday) 
act2 <- act1
```
#### 5.2 process new data and plot it
```{r results="asis",cache=TRUE}
library("lattice")
step_wd <- ddply(act2, c("weekd","interval"),summarise, stepwd= mean(steps))
xyplot(stepwd~interval|weekd, data= step_wd,type="l",layout=c(1,2),
        main="Average Steps taken per 5-min interval",
        xlab="Interval",ylab= "Number of steps",
        col= col1[11])
```
