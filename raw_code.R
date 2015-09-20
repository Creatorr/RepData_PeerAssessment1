#############################################################################################
##
##  Analyze data collected from a personal activity monitoring device
##
##  Written by: Romanov Aleksandr (20.09.15)
##
##  Purpose:
##  This script for assignment Coursera Reproducible Research: Peer Assessment 1
##
#############################################################################################

##  Load required packages
library(knitr)
library(lattice)

##  Save old work directory and set work directory '/Reproducible_research/RepData_PeerAssessment1'
old_wd <- getwd()
if (length(grep("RepData_PeerAssessment1", old_wd)) == 0){
        setwd(normalizePath(paste0(old_wd, "/Reproducible_research/RepData_PeerAssessment1"), winslash = "\\"))
}
Sys.setlocale("LC_ALL", "English")

##  global setup options for knitr
opts_chunk$set(message=FALSE, fig.width = 6, fig.height = 6)


##  extract ZIP and load CSV
if(!file.exists("activity.csv")) {
        message('unzip activity.zip...')
        unzip("activity.zip")
}
raw_data <- read.csv("activity.csv", header = TRUE)
clear_data <- na.omit(raw_data)

##  stepsPerDay
stepsPerDay <- aggregate(clear_data$steps, list(clear_data$date),sum, na.rm = TRUE)
colnames(stepsPerDay) <- c('date','num_steps')

## plotting histogram
png(file = ".\\figure\\plot1.png", width = 800, height = 800)
hist(stepsPerDay$num_steps,
     xlab = 'steps per day',
     main = 'histogram of total number of steps taken per day')
dev.off()

##  mean and median of steps taken per day
median_StepsPerDay  <- median(stepsPerDay$num_steps, na.rm = TRUE)
mean_StepsPerDay  <- mean(stepsPerDay$num_steps, na.rm = TRUE)

##  average number of steps taken
avg_StepsInterval <- aggregate(clear_data$steps, list(clear_data$interval), mean, na.rm = TRUE)
colnames(avg_StepsInterval) <- c('interval', 'num_steps')
png(file = ".\\figure\\plot2.png", width = 800, height = 800)
plot(avg_StepsInterval$interval, avg_StepsInterval$num_steps,
     type = 'l',
     xlab = 'Interval',
     ylab = 'Number of steps',
     main = 'The average daily activity pattern') 
dev.off()

##  maximum number of steps in the interval
max_StepsInterval <- avg_StepsInterval$interval[which.max(avg_StepsInterval$num_steps)]

##  number of rows with NA values
num_rowsNA <- sum(!complete.cases(raw_data))

##  substituting the number of steps columns with NA values with average number of steps for the given interval
new_stepsPerDay <- merge(raw_data,avg_StepsInterval, by = 'interval' ) 
new_stepsPerDay$steps[is.na(new_stepsPerDay$steps)] <- new_stepsPerDay$num_steps[is.na(new_stepsPerDay$steps)]
new_stepsPerDay <- new_stepsPerDay[,c(1:3)]

##  sum of steps across different dates
total_stepsPerDay <- aggregate(new_stepsPerDay$steps,list(new_stepsPerDay$date),sum, na.rm = TRUE)
colnames(total_stepsPerDay) <- c('date','num_steps')

##  Plotting the histogram
png(file = ".\\figure\\plot3.png", width = 800, height = 800)
barplot(total_stepsPerDay$num_steps,
        names.arg = total_stepsPerDay$date,
        main = 'total number of steps taken each day (imputed data)',
        xlab = 'date',
        ylab = 'number of steps')
dev.off()

##  mean and median of steps taken per day (imputed)
mean_total_stepsPerDay <- mean(total_stepsPerDay$num_steps)
median_total_stepsPerDay <- median(total_stepsPerDay$num_steps)

##  weekday/weekend
new_stepsPerDay$date <- as.Date(new_stepsPerDay$date,format = '%Y-%m-%d')
new_stepsPerDay$day <- ifelse(weekdays(new_stepsPerDay$date) %in% c('Saturday','Sunday'), 'weekend', 'weekday')

## make a panel time series plot
panel_weekday <- aggregate(steps ~ interval, data = new_stepsPerDay[new_stepsPerDay$day=="weekday",],mean)
panel_weekend <- aggregate(steps ~ interval, data = new_stepsPerDay[new_stepsPerDay$day=="weekend",],mean)

png(file = ".\\figure\\plot4.png", width = 800, height = 800)
par(mfrow = c(2,1))
plot(panel_weekday,
     type = "l",
     xlab = "intervals",
     ylab = "average steps/interval",
     main = "weekdays")
plot(panel_weekend,
     type = "l",
     xlab = "intervals",
     ylab = "average steps/interval",
     main = "weekends")
dev.off()

##  Restore work directory
setwd(normalizePath(gsub("/Reproducible_research/RepData_PeerAssessment1", "", getwd()), winslash = "\\"))
## Restore Locale
Sys.setlocale("LC_ALL", "Russian")