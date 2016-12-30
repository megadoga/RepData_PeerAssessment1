#Loading and preprocessing the data
#==================================
rawfile <- read.csv(file = "activity.csv")
#Format the character column to represent class "Date"
rawfile$date <- as.Date(rawfile$date,format="%Y-%m-%d")

#Mean total number of steps taken per day
#========================================
#Calculate the total steps taken per day
steps_sum <- aggregate(steps ~ date ,data = rawfile, sum,na.action = NULL)
#Histogram of total number of steps per day
#------------------------------------------
#Define the plot styles
par(#Set to one plot per window
    mfrow=c(1,1),
    # Change the colors
    col.main="black", col.lab="dark blue",
    # Titles in italic and bold
    font.main=2, font.lab=4,
    # Change font size
    cex.main=1.5, cex.lab=1, cex.axis=0.8,
    # End style of the lines
    lend = 2)
#Plot the histogram
hist(steps_sum$steps, 
     breaks = 20,
     main = "Total number of steps vs frequent days",
     xlab = "Total number of steps",
     ylab = "Days frequency", 
     col = "dark grey",
     lwd = 2)

#Calculate the mean/median total steps taken per day
#---------------------------------------------
#Mean total number of steps taken per day
total_steps_mean <- mean(steps_sum$steps,na.rm = TRUE)
mean_print<-paste("The total MEAN steps taken per day is:",total_steps_mean)
print(mean_print)
#Median total number of steps taken per day
total_steps_median <- median(steps_sum$steps,na.rm = TRUE)
median_print<-paste("The total MEDIAN steps taken per day is:",total_steps_median)
print(median_print)

#Averaged daily activity pattern
#==============================
#Calculate the number of steps taken in each 5 min interval averaged accross all days
daily_pattern<-aggregate(steps ~ interval, data = rawfile, mean)
#Plot the averaged steps time-series
plot(daily_pattern,type = "l",
     main = "Average number of steps across all days",
     xlab = "5 minutes interval",
     ylab = "Average number of steps", 
     col = "dark red")
#Find the 5 minute interval which has the maximum number of steps
#----------------------------------------------------------------
max_steps_interval<-daily_pattern$interval[daily_pattern$steps == max(daily_pattern$steps)]
max_steps<-paste("The maximum number os steps in a 5 minute interval is",max_steps_interval,"steps")
print(max_steps)

#Imputing missing values
#=======================
#Calculate the total number of rows with NAs
NA_rows<-is.na(rawfile$steps)
total_rows_na<-length(rawfile$steps[NA_rows])
#Calculate the number of NAs per date
na_per_day<-aggregate(steps ~ date,rawfile,na.action=NULL,function(x){sum(is.na(x))})
#   Strategy for filling the missing values: 
#   adopt the median value of the overall total number of steps per day
#
#Creation of the new dataset filling the NA's by the total mean steps per day
newdata<-rawfile
#Define the new total steps taken per day
intervals_per_day<-max(newdata$interval,na.rm=TRUE)
mean_interval_steps<-total_steps_mean/intervals_per_day
newdata$steps[is.na(newdata$steps)]<-mean_interval_steps
steps_sum_new <- aggregate(steps ~ date ,data = newdata, sum)
#Plot the histogram
hist(steps_sum_new$steps, 
     breaks = 20,
     main = "Total number of steps vs frequent days - NEW",
     xlab = "Total number of steps",
     ylab = "Days frequency", 
     col = "dark grey",
     lwd = 2)
#Calculate the total mean/median steps taken per day
#---------------------------------------------
#steps_mean <- aggregate(steps ~ date, data = rawfile, mean)
total_steps_mean_new <- mean(steps_sum_new$steps,na.rm = TRUE)
mean_print<-paste("The NEW total MEAN steps taken per day is:",total_steps_mean_new)
print(mean_print)
#Total median steps taken per day
total_steps_median_new <- median(steps_sum_new$steps,na.rm = TRUE)
median_print<-paste("The NEW total MEDIAN steps taken per day is:",total_steps_median_new)
print(median_print)

#Differences between weekdays and weekends
#=========================================
#Set the weekdays and weekends in the new data set
#Find the weekdays and the weekends among the dates
week_vector<-weekdays(newdata$date)
day_type<-rep("weekday",length(week_vector))
for(i in 1:length(week_vector)){
    if(week_vector[i]=="Sunday"){day_type[i]<-"weekend"}
    if(week_vector[i]=="Saturday"){day_type[i]<-"weekend"}
}
#Merge the type of day vector with the original dataset
newdata_w<-cbind(newdata,day_type)

#Calculate the number of steps taken in each 5 min interval averaged accross all WEEKDAYS
weekdays_pattern<-aggregate(steps ~ interval, 
                            data = newdata_w[newdata_w$day_type=="weekday",], mean)
#Calculate the number of steps taken in each 5 min interval averaged accross all WEEKDEND
weekends_pattern<-aggregate(steps ~ interval, 
                            data = newdata_w[newdata_w$day_type=="weekend",], mean)
#Plot the averaged steps time-series
par(#Define plot panel
    mfrow=c(2,1),
    # Change the colors
    col.main="black", col.lab="dark blue",
    # Titles in italic and bold
    font.main=2, font.lab=4,
    # Change font size
    cex.main=1, cex.lab=0.8, cex.axis=0.8,
    # End style of the lines
    lend = 2)
#Plot the averaged WEEKDAYS steps time-series
plot(weekdays_pattern,type = "l",
     main = "Average number of steps across all WEEKDAYS",
     xlab = "5 minutes interval",
     ylab = "Average number of steps", 
     col = "dark red")
#Plot the averaged WEEKENDS steps time-series
plot(weekends_pattern,type = "l",
     main = "Average number of steps across all WEEKENDS",
     xlab = "5 minutes interval",
     ylab = "Average number of steps", 
     col = "dark red")
