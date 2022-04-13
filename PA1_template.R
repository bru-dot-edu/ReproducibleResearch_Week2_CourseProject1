#load libraries
library(ggplot2)
library(dplyr)
library(knitr)

#Code for reading in the dataset and/or processing the data
unzip("repdata_data_activity.zip")
df <- read.csv("activity.csv")

#Mean, median, and histogram of daily steps
q1 <- df %>% group_by(date) %>% summarize(steps=sum(steps, na.rm=TRUE))
ggplot(q1, aes(x = steps)) + 
        geom_histogram()
mean(q1$steps)
median(q1$steps)

#time series plot and average steps per day
q2 <- df %>% group_by(interval) %>% summarize(mean=mean(steps, na.rm=TRUE))
ggplot(q2, aes(x=interval, y = mean)) +
        geom_line()
print(q2[which.max(q2$mean),]$interval)

#Code to describe and show a strategy for imputing missing data
a <- nrow(df)
b <- nrow(na.omit(df))
print(a-b)

#fill in missing data with average from each 5 minute interval
#q2 is what we reference in the loop 

filled_df <- df
for (j in 1:nrow(filled_df)){
        if (is.na(df$steps[j])) {
                filled_df$steps[j] <- q2$mean[filled_df$interval[j] == q2$interval]
        }
}

#Mean, median, and histogram of daily steps after missing values are imputed
q_1 <- filled_df %>% group_by(date) %>% summarize(steps=sum(steps, na.rm=TRUE))
ggplot(q_1, aes(x = steps)) + 
        geom_histogram()
mean(q_1$steps)
median(q_1$steps)

#weekdays vs weekends comparison
day_df <- filled_df
day_df$date <- as.Date(day_df$date)
for (j in 1:nrow(day_df)){
        if (weekdays(day_df$date[j]) == "Saturday" |
            weekdays(day_df$date[j]) == "Sunday") {
                day_df$day[j] <- "Weekend" 
            } else {day_df$day[j] <- "Weekday" }
}


df_plot <- aggregate(steps ~ day + interval, day_df, mean)

ggplot(df_plot, aes(x = interval, y = steps)) +
        geom_line() + facet_wrap(~ day)
