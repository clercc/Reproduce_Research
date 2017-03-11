library(dplyr)

fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
fpath <- "~/Reprod_ResP1.zip"
download.file(fileURL, fpath)

raw.data <- read.csv(unz(fpath, "activity.csv"))

total.steps.day <- tapply(raw.data$steps, raw.data$date, sum)
hist(total.steps.day, main = "Histogram of Total Number of Steps Taken Each Day", xlab = "Total Steps")
mean(total.steps.day, na.rm = TRUE)
median(total.steps.day, na.rm = TRUE)

avg.steps.day <- tapply(raw.data$steps, raw.data$interval, mean, na.rm = TRUE)
plot(avg.steps.day ~ raw.data$interval[1:288], type = "l", main = "Daily Average Pattern", xlab = "5 Minute Interval", ylab = "Average Steps Per Day")
avg.steps.interval <- cbind(raw.data$interval[1:288],avg.steps.day)
x <- which.max(avg.steps.interval[,2])
avg.steps.interval[x]

length(raw.data[is.na(raw.data)])

processed.data <- raw.data
avg_steps <- rep(avg.steps.day, 61)
processed.data <- cbind(avg_steps, processed.data)

for (i in 1:nrow(processed.data)) {
        if (is.na(processed.data$steps[i])) {
                processed.data$steps[i] <- processed.data$avg_steps[i]
        } else {processed.data$steps[i] <- processed.data$steps[i]}
}
processed.data <- processed.data[c(2:4)]

total.steps.day <- tapply(processed.data$steps, processed.data$date, sum)
hist(total.steps.day, main = "Histogram of Total Number of Steps Taken Each Day", xlab = "Total Steps")
mean(total.steps.day)
median(total.steps.day)

wkdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
processed.data$date <- as.Date(processed.data$date)
processed.data$datetype <- factor((weekdays(processed.data$date) %in% wkdays), levels = c(FALSE, TRUE), labels = c('Weekend', 'Weekday'))
      

weekd <- processed.data %>% filter(datetype == "Weekday") %>% group_by(interval) %>% summarize(mean(steps))
weeke <- processed.data %>% filter(datetype == "Weekend") %>% group_by(interval) %>% summarize(mean(steps))

par(mfrow = c(2,1))
plot(weekd$`mean(steps)` ~ weekd$interval, type = "l", main = "Average Weekday Activity Pattern", xlab = "5 Minute Interval", ylab = "Average Steps Per Day")
plot(weeke$`mean(steps)` ~ weeke$interval, type = "l", main = "Average Weekend Activity Pattern", xlab = "5 Minute Interval", ylab = "Average Steps Per Day")
tapply(processed.data$steps, processed.data$datetype, mean)
