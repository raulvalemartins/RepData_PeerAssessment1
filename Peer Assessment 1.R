library(data.table)
library(dplyr)
library(ggplot2)
library(scales)


## Read data
df <- fread("activity.csv", header = TRUE, sep = ",", data.table = FALSE )
ad <- tbl_df(df)

## clean data
good <- complete.cases(ad)
ad <- ad[good,]

## calculate outputs
grouped_ad <- group_by ( ad, date)
sum_ad <- summarize(grouped_ad, stepsperday = sum(steps))
sum_ad <- mutate(sum_ad, datePOSIXct = as.POSIXct (date))

## graph output
g <- ggplot(sum_ad, aes(x=datePOSIXct, y=stepsperday ))
g <- g + geom_histogram( stat = "identity", fill = "darkgreen" ) +
        ggtitle("Total Number of Steps Taken per Day") +
        xlab( "Days") +
        ylab( "Number of Steps") +
        theme(plot.title stepsperinterval= element_text(size = rel(1.7))) +
        scale_x_datetime(breaks = date_breaks("5 days"),
                        labels = date_format("%b-%d"),
                        limits = c(as.POSIXct("2012-10-02"), 
                                   as.POSIXct("2012-11-29")) )
print(g)


grouped_ad <- group_by ( ad, date)
sum_ad <- summarize(grouped_ad, stepsperday = sum(steps))
reported_ad <- summarize(sum_ad, meanstepsperday = mean(stepsperday), 
                         medianstepperday = median(stepsperday))

grouped_ad <- group_by ( ad, interval)
averaged_ad <-  summarize(grouped_ad, avgstepsperinterval = mean(steps))
g <- ggplot(averaged_ad, aes(x=interval, y=avgstepsperinterval, group = 1 ))
g <- g + geom_line(stat = "identity", size = 1.2, colour = "darkgreen") + 
        ylab("Average of Steps Taken") +
        xlab("Interval") +
        ggtitle("Average of Steps Taken per Interval of 5 minutes") +
        theme(plot.title = element_text(size = rel(1.7)))
print(g)

averaged_ad[averaged_ad$avgstepsperinterval == max(averaged_ad$avgstepsperinterval), 1 ]


ad <- tbl_df(df)
good <- complete.cases(ad)
totalnas <- nrow(ad[!good,] )

adgood <- ad[good,]
adnotgood <- ad[!good,]
grouped_ad <- group_by ( adgood, interval)
meaninterval <- summarize(grouped_ad, stepsperinterval = trunc(mean(steps, rm.na = TRUE), 0))
for ( i in seq_len( nrow(adnotgood) )) {
        intervalinanalize <- as.character(adnotgood[i, 3])
        steps <- meaninterval[meaninterval$interval == intervalinanalize, 2]
        adnotgood[i, 1] <- steps
}
newad <- rbind( adgood, adnotgood)
Sys.setlocale("LC_TIME", "C")
newad$weekend <- "weekday"
newad$weekend[weekdays(as.POSIXct(newad$date)) %in% c("Saturday","Sunday")] <- "weekend"
newad$weekend <- as.factor(newad$weekend)
grouped_ad <- group_by ( newad, interval, weekend)
sum_ad <- summarize(grouped_ad, avgsteps = trunc(mean(steps, rm.na = TRUE), 0))

g <- ggplot(sum_ad, aes(x=interval, y=avgsteps ))
g <- g + geom_line( facets = . ~ weekend )+ ##stat = "identity", size = 1.2, colour = "darkgreen") +
        ylab("Average of Steps Taken") +
        xlab("Interval") +
        ggtitle("Average of Steps Taken per Interval of 5 minutes") +
        theme(plot.title = element_text(size = rel(1.7)))
print(g)





qplot(x = interval, y = avgsteps, data = sum_ad, geom = c("line"), 
      facets = weekend~., main = "Average Number of Steps Taken", 
      ylab = "Average Number of Steps")


