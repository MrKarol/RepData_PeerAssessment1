library(dplyr)
library(ggplot2)

unzip("repdata-data-activity.zip")
Activity <- read.csv("activity.csv")

#t1 contains sum of number of steps per day
t1 <- Activity %>% group_by(date) %>% summarise(NumOfSteps=sum(steps,na.rm=TRUE))

#png("figure/HistogramOfTheTotalNumberOfStepsTakenEachDay.png") #png device
#f1 is a histogram
f1 <- ggplot(t1, aes(x = factor(date), y = NumOfSteps)) + theme_bw() + geom_bar(stat="identity") 
f1 <- f1 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
f1 <- f1 + ylab(" total number of steps taken each day") + xlab("date")
#dev.off()
f1

print.data.frame(
    t1 %>% summarise(
            MeanOfTotalNumOfStepsPerDay = mean(NumOfSteps),
            MedianOfTotalNumOfStepsPerDay = median(NumOfSteps)
           )
)
cat("Mean: ",toString(t1 %>% summarise(MeanOfTotalNumOfStepsPerDay = mean(NumOfSteps))))
cat("Median: ",toString(t1 %>% summarise(MedianOfTotalNumOfStepsPerDay = median(NumOfSteps))))

#t2 contains avg number of steps per interval
t2 <- Activity %>% group_by(interval) %>% summarise(AvgNumOfSteps=mean(steps,na.rm=TRUE))

f2 <- ggplot(t2, aes(interval,AvgNumOfSteps)) + theme_bw() + geom_line() 
f2 <- f2 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
f2 <- f2 + ylab(" avergage number of steps per day across all days") + xlab("interval")
f2

interval_with_highest_avg <- max(t2$AvgNumOfSteps)
print.data.frame(
    t2 %>% filter(AvgNumOfSteps==interval_with_highest_avg)
)
cat('Interval with highest average: ',toString((t2 %>% filter(AvgNumOfSteps==interval_with_highest_avg))[1]))

print.data.frame(
    Activity %>% filter(is.na(steps)) %>% summarise(MissingValues=n())
)
cat('Number of missing values: ',toString((Activity %>% filter(is.na(steps)) %>% summarise(MissingValues=n()))))

Activity2 <- Activity %>% filter(is.na(steps)) 
Activity2 <- left_join(Activity2,t2)
Activity2 <- select(Activity2, -steps)
Activity2 <- left_join(Activity,Activity2)
Activity2 <- Activity2 %>% mutate(steps_cleaned = ifelse(is.na(steps),AvgNumOfSteps,steps))
Activity2 <- select(Activity2,date,interval,steps_cleaned,steps) 

t3 <- Activity2 %>% group_by(date) %>% summarise(NumOfSteps=sum(steps,na.rm=TRUE),NumOfStepsCleaned=sum(steps_cleaned))

print.data.frame(
    t3 %>% summarise(
        Original_MeanOfTotalNumOfStepsPerDay = mean(NumOfSteps),
        Original_MedianOfTotalNumOfStepsPerDay = median(NumOfSteps),
        NAsPopulated_MeanOfTotalNumOfStepsPerDay = mean(NumOfStepsCleaned),
        NAsPopulated_MedianOfTotalNumOfStepsPerDay = median(NumOfStepsCleaned)
    )
)
cat('Original data set mean: ', toString(t3 %>% summarise(Original_MeanOfTotalNumOfStepsPerDay = mean(NumOfSteps))))
cat('Original data set median: ', toString(t3 %>% summarise(Original_MedianOfTotalNumOfStepsPerDay = median(NumOfSteps))))
cat('New data set mean: ', toString(t3 %>% summarise(NAsPopulated_MeanOfTotalNumOfStepsPerDay = mean(NumOfStepsCleaned))))
cat('New data set median: ', toString(t3 %>% summarise(NAsPopulated_MedianOfTotalNumOfStepsPerDay = median(NumOfStepsCleaned))))

t3 <- with(t3,
        data.frame(
            date = rep(date, 2),
            dataset = factor(rep(c("Original","New"), each = NROW(t3))),
            value = c(NumOfSteps,NumOfStepsCleaned)    
        )
    )

f3 <- ggplot(t3, aes(date, value)) + geom_bar(aes(fill = dataset), position = "dodge", stat="identity")+ theme_bw() 
f3 <- f3 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
f3 <- f3 + ylab("total number of steps taken each day") + xlab("date") 
f3

#add new variable: IsWeekday
Activity2 <- Activity2 %>% mutate(IsWeekday = ifelse(weekdays(as.Date(date))=="Saturday" | weekdays(as.Date(date))=="Sunday","Weekend","Weekday"))

#prepare data.frame that will be used in the chart f4
t4 <- Activity2 %>% group_by(interval,IsWeekday) %>% summarise(AvgNumOfSteps=mean(steps_cleaned,na.rm=TRUE))

f4 <- qplot(interval,AvgNumOfSteps, data=t4, geom=c("line")) + 
    theme_bw() + 
    ggtitle("The average number of steps taken, averaged across all weekday days or weekend days, per 5-minute interval (New data set") + 
    facet_wrap(~IsWeekday,nrow=2)  + 
    ylab("Average number of stpes taken")
f4