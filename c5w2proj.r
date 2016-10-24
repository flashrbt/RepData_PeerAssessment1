library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)

dt_act<-data.table(read.csv("activity.csv"))

dt_totalsteps<-dt_act[,sum(steps,na.rm = TRUE),by=date]

names(dt_totalsteps)<-c("date","total_steps")

hist_totalsteps<-ggplot(data=dt_totalsteps,aes(total_steps))
hist_totalsteps<-hist_totalsteps+geom_histogram(bins = 40)+xlab("total steps")
print(hist_totalsteps)

data_summary<-summary(dt_totalsteps$total_steps)
print(data_summary)

dt_act[,step_mean:=mean(steps,na.rm=TRUE),by=interval]

g<-ggplot(data=dt_act,aes(interval,step_mean))
g<-g+geom_line()+ylab("Average steps")
print(g)

print(c(max(dt_act$step_mean),dt_act$interval[which.max(dt_act$step_mean)]))

print(sum(is.na(dt_act$steps)))

dt_act[,old_steps:=steps]



dt_act[,steps:=replace(steps,is.na(steps),step_mean)]

dt_new_act<-select(dt_act,steps,date,interval)


dt_new_totalsteps<-dt_new_act[,sum(steps),by=date]
names(dt_new_totalsteps)<-c("date","new_total_steps")


data_summary_2<-summary(dt_new_totalsteps$new_total_steps)
print(data_summary_2)

hist_new_totalsteps<-ggplot(data=dt_new_totalsteps,aes(new_total_steps))
hist_new_totalsteps<-hist_new_totalsteps+geom_histogram(bins = 40)+xlab("total steps")+ggtitle("Total steps per day with filled data")
print(hist_new_totalsteps)


weekdays= c("Monday","Tuesday","Wednesday","Thursday","Friday")

dt_new_act$weekday<-as.factor(ifelse(weekdays(as.Date(dt_new_act$date))%in%weekdays,"weekday","weekend"))

weekdaysplit<-ddply(dt_new_act, c("interval","weekday"),summarise,
                                     meansteps = mean(steps,na.rm=TRUE)
)
# weekdaysplit2<-dt_new_act[,.(meansteps=mean(steps,na.rm = TRUE)),by=.(interval,weekday)][,.(totalmeansteps=sum(meansteps)),by=.(interval,weekday)]

weekdayplot<-ggplot(weekdaysplit,aes(x=interval,y=meansteps))+
        facet_wrap(~weekday,nrow=2,ncol=1)+ geom_line()
weekdayplot<-weekdayplot+ ggtitle("Mean steps over each 5min interval split by weekday/weekend")+ylab("Average steps")+ xlab("Interval ")
print(weekdayplot)



