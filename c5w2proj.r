library(data.table)
library(ggplot2)


dt_act<-data.table(read.csv("activity.csv"))
dt_totalsteps<-dt_act[,sum(steps,na.rm = TRUE),by=date]

names(dt_totalsteps)<-c("date","total_steps")

hist_totalsteps<-ggplot(data=dt_totalsteps,aes(total_steps))
hist_totalsteps<-hist_totalsteps+geom_histogram()
print(hist_totalsteps)

data_summary<-summary(dt_totalsteps$total_steps)
print(data_summary)

dt_time_steps<-dt_act[,step_mean:=mean(steps,na.rm=TRUE),by=interval]

g<-ggplot(data=dt_time_steps,aes(interval,step_mean))
g<-g+geom_line()+ylab("Average steps")
print(g)

print(c(max(dt_time_steps$step_mean),dt_time_steps$interval[which.max(dt_time_steps$step_mean)]))

print(sum(is.na(dt_act$steps)))

dt_time_steps[,steps:=replace(steps,is.na(steps),step_mean)]
dt_new_totalsteps<-dt_act[,sum(steps),by=date]
names(dt_new_totalsteps)<-c("date","New_total_steps")


data_summary_2<-summary(dt_new_totalsteps$New_total_steps)
print(data_summary_2)

#dt_filled_act<-
