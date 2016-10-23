library(data.table)
library(ggplot2)


dt_act<-data.table(read.csv("activity.csv"))

dt_totalsteps<-dt_act[,sum(steps,na.rm = TRUE),by=date]

names(dt_totalsteps)<-c("date","total_steps")

hist_totalsteps<-ggplot(data=dt_totalsteps,aes(total_steps))
hist_totalsteps<-hist_totalsteps+geom_histogram()+xlab("total steps")
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


dt_new_totalsteps<-dt_act[,sum(steps),by=date]
names(dt_new_totalsteps)<-c("date","new_total_steps")


data_summary_2<-summary(dt_new_totalsteps$new_total_steps)
print(data_summary_2)

hist_new_totalsteps<-ggplot(data=dt_new_totalsteps,aes(new_total_steps))
hist_new_totalsteps<-hist_new_totalsteps+geom_histogram()+xlab("total steps")
print(hist_new_totalsteps)
