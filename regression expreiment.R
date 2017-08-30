##########Procrastination & Step Regression##########
x <- Transaction_InnerJoin$step_mean
y <- Transaction_InnerJoin$Procra_value
summary(lm(y ~ x))
# plot(lm(y ~ x),col="red",pch=16, which=1)
plot(x,y,col = "blue",main = "Procrastination & Step Regression",
     abline(lm(y~x)),cex = 1.0,pch = 16,xlab = "Step Count",ylab = "Procrastination Scale")
##########Step & Distance Regression##########
x <- Transaction_InnerJoin$step_mean
y <- Transaction_InnerJoin$DistanceWalkingRunning_mean
summary(lm(y ~ x))
plot(x,y,col = "blue",main = "Step & Distance Regression",
     abline(lm(y~x)),cex = 1.0,pch = 16,ylab = "Distance km",xlab = "Step")

##########Step & Distance Regression##########
x <- Transaction_InnerJoin$StandHour_mean
y <- Transaction_InnerJoin$Procra_value
summary(lm(y ~ x))

plot(x,y,col = "blue",main = "Procrastination & StandHour",
     abline(lm(y~x)),cex = 1.0,pch = 16,xlab = "StandHour (miniute)",ylab = "Procrastination Scale")
##########Step & Sleep ##########
x <- Transaction_InnerJoin$SleepAnalysis_mean
y <- Transaction_InnerJoin$Procra_value
summary(lm(y ~ x))
plot(x,y,col = "blue",main = "Procrastination & SleepAnalysis",
     abline(lm(y~x)),cex = 1.0,pch = 16,xlab = "Sleep (minute)",ylab = "Procrastination Scale")

##########Step & Sleep ##########
x <- Transaction_InnerJoin$ExerciseTime_mean
y <- Transaction_InnerJoin$Procra_value
summary(lm(y ~ x))

plot(x,y,col = "blue",main = "Procrastination & ExerciseTime",
     abline(lm(y~x)),cex = 1.0,pch = 16,xlab = "ExerciseTime",ylab = "Procrastination Scale")
x= c(1,2,3,4,5,6,7,8,9,10)
y= c(2,3,4,5,6,7,8,9,10,12)
plot(x,y,col = "blue",main = "Procra & SleepAnalysis",
     abline(lm(y~x)),cex = 1.0,pch = 16,xlab = "SleepAnalysis",ylab = "Procrastination Scale")
summary(lm(x ~ y))