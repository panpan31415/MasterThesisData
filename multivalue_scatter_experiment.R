library(corrplot) # We'll use corrplot later on in this example too.
library(visreg) # This library will allow us to show multivariate graphs.
library(rgl)
library(knitr)
library(scatterplot3d)

df_procra_step_sleep_exercise_stand <- Transaction_InnerJoin[,
                                                             c("Procra_value",
                                                               "step_mean",
                                                               "SleepAnalysis_mean",
                                                               "ExerciseTime_mean",
                                                               "StandHour_mean",
                                                               "ActiveEnergy_mean",
                                                               "BasalEnergy_mean",
                                                               "BodyFatPercentage_mean",
                                                               "BodyMass_mean",
                                                               "BMI_mean",
                                                               "LeanBodyMass_mean",
                                                               "DistanceWalkingRunning_mean",
                                                               "HeartRate_mean"
                                                               )]
df_procra_step_sleep_exercise_stand[,1] <- as.double(df_procra_step_sleep_exercise_stand[,1])
df_procra_step_sleep_exercise_stand[,2] <- as.double(df_procra_step_sleep_exercise_stand[,2])
df_procra_step_sleep_exercise_stand[,3] <- as.double(df_procra_step_sleep_exercise_stand[,3])
df_procra_step_sleep_exercise_stand[,4] <- as.double(df_procra_step_sleep_exercise_stand[,4])
df_procra_step_sleep_exercise_stand[,5] <- as.double(df_procra_step_sleep_exercise_stand[,5])
df_procra_step_sleep_exercise_stand[,6] <- as.double(df_procra_step_sleep_exercise_stand[,6])
df_procra_step_sleep_exercise_stand[,7] <- as.double(df_procra_step_sleep_exercise_stand[,7])
df_procra_step_sleep_exercise_stand[,8] <- as.double(df_procra_step_sleep_exercise_stand[,8])
df_procra_step_sleep_exercise_stand[,9] <- as.double(df_procra_step_sleep_exercise_stand[,9])
df_procra_step_sleep_exercise_stand[,10] <- as.double(df_procra_step_sleep_exercise_stand[,10])
df_procra_step_sleep_exercise_stand[,11] <- as.double(df_procra_step_sleep_exercise_stand[,11])
df_procra_step_sleep_exercise_stand[,12] <- as.double(df_procra_step_sleep_exercise_stand[,12])
df_procra_step_sleep_exercise_stand[,13] <- as.double(df_procra_step_sleep_exercise_stand[,13])
cor( use = "complete.obs" , df_procra_step_sleep_exercise_stand) 

step_mean.c <- scale(df_procra_step_sleep_exercise_stand$step_mean, center = TRUE,scale = FALSE)
SleepAnalysis_mean.c <- scale(df_procra_step_sleep_exercise_stand$SleepAnalysis_mean ,center = TRUE,scale = FALSE)
ExerciseTime_mean.c <- scale(df_procra_step_sleep_exercise_stand$ExerciseTime_mean , center = TRUE,scale = FALSE)
StandHour_mean.c <- scale(df_procra_step_sleep_exercise_stand$StandHour_mean , center = TRUE,scale = FALSE)
c.vars <- cbind(step_mean.c,SleepAnalysis_mean.c,ExerciseTime_mean.c,StandHour_mean.c)
df_procra_step_sleep_exercise_stand_c.vars <- cbind(df_procra_step_sleep_exercise_stand,c.vars)
names(df_procra_step_sleep_exercise_stand_c.vars)[1:17] = c("Procra","Step","Sleep","Exercise","Stand","AEnergy",
                                                            "BEnergy","BodyFat","BodyMass","BMI","LBodyMass","Distance",
                                                            "HeartRate","step.c","sleep.c","exercise.c","standhour.c")

y = df_procra_step_sleep_exercise_stand_c.vars$Procra
model <- lm(y ~ step_mean.c + SleepAnalysis_mean.c + ExerciseTime_mean.c +StandHour_mean.c ,data = df_procra_step_sleep_exercise_stand_c.vars)
summary(model)


newdatacor = cor(df_procra_step_sleep_exercise_stand_c.vars[1:13],use = "complete.obs")
corrplot(newdatacor, method = "circle")
summary(newdatacor)
