#split different types of variables for later analysis
df_StandHour <- subset(df,df$type == "HKCategoryTypeIdentifierAppleStandHour")
df_MindfulSession <- subset(df,df$type == "HKCategoryTypeIdentifierMindfulSession")
df_SleepAnalysis <- subset(df,df$type == "HKCategoryTypeIdentifierSleepAnalysis")
df_ActiveEnergyBurned <- subset(df,df$type == "HKQuantityTypeIdentifierActiveEnergyBurned")
df_ExerciseTime <- subset(df,df$type == "HKQuantityTypeIdentifierAppleExerciseTime")
df_BasalEnergyBurned <- subset(df,df$type == "HKQuantityTypeIdentifierBasalEnergyBurned")
df_BodyFatPercentage <- subset(df,df$type == "HKQuantityTypeIdentifierBodyFatPercentage")
df_BodyMass <- subset(df,df$type == "HKQuantityTypeIdentifierBodyMass")
df_BodyMassIndex <- subset(df,df$type == "HKQuantityTypeIdentifierBodyMassIndex")
df_DistanceSwimming <- subset(df,df$type == "HKQuantityTypeIdentifierDistanceSwimming")
df_DistanceWalkingRunning <- subset(df,df$type == "HKQuantityTypeIdentifierDistanceWalkingRunning")
df_HeartRate <- subset(df,df$type == "HKQuantityTypeIdentifierHeartRate")
df_Height <- subset(df,df$type == "HKQuantityTypeIdentifierHeight")
df_LeanBodyMass <- subset(df,df$type == "HKQuantityTypeIdentifierLeanBodyMass")
df_StepCount <- subset(df,df$type == "HKQuantityTypeIdentifierStepCount")
df_SwimmingStrokeCount <- subset(df,df$type == "HKQuantityTypeIdentifierSwimmingStrokeCount")
