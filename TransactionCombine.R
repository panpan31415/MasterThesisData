#Load libaries
library(Matrix)
library(arules)

# Combinition of transaction
rm(Transaction_InnerJoin)
Transaction_InnerJoin <- merge(procrastination_week,step_week, by=c("year","week"),all = TRUE)
Transaction_InnerJoin <- merge(Transaction_InnerJoin,ActiveEnergy_week, by=c("year","week"),all = TRUE)
Transaction_InnerJoin <- merge(Transaction_InnerJoin,BasalEnergy_week, by=c("year","week"),all = TRUE)
Transaction_InnerJoin <- merge(Transaction_InnerJoin,BodyFatPercentage_week, by=c("year","week"),all = TRUE)
Transaction_InnerJoin <- merge(Transaction_InnerJoin,BodyMass_week, by=c("year","week"),all = TRUE)
Transaction_InnerJoin <- merge(Transaction_InnerJoin,BodyMassIndex_week, by=c("year","week"),all = TRUE)
Transaction_InnerJoin <- merge(Transaction_InnerJoin,LeanBodyMass_week, by=c("year","week"),all = TRUE)
Transaction_InnerJoin <- merge(Transaction_InnerJoin,StandHour_week, by=c("year","week"),all = TRUE)
Transaction_InnerJoin <- merge(Transaction_InnerJoin,DistanceWalkingRunning_week, by=c("year","week"),all = TRUE)
Transaction_InnerJoin <- merge(Transaction_InnerJoin,SleepAnalysis_week, by=c("year","week"),all = TRUE)
Transaction_InnerJoin <- merge(Transaction_InnerJoin,ExerciseTime_week, by=c("year","week"),all = TRUE)
Transaction_InnerJoin <- merge(Transaction_InnerJoin,HeartRate_week, by=c("year","week"),all = TRUE)


new_names = c("year","week","date","Procra_value","step_mean","step_indication"
                                                 ,"ActiveEnergy_mean","ActiveEnergy_indication"
                                                 ,"BasalEnergy_mean","BasalEnergy_indication"
                                                 ,"BodyFatPercentage_mean","BodyFatPercentage_indication"
                                                 ,"BodyMass_mean","BodyMass_indication"
                                                 ,"BMI_mean","BMI_indication"
                                                 ,"LeanBodyMass_mean","LeanBodyMass_indication"
                                                 ,"StandHour_mean","StandHour_indication"
                                                 ,"DistanceWalkingRunning_mean","DistanceWalkingRunning_indication"
                                                 ,"SleepAnalysis_mean","SleepAnalysis_indication"
                                                 ,"ExerciseTime_mean","ExerciseTime_indication"
                                                 ,"HeartRate_mean","HeartRate_indication"
              )
names(Transaction_InnerJoin) = new_names

Procra_value <- Transaction_InnerJoin$Procra_value
step_indication <- Transaction_InnerJoin$step_indication
ActiveEnergy_indication <- Transaction_InnerJoin$ActiveEnergy_indication
BasalEnergy_indication <- Transaction_InnerJoin$BasalEnergy_indication
BodyFatPercentage_indication <- Transaction_InnerJoin$BodyFatPercentage_indication
BodyMass_indication <- Transaction_InnerJoin$BodyMass_indication
BodyMassIndex_indication <- Transaction_InnerJoin$BMI_indication
LeanBodyMass_indication <- Transaction_InnerJoin$LeanBodyMass_indication
StandHour_indication <- Transaction_InnerJoin$StandHour_indication
DistanceWalkingRunning_indication <- Transaction_InnerJoin$DistanceWalkingRunning_indication
SleepAnalysis_indication <- Transaction_InnerJoin$SleepAnalysis_indication
ExerciseTime_indication <- Transaction_InnerJoin$ExerciseTime_indication
HeartRate <- Transaction_InnerJoin$HeartRate_indication
df_transaction <- data.frame(
                              Procra_value = as.factor(Procra_value),
                              step_indication = as.factor(step_indication),
                              ActiveEnergy_indication = as.factor(ActiveEnergy_indication),
                              BasalEnergy_indication = as.factor(BasalEnergy_indication),
                              BodyFatPercentage_indication = as.factor(BodyFatPercentage_indication),
                              BodyMass_indication = as.factor(BodyMass_indication),
                              BodyMassIndex_indication = as.factor(BodyMassIndex_indication),
                              LeanBodyMass_indication = as.factor(LeanBodyMass_indication),
                              StandHour_indication = as.factor(StandHour_indication),
                              DistanceWalkingRunning_indication = as.factor(DistanceWalkingRunning_indication),
                              SleepAnalysis_indication =as.factor(SleepAnalysis_indication),
                              ExerciseTime_indication = as.factor(ExerciseTime_indication),
                              HeartRate = as.factor(HeartRate))

transaction <- as(df_transaction,"transactions")
#as(transaction,"data.frame")
inspect(transaction)

rules <- apriori(transaction,parameter = list(supp = 0.01,conf = 0.01, target = "rules"))
class(rules)
summary(rules)
cat("association rules", capture.output(inspect(rules)), file="/Users/panpan31415/Desktop/rule.txt", sep = "\n", append = TRUE)

df_rules = data.frame(
                        lhs = labels(lhs(rules)),
                        rhs = labels(rhs(rules)),
                        rules@quality )

head(df_rules)
# ## example 3: creating transactions from data.frame
# a_df <- data.frame(
#   age   = as.factor(c(6, 8, NA, 9, 16,10,10,10,10)),
#   grade = as.factor(c("A", "C", "F", NA, "C", "C", "C", "C", "C")),
#   pass  = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE))
# ## note: factors are translated differently to logicals and NAs are ignored
# a_df
# 
# ## coerce
# trans3 <- as(a_df, "transactions")
# inspect(trans3)
# a_rules <- apriori(a_df)
# as(trans3, "data.frame")
# inspect(a_rules)
