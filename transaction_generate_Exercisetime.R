# build up transaction for Association Data Mining
# aggrication by day , variable = HKQuantityTypeIdentifierAppleExerciseTime

ExerciseTime_day <- aggregate(as.numeric(df_ExerciseTime$value) , by = list(date = as.Date(df_ExerciseTime$creationDate)), FUN=sum)
names(ExerciseTime_day) <- c("date","ExerciseTime")
weekNo <- as.integer(format(as.Date(ExerciseTime_day$date),"%U"))+1
year <- format(as.Date(ExerciseTime_day$date),"%Y")
ExerciseTime_day <- cbind(ExerciseTime_day,weekNo)
ExerciseTime_day <- cbind(ExerciseTime_day,year)
names(ExerciseTime_day) <- c("date","ExerciseTime","week","year")
max(ExerciseTime_day$ExerciseTime)
min(ExerciseTime_day$ExerciseTime)
# set up intervals of ExerciseTime
ExerciseTime_A <- c(0, 10)
ExerciseTime_B <- c(10*1, 10*2)
ExerciseTime_C <- c(10*2, 10*3)
ExerciseTime_D <- c(10*3, 10*4)
ExerciseTime_E <- c(10*4, 10*5)
ExerciseTime_F <- c(10*5, 10*6)
ExerciseTime_G <- c(10*6, 10*7)
ExerciseTime_H <- c(10*7, 10*8)
ExerciseTime_I <- c(10*8, 10*9)
ExerciseTime_J <- c(10*9, 10*10)
ExerciseTime_K <- c(10*10, 10*11)
ExerciseTime_L <- c(10*11, 10*1000)

for(i in 1:nrow(ExerciseTime_day)) {
  ExerciseTime = ExerciseTime_day[i,2]
  if(ExerciseTime>=ExerciseTime_A[1] && ExerciseTime<ExerciseTime_A[2]) {ExerciseTime_day[i,5] <-"A"}
  if(ExerciseTime>=ExerciseTime_B[1] && ExerciseTime<ExerciseTime_B[2]) {ExerciseTime_day[i,5] <-"B"}
  if(ExerciseTime>=ExerciseTime_C[1] && ExerciseTime<ExerciseTime_C[2]) {ExerciseTime_day[i,5] <-"C"}
  if(ExerciseTime>=ExerciseTime_D[1] && ExerciseTime<ExerciseTime_D[2]) {ExerciseTime_day[i,5] <-"D"}
  if(ExerciseTime>=ExerciseTime_E[1] && ExerciseTime<ExerciseTime_E[2]) {ExerciseTime_day[i,5] <-"E"}
  if(ExerciseTime>=ExerciseTime_F[1] && ExerciseTime<ExerciseTime_F[2]) {ExerciseTime_day[i,5] <-"F"}
  if(ExerciseTime>=ExerciseTime_G[1] && ExerciseTime<ExerciseTime_G[2]) {ExerciseTime_day[i,5] <-"G"}
  if(ExerciseTime>=ExerciseTime_H[1] && ExerciseTime<ExerciseTime_H[2]) {ExerciseTime_day[i,5] <-"H"}
  if(ExerciseTime>=ExerciseTime_I[1] && ExerciseTime<ExerciseTime_I[2]) {ExerciseTime_day[i,5] <-"I"}
  if(ExerciseTime>=ExerciseTime_J[1] && ExerciseTime<ExerciseTime_J[2]) {ExerciseTime_day[i,5] <-"J"}
  if(ExerciseTime>=ExerciseTime_K[1] && ExerciseTime<ExerciseTime_K[2]) {ExerciseTime_day[i,5] <-"K"}
  if(ExerciseTime>=ExerciseTime_L[1] ) {ExerciseTime_day[i,5] <-"L"}
}
names(ExerciseTime_day) <- c("date","ExerciseTime","week","year","ExerciseTime_indication")
ExerciseTime_indication <- factor(ExerciseTime_day$ExerciseTime_indication,ordered = TRUE)
barplot(prop.table(table(ExerciseTime_indication)))

ExerciseTime_week <- aggregate(as.numeric(ExerciseTime_day$ExerciseTime) ,
                            by = list(week =ExerciseTime_day$week,year=ExerciseTime_day$year ), FUN=mean)
for(i in 1:nrow(ExerciseTime_week)) {
  ExerciseTime = ExerciseTime_week[i,3]
  if(ExerciseTime>=ExerciseTime_A[1] && ExerciseTime<ExerciseTime_A[2]) {ExerciseTime_week[i,4] <-"A"}
  if(ExerciseTime>=ExerciseTime_B[1] && ExerciseTime<ExerciseTime_B[2]) {ExerciseTime_week[i,4] <-"B"}
  if(ExerciseTime>=ExerciseTime_C[1] && ExerciseTime<ExerciseTime_C[2]) {ExerciseTime_week[i,4] <-"C"}
  if(ExerciseTime>=ExerciseTime_D[1] && ExerciseTime<ExerciseTime_D[2]) {ExerciseTime_week[i,4] <-"D"}
  if(ExerciseTime>=ExerciseTime_E[1] && ExerciseTime<ExerciseTime_E[2]) {ExerciseTime_week[i,4] <-"E"}
  if(ExerciseTime>=ExerciseTime_F[1] && ExerciseTime<ExerciseTime_F[2]) {ExerciseTime_week[i,4] <-"F"}
  if(ExerciseTime>=ExerciseTime_G[1] && ExerciseTime<ExerciseTime_G[2]) {ExerciseTime_week[i,4] <-"G"}
  if(ExerciseTime>=ExerciseTime_H[1] && ExerciseTime<ExerciseTime_H[2]) {ExerciseTime_week[i,4] <-"H"}
  if(ExerciseTime>=ExerciseTime_I[1] && ExerciseTime<ExerciseTime_I[2]) {ExerciseTime_week[i,4] <-"I"}
  if(ExerciseTime>=ExerciseTime_J[1] && ExerciseTime<ExerciseTime_J[2]) {ExerciseTime_week[i,4] <-"J"}
  if(ExerciseTime>=ExerciseTime_K[1] && ExerciseTime<ExerciseTime_K[2]) {ExerciseTime_week[i,4] <-"K"}
  if(ExerciseTime>=ExerciseTime_L[1] ) {ExerciseTime_day[i,4] <-"L"}
}
names(ExerciseTime_week) <- c("week","year","mean","ExerciseTime_indication")
ExerciseTime_indication <- factor(ExerciseTime_week$ExerciseTime_indication,ordered = TRUE)
barplot(prop.table(table(ExerciseTime_indication)))



