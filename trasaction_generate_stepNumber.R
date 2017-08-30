# build up transaction for Association Data Mining
# aggrication by day , variable = HKQuantityTypeIdentifierstepCount
step_day <- aggregate(as.numeric(df_StepCount$value) , by = list(date = as.Date(df_StepCount$creationDate)), FUN=sum)
names(step_day) <- c("date","step")
weekNo <- as.integer(format(as.Date(step_day$date),"%U"))+1
library(stringr)
weekNo <- str_pad(weekNo, 2, pad = "0")
year <- format(as.Date(step_day$date),"%Y")
step_day <- cbind(step_day,weekNo)
step_day <- cbind(step_day,year)
names(step_day) <- c("date","step","week","year")

# set up intervals of step
step_A <- c(0, 2500*1)
step_B <- c(2500*1, 2500*2)
step_C <- c(2500*2, 2500*3)
step_D <- c(2500*3, 2500*4)
step_E <- c(2500*4, 2500*5)
step_F <- c(2500*5, 2500*6)
step_G <- c(2500*6, 2500*7)
step_H <- c(2500*7, 2500*8)
step_I <- c(2500*8, 2500*9)
step_J <- c(2500*9, 2500*10)
step_K <- c(2500*10, 2500*11)
step_L <- c(2500*11, 2500*1000)

 for(i in 1:nrow(step_day)) {
   step = step_day[i,2]
   if(step>=step_A[1] && step<step_A[2]) {step_day[i,5] <-"A"}
   if(step>=step_B[1] && step<step_B[2]) {step_day[i,5] <-"B"}
   if(step>=step_C[1] && step<step_C[2]) {step_day[i,5] <-"C"}
   if(step>=step_D[1] && step<step_D[2]) {step_day[i,5] <-"D"}
   if(step>=step_E[1] && step<step_E[2]) {step_day[i,5] <-"E"}
   if(step>=step_F[1] && step<step_F[2]) {step_day[i,5] <-"F"}
   if(step>=step_G[1] && step<step_G[2]) {step_day[i,5] <-"G"}
   if(step>=step_H[1] && step<step_H[2]) {step_day[i,5] <-"H"}
   if(step>=step_I[1] && step<step_I[2]) {step_day[i,5] <-"I"}
   if(step>=step_J[1] && step<step_J[2]) {step_day[i,5] <-"J"}
   if(step>=step_K[1] && step<step_K[2]) {step_day[i,5] <-"K"}
   if(step>=step_L[1] ) {step_day[i,5] <-"L"}
 }
names(step_day) <- c("date","step","week","year","step_indication")
step_indication <- factor(step_day$step_indication,ordered = TRUE)
barplot(prop.table(table(step_indication)))

step_week <- aggregate(as.numeric(step_day$step) ,
                                         by = list(week =step_day$week,year=step_day$year ), FUN=mean)
for(i in 1:nrow(step_week)) {
  step = step_week[i,3]
  if(step>=step_A[1] && step<step_A[2]) {step_week[i,4] <-"A"}
  if(step>=step_B[1] && step<step_B[2]) {step_week[i,4] <-"B"}
  if(step>=step_C[1] && step<step_C[2]) {step_week[i,4] <-"C"}
  if(step>=step_D[1] && step<step_D[2]) {step_week[i,4] <-"D"}
  if(step>=step_E[1] && step<step_E[2]) {step_week[i,4] <-"E"}
  if(step>=step_F[1] && step<step_F[2]) {step_week[i,4] <-"F"}
  if(step>=step_G[1] && step<step_G[2]) {step_week[i,4] <-"G"}
  if(step>=step_H[1] && step<step_H[2]) {step_week[i,4] <-"H"}
  if(step>=step_I[1] && step<step_I[2]) {step_week[i,4] <-"I"}
  if(step>=step_J[1] && step<step_J[2]) {step_week[i,4] <-"J"}
  if(step>=step_K[1] && step<step_K[2]) {step_week[i,4] <-"K"}
  if(step>=step_L[1] ) {step_day[i,4] <-"L"}
}
names(step_week) <- c("week","year","mean","step_indication")
step_indication <- factor(step_week$step_indication,ordered = TRUE)
barplot(prop.table(table(step_indication)))
plot(factor(paste(step_week$year,step_week$week)),step_week$mean,las="2",type ="n")
lines(factor(paste(step_week$year,step_week$week)),step_week$mean,type ="l") 
