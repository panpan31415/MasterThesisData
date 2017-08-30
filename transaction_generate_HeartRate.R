# build up transaction for Association Data Mining
# aggrication by day , variable = HKQuantityTypeIdentifierHeartRate

HeartRate_day <- aggregate(as.numeric(df_HeartRate$value) , by = list(date = as.Date(df_HeartRate$creationDate)), FUN=mean)
names(HeartRate_day) <- c("date","HeartRate")
weekNo <- as.integer(format(as.Date(HeartRate_day$date),"%U"))+1
year <- format(as.Date(HeartRate_day$date),"%Y")
HeartRate_day <- cbind(HeartRate_day,weekNo)
HeartRate_day <- cbind(HeartRate_day,year)
names(HeartRate_day) <- c("date","HeartRate","week","year")
max(HeartRate_day$HeartRate)
min(HeartRate_day$HeartRate)
# set up intervals of HeartRate
HeartRate_A <- c(50, 50+15*1)
HeartRate_B <- c(50+15*1, 50+15*2)
HeartRate_C <- c(50+15*2, 50+15*3)
HeartRate_D <- c(50+15*3, 50+15*4)
HeartRate_E <- c(50+15*4, 50+15*5)
HeartRate_F <- c(50+15*5, 50+15*6)
HeartRate_G <- c(50+15*6, 50+15*7)
HeartRate_H <- c(50+15*7, 50+15*8)
HeartRate_I <- c(50+15*8, 50+15*9)
HeartRate_J <- c(50+15*9, 50+15*10)
HeartRate_K <- c(50+15*10, 50+15*11)
HeartRate_L <- c(50+15*11, 50+15*50+1500)

for(i in 1:nrow(HeartRate_day)) {
  HeartRate = HeartRate_day[i,2]
  if(HeartRate>=HeartRate_A[1] && HeartRate<HeartRate_A[2]) {HeartRate_day[i,5] <-"A"}
  if(HeartRate>=HeartRate_B[1] && HeartRate<HeartRate_B[2]) {HeartRate_day[i,5] <-"B"}
  if(HeartRate>=HeartRate_C[1] && HeartRate<HeartRate_C[2]) {HeartRate_day[i,5] <-"C"}
  if(HeartRate>=HeartRate_D[1] && HeartRate<HeartRate_D[2]) {HeartRate_day[i,5] <-"D"}
  if(HeartRate>=HeartRate_E[1] && HeartRate<HeartRate_E[2]) {HeartRate_day[i,5] <-"E"}
  if(HeartRate>=HeartRate_F[1] && HeartRate<HeartRate_F[2]) {HeartRate_day[i,5] <-"F"}
  if(HeartRate>=HeartRate_G[1] && HeartRate<HeartRate_G[2]) {HeartRate_day[i,5] <-"G"}
  if(HeartRate>=HeartRate_H[1] && HeartRate<HeartRate_H[2]) {HeartRate_day[i,5] <-"H"}
  if(HeartRate>=HeartRate_I[1] && HeartRate<HeartRate_I[2]) {HeartRate_day[i,5] <-"I"}
  if(HeartRate>=HeartRate_J[1] && HeartRate<HeartRate_J[2]) {HeartRate_day[i,5] <-"J"}
  if(HeartRate>=HeartRate_K[1] && HeartRate<HeartRate_K[2]) {HeartRate_day[i,5] <-"K"}
  if(HeartRate>=HeartRate_L[1] ) {HeartRate_day[i,5] <-"L"}
}
names(HeartRate_day) <- c("date","HeartRate","week","year","HeartRate_indication")
HeartRate_indication <- factor(HeartRate_day$HeartRate_indication,ordered = TRUE)
barplot(prop.table(table(HeartRate_indication)))

HeartRate_week <- aggregate(as.numeric(HeartRate_day$HeartRate) ,
                               by = list(week =HeartRate_day$week,year=HeartRate_day$year ), FUN=mean)
for(i in 1:nrow(HeartRate_week)) {
  HeartRate = HeartRate_week[i,3]
  if(HeartRate>=HeartRate_A[1] && HeartRate<HeartRate_A[2]) {HeartRate_week[i,4] <-"A"}
  if(HeartRate>=HeartRate_B[1] && HeartRate<HeartRate_B[2]) {HeartRate_week[i,4] <-"B"}
  if(HeartRate>=HeartRate_C[1] && HeartRate<HeartRate_C[2]) {HeartRate_week[i,4] <-"C"}
  if(HeartRate>=HeartRate_D[1] && HeartRate<HeartRate_D[2]) {HeartRate_week[i,4] <-"D"}
  if(HeartRate>=HeartRate_E[1] && HeartRate<HeartRate_E[2]) {HeartRate_week[i,4] <-"E"}
  if(HeartRate>=HeartRate_F[1] && HeartRate<HeartRate_F[2]) {HeartRate_week[i,4] <-"F"}
  if(HeartRate>=HeartRate_G[1] && HeartRate<HeartRate_G[2]) {HeartRate_week[i,4] <-"G"}
  if(HeartRate>=HeartRate_H[1] && HeartRate<HeartRate_H[2]) {HeartRate_week[i,4] <-"H"}
  if(HeartRate>=HeartRate_I[1] && HeartRate<HeartRate_I[2]) {HeartRate_week[i,4] <-"I"}
  if(HeartRate>=HeartRate_J[1] && HeartRate<HeartRate_J[2]) {HeartRate_week[i,4] <-"J"}
  if(HeartRate>=HeartRate_K[1] && HeartRate<HeartRate_K[2]) {HeartRate_week[i,4] <-"K"}
  if(HeartRate>=HeartRate_L[1] ) {HeartRate_day[i,4] <-"L"}
}
names(HeartRate_week) <- c("week","year","mean","HeartRate_indication")
HeartRate_indication <- factor(HeartRate_week$HeartRate_indication,ordered = TRUE)
barplot(prop.table(table(HeartRate_indication)))



