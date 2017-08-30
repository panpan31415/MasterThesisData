# build up transaction for Association Data Mining
# aggrication by day , variable = HKQuantityTypeIdentifierDistanceWalkingRunning

DistanceWalkingRunning_day <- aggregate(as.numeric(df_DistanceWalkingRunning$value) , by = list(date = as.Date(df_DistanceWalkingRunning$creationDate)), FUN=sum)
names(DistanceWalkingRunning_day) <- c("date","DistanceWalkingRunning")
weekNo <- as.integer(format(as.Date(DistanceWalkingRunning_day$date),"%U"))+1
year <- format(as.Date(DistanceWalkingRunning_day$date),"%Y")
DistanceWalkingRunning_day <- cbind(DistanceWalkingRunning_day,weekNo)
DistanceWalkingRunning_day <- cbind(DistanceWalkingRunning_day,year)
names(DistanceWalkingRunning_day) <- c("date","Distance","week","year")
max(DistanceWalkingRunning_day$DistanceWalkingRunning)
min(DistanceWalkingRunning_day$DistanceWalkingRunning)
# set up intervals of DistanceWalkingRunning
DistanceWalkingRunning_A <- c(0, 2*1)
DistanceWalkingRunning_B <- c(2*1, 2*2)
DistanceWalkingRunning_C <- c(2*2, 2*3)
DistanceWalkingRunning_D <- c(2*3, 2*4)
DistanceWalkingRunning_E <- c(2*4, 2*5)
DistanceWalkingRunning_F <- c(2*5, 2*6)
DistanceWalkingRunning_G <- c(2*6, 2*7)
DistanceWalkingRunning_H <- c(2*7, 2*8)
DistanceWalkingRunning_I <- c(2*8, 2*9)
DistanceWalkingRunning_J <- c(2*9, 2*10)
DistanceWalkingRunning_K <- c(2*10, 2*11)
DistanceWalkingRunning_L <- c(2*11, 2*1000)

for(i in 1:nrow(DistanceWalkingRunning_day)) {
  DistanceWalkingRunning = DistanceWalkingRunning_day[i,2]
  if(DistanceWalkingRunning>=DistanceWalkingRunning_A[1] && DistanceWalkingRunning<DistanceWalkingRunning_A[2]) {DistanceWalkingRunning_day[i,5] <-"A"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_B[1] && DistanceWalkingRunning<DistanceWalkingRunning_B[2]) {DistanceWalkingRunning_day[i,5] <-"B"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_C[1] && DistanceWalkingRunning<DistanceWalkingRunning_C[2]) {DistanceWalkingRunning_day[i,5] <-"C"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_D[1] && DistanceWalkingRunning<DistanceWalkingRunning_D[2]) {DistanceWalkingRunning_day[i,5] <-"D"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_E[1] && DistanceWalkingRunning<DistanceWalkingRunning_E[2]) {DistanceWalkingRunning_day[i,5] <-"E"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_F[1] && DistanceWalkingRunning<DistanceWalkingRunning_F[2]) {DistanceWalkingRunning_day[i,5] <-"F"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_G[1] && DistanceWalkingRunning<DistanceWalkingRunning_G[2]) {DistanceWalkingRunning_day[i,5] <-"G"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_H[1] && DistanceWalkingRunning<DistanceWalkingRunning_H[2]) {DistanceWalkingRunning_day[i,5] <-"H"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_I[1] && DistanceWalkingRunning<DistanceWalkingRunning_I[2]) {DistanceWalkingRunning_day[i,5] <-"I"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_J[1] && DistanceWalkingRunning<DistanceWalkingRunning_J[2]) {DistanceWalkingRunning_day[i,5] <-"J"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_K[1] && DistanceWalkingRunning<DistanceWalkingRunning_K[2]) {DistanceWalkingRunning_day[i,5] <-"k"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_L[1] ) {DistanceWalkingRunning_day[i,5] <-"L"}
}
names(DistanceWalkingRunning_day) <- c("date","Distance","week","year","DistanceWalkingRunning_indication")
DistanceWalkingRunning_indication <- factor(DistanceWalkingRunning_day$DistanceWalkingRunning_indication,ordered = TRUE)
barplot(prop.table(table(DistanceWalkingRunning_indication)))


DistanceWalkingRunning_week <- aggregate(as.numeric(DistanceWalkingRunning_day$Distance) ,
                            by = list(week =DistanceWalkingRunning_day$week,year=DistanceWalkingRunning_day$year ), FUN=mean)
for(i in 1:nrow(DistanceWalkingRunning_week)) {
  DistanceWalkingRunning = DistanceWalkingRunning_week[i,3]
  if(DistanceWalkingRunning>=DistanceWalkingRunning_A[1] && DistanceWalkingRunning<DistanceWalkingRunning_A[2]) {DistanceWalkingRunning_week[i,4] <-"A"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_B[1] && DistanceWalkingRunning<DistanceWalkingRunning_B[2]) {DistanceWalkingRunning_week[i,4] <-"B"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_C[1] && DistanceWalkingRunning<DistanceWalkingRunning_C[2]) {DistanceWalkingRunning_week[i,4] <-"C"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_D[1] && DistanceWalkingRunning<DistanceWalkingRunning_D[2]) {DistanceWalkingRunning_week[i,4] <-"D"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_E[1] && DistanceWalkingRunning<DistanceWalkingRunning_E[2]) {DistanceWalkingRunning_week[i,4] <-"E"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_F[1] && DistanceWalkingRunning<DistanceWalkingRunning_F[2]) {DistanceWalkingRunning_week[i,4] <-"F"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_G[1] && DistanceWalkingRunning<DistanceWalkingRunning_G[2]) {DistanceWalkingRunning_week[i,4] <-"G"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_H[1] && DistanceWalkingRunning<DistanceWalkingRunning_H[2]) {DistanceWalkingRunning_week[i,4] <-"H"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_I[1] && DistanceWalkingRunning<DistanceWalkingRunning_I[2]) {DistanceWalkingRunning_week[i,4] <-"I"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_J[1] && DistanceWalkingRunning<DistanceWalkingRunning_J[2]) {DistanceWalkingRunning_week[i,4] <-"J"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_K[1] && DistanceWalkingRunning<DistanceWalkingRunning_K[2]) {DistanceWalkingRunning_week[i,4] <-"K"}
  if(DistanceWalkingRunning>=DistanceWalkingRunning_L[1] ) {DistanceWalkingRunning_day[i,4] <-"L"}
}
names(DistanceWalkingRunning_week) <- c("week","year","mean","DistanceWalkingRunning_indication")
DistanceWalkingRunning_indication <- factor(DistanceWalkingRunning_week$DistanceWalkingRunning_indication,ordered = TRUE)
barplot(prop.table(table(DistanceWalkingRunning_indication)))



