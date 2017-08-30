# build up transaction for Association Data Mining
# aggrication by day , variable = HKQuantityTypeIdentifierBodyMassIndexIndex
BodyMassIndex_day <- aggregate(as.numeric(df_BodyMassIndex$value) ,
                          by = list(date = as.Date(df_BodyMassIndex$creationDate)), FUN=mean)
names(BodyMassIndex_day) <- c("date","BodyMassIndex")
weekNo <- as.integer(format(as.Date(BodyMassIndex_day$date),"%U"))+1
year <- format(as.Date(BodyMassIndex_day$date),"%Y")
BodyMassIndex_day <- cbind(BodyMassIndex_day,weekNo)
BodyMassIndex_day <- cbind(BodyMassIndex_day,year)
names(BodyMassIndex_day) <- c("date","BodyMassIndex","week number","year")
max(df_BodyMassIndex$value)
min(df_BodyMassIndex$value)
# set up intervals of BodyMassIndex
BodyMassIndex_A <- c(24, 24+0.12555)
BodyMassIndex_B <- c(24+0.1255, 24+0.1255*2)
BodyMassIndex_C <- c(24+0.1255*2, 24+0.1255*3)
BodyMassIndex_D <- c(24+0.1255*3, 24+0.1255*4)
BodyMassIndex_E <- c(24+0.1255*4, 24+0.1255*5)
BodyMassIndex_F <- c(24+0.1255*5, 24+0.1255*6)
BodyMassIndex_G <- c(24+0.1255*6, 24+0.1255*7)
BodyMassIndex_H <- c(24+0.1255*7, 24+0.1255*8)
BodyMassIndex_I <- c(24+0.1255*8, 24+0.1255*9)
BodyMassIndex_J <- c(24+0.1255*9, 24+0.1255*10)
BodyMassIndex_K <- c(24+0.1255*10, 24+0.1255*11)
BodyMassIndex_L <- c(24+0.1255*11, 24+0.1255*1200)

for(i in 1:nrow(BodyMassIndex_day)) {
  BodyMassIndex = BodyMassIndex_day[i,2]
  if(BodyMassIndex>=BodyMassIndex_A[1] && BodyMassIndex<BodyMassIndex_A[2]) {BodyMassIndex_day[i,5] <-"A"}
  if(BodyMassIndex>=BodyMassIndex_B[1] && BodyMassIndex<BodyMassIndex_B[2]) {BodyMassIndex_day[i,5] <-"B"}
  if(BodyMassIndex>=BodyMassIndex_C[1] && BodyMassIndex<BodyMassIndex_C[2]) {BodyMassIndex_day[i,5] <-"C"}
  if(BodyMassIndex>=BodyMassIndex_D[1] && BodyMassIndex<BodyMassIndex_D[2]) {BodyMassIndex_day[i,5] <-"D"}
  if(BodyMassIndex>=BodyMassIndex_E[1] && BodyMassIndex<BodyMassIndex_E[2]) {BodyMassIndex_day[i,5] <-"E"}
  if(BodyMassIndex>=BodyMassIndex_F[1] && BodyMassIndex<BodyMassIndex_F[2]) {BodyMassIndex_day[i,5] <-"F"}
  if(BodyMassIndex>=BodyMassIndex_G[1] && BodyMassIndex<BodyMassIndex_G[2]) {BodyMassIndex_day[i,5] <-"G"}
  if(BodyMassIndex>=BodyMassIndex_H[1] && BodyMassIndex<BodyMassIndex_H[2]) {BodyMassIndex_day[i,5] <-"H"}
  if(BodyMassIndex>=BodyMassIndex_I[1] && BodyMassIndex<BodyMassIndex_I[2]) {BodyMassIndex_day[i,5] <-"I"}
  if(BodyMassIndex>=BodyMassIndex_J[1] && BodyMassIndex<BodyMassIndex_J[2]) {BodyMassIndex_day[i,5] <-"J"}
  if(BodyMassIndex>=BodyMassIndex_K[1] && BodyMassIndex<BodyMassIndex_K[2]) {BodyMassIndex_day[i,5] <-"K"}
  if(BodyMassIndex>=BodyMassIndex_L[1] ) {BodyMassIndex_day[i,5] <-"L"}
}
names(BodyMassIndex_day) <- c("date","BodyMassIndex","week","year","BMI_indication")
BodyMassIndex_indication <- factor(BodyMassIndex_day$BMI_indication,ordered = TRUE)
barplot(prop.table(table(BodyMassIndex_indication)))

BodyMassIndex_week <- aggregate(as.numeric(BodyMassIndex_day$BodyMassIndex) ,
                                    by = list(week =BodyMassIndex_day$week,year=BodyMassIndex_day$year ), FUN=mean)
for(i in 1:nrow(BodyMassIndex_week)) {
  BodyMassIndex = BodyMassIndex_week[i,3]
  if(BodyMassIndex>=BodyMassIndex_A[1] && BodyMassIndex<BodyMassIndex_A[2]) {BodyMassIndex_week[i,4] <-"A"}
  if(BodyMassIndex>=BodyMassIndex_B[1] && BodyMassIndex<BodyMassIndex_B[2]) {BodyMassIndex_week[i,4] <-"B"}
  if(BodyMassIndex>=BodyMassIndex_C[1] && BodyMassIndex<BodyMassIndex_C[2]) {BodyMassIndex_week[i,4] <-"C"}
  if(BodyMassIndex>=BodyMassIndex_D[1] && BodyMassIndex<BodyMassIndex_D[2]) {BodyMassIndex_week[i,4] <-"D"}
  if(BodyMassIndex>=BodyMassIndex_E[1] && BodyMassIndex<BodyMassIndex_E[2]) {BodyMassIndex_week[i,4] <-"E"}
  if(BodyMassIndex>=BodyMassIndex_F[1] && BodyMassIndex<BodyMassIndex_F[2]) {BodyMassIndex_week[i,4] <-"F"}
  if(BodyMassIndex>=BodyMassIndex_G[1] && BodyMassIndex<BodyMassIndex_G[2]) {BodyMassIndex_week[i,4] <-"G"}
  if(BodyMassIndex>=BodyMassIndex_H[1] && BodyMassIndex<BodyMassIndex_H[2]) {BodyMassIndex_week[i,4] <-"H"}
  if(BodyMassIndex>=BodyMassIndex_I[1] && BodyMassIndex<BodyMassIndex_I[2]) {BodyMassIndex_week[i,4] <-"I"}
  if(BodyMassIndex>=BodyMassIndex_J[1] && BodyMassIndex<BodyMassIndex_J[2]) {BodyMassIndex_week[i,4] <-"J"}
  if(BodyMassIndex>=BodyMassIndex_K[1] && BodyMassIndex<BodyMassIndex_K[2]) {BodyMassIndex_week[i,4] <-"K"}
  if(BodyMassIndex>=BodyMassIndex_L[1] ) {BodyMassIndex_week[i,4] <-"L"}
}
names(BodyMassIndex_week) <- c("week","year","BodyMassIndex","BMI_indication")
BodyMassIndex_indication <- factor(BodyMassIndex_week$BMI_indication,ordered = TRUE)
barplot(prop.table(table(BodyMassIndex_indication)))


