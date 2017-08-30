# build up transaction for Association Data Mining
# aggrication by day , variable = HKQuantityTypeIdentifierBodyFatPercentage
BodyFatPercentage_day <- aggregate(as.numeric(df_BodyFatPercentage$value) ,
                             by = list(date = as.Date(df_BodyFatPercentage$creationDate)), FUN=mean)
names(BodyFatPercentage_day) <- c("date","BodyFatPercentage")
weekNo <- as.integer(format(as.Date(BodyFatPercentage_day$date),"%U"))+1
year <- format(as.Date(BodyFatPercentage_day$date),"%Y")
BodyFatPercentage_day <- cbind(BodyFatPercentage_day,weekNo)
BodyFatPercentage_day <- cbind(BodyFatPercentage_day,year)
names(BodyFatPercentage_day) <- c("date","BodyFatPercentage","week number","year")
max(df_BodyFatPercentage$value)
min(df_BodyFatPercentage$value)
# set up intervals of BodyFatPercentage
BodyFatPercentage_A <- c(0.14, 0.14+0.003)
BodyFatPercentage_B <- c(0.14+0.003, 0.14+0.003*2)
BodyFatPercentage_C <- c(0.14+0.003*2, 0.14+0.003*3)
BodyFatPercentage_D <- c(0.14+0.003*3, 0.14+0.003*4)
BodyFatPercentage_E <- c(0.14+0.003*4, 0.14+0.003*5)
BodyFatPercentage_F <- c(0.14+0.003*5, 0.14+0.003*6)
BodyFatPercentage_G <- c(0.14+0.003*6, 0.14+0.003*7)
BodyFatPercentage_H <- c(0.14+0.003*7, 0.14+0.003*8)
BodyFatPercentage_I <- c(0.14+0.003*8, 0.14+0.003*9)
BodyFatPercentage_J <- c(0.14+0.003*9, 0.14+0.003*10)
BodyFatPercentage_K <- c(0.14+0.003*10, 0.14+0.003*11)
BodyFatPercentage_L <- c(0.14+0.003*11, 0.14+0.003*12)

for(i in 1:nrow(BodyFatPercentage_day)) {
  BodyFatPercentage = BodyFatPercentage_day[i,2]
  if(BodyFatPercentage>=BodyFatPercentage_A[1] && BodyFatPercentage<BodyFatPercentage_A[2]) {BodyFatPercentage_day[i,5] <-"A"}
  if(BodyFatPercentage>=BodyFatPercentage_B[1] && BodyFatPercentage<BodyFatPercentage_B[2]) {BodyFatPercentage_day[i,5] <-"B"}
  if(BodyFatPercentage>=BodyFatPercentage_C[1] && BodyFatPercentage<BodyFatPercentage_C[2]) {BodyFatPercentage_day[i,5] <-"C"}
  if(BodyFatPercentage>=BodyFatPercentage_D[1] && BodyFatPercentage<BodyFatPercentage_D[2]) {BodyFatPercentage_day[i,5] <-"D"}
  if(BodyFatPercentage>=BodyFatPercentage_E[1] && BodyFatPercentage<BodyFatPercentage_E[2]) {BodyFatPercentage_day[i,5] <-"E"}
  if(BodyFatPercentage>=BodyFatPercentage_F[1] && BodyFatPercentage<BodyFatPercentage_F[2]) {BodyFatPercentage_day[i,5] <-"F"}
  if(BodyFatPercentage>=BodyFatPercentage_G[1] && BodyFatPercentage<BodyFatPercentage_G[2]) {BodyFatPercentage_day[i,5] <-"G"}
  if(BodyFatPercentage>=BodyFatPercentage_H[1] && BodyFatPercentage<BodyFatPercentage_H[2]) {BodyFatPercentage_day[i,5] <-"H"}
  if(BodyFatPercentage>=BodyFatPercentage_I[1] && BodyFatPercentage<BodyFatPercentage_I[2]) {BodyFatPercentage_day[i,5] <-"I"}
  if(BodyFatPercentage>=BodyFatPercentage_J[1] && BodyFatPercentage<BodyFatPercentage_J[2]) {BodyFatPercentage_day[i,5] <-"J"}
  if(BodyFatPercentage>=BodyFatPercentage_K[1] && BodyFatPercentage<BodyFatPercentage_K[2]) {BodyFatPercentage_day[i,5] <-"K"}
  if(BodyFatPercentage>=BodyFatPercentage_L[1] ) {BodyFatPercentage_day[i,5] <-"L"}
}
names(BodyFatPercentage_day) <- c("date","BodyFatPercentage","week number","year","BodyFatPercentage_indication")
BodyFatPercentage_indication <- factor(BodyFatPercentage_day$BodyFatPercentage_indication,ordered = TRUE)
barplot(prop.table(table(BodyFatPercentage_indication)))


BodyFatPercentage_week <- aggregate(as.numeric(BodyFatPercentage_day$BodyFatPercentage) ,
                              by = list(week =BodyFatPercentage_day$week,year=BodyFatPercentage_day$year ), FUN=mean)
for(i in 1:nrow(BodyFatPercentage_week)) {
  BodyFatPercentage = BodyFatPercentage_week[i,3]
  if(BodyFatPercentage>=BodyFatPercentage_A[1] && BodyFatPercentage<BodyFatPercentage_A[2]) {BodyFatPercentage_week[i,4] <-"A"}
  if(BodyFatPercentage>=BodyFatPercentage_B[1] && BodyFatPercentage<BodyFatPercentage_B[2]) {BodyFatPercentage_week[i,4] <-"B"}
  if(BodyFatPercentage>=BodyFatPercentage_C[1] && BodyFatPercentage<BodyFatPercentage_C[2]) {BodyFatPercentage_week[i,4] <-"C"}
  if(BodyFatPercentage>=BodyFatPercentage_D[1] && BodyFatPercentage<BodyFatPercentage_D[2]) {BodyFatPercentage_week[i,4] <-"D"}
  if(BodyFatPercentage>=BodyFatPercentage_E[1] && BodyFatPercentage<BodyFatPercentage_E[2]) {BodyFatPercentage_week[i,4] <-"E"}
  if(BodyFatPercentage>=BodyFatPercentage_F[1] && BodyFatPercentage<BodyFatPercentage_F[2]) {BodyFatPercentage_week[i,4] <-"F"}
  if(BodyFatPercentage>=BodyFatPercentage_G[1] && BodyFatPercentage<BodyFatPercentage_G[2]) {BodyFatPercentage_week[i,4] <-"G"}
  if(BodyFatPercentage>=BodyFatPercentage_H[1] && BodyFatPercentage<BodyFatPercentage_H[2]) {BodyFatPercentage_week[i,4] <-"H"}
  if(BodyFatPercentage>=BodyFatPercentage_I[1] && BodyFatPercentage<BodyFatPercentage_I[2]) {BodyFatPercentage_week[i,4] <-"I"}
  if(BodyFatPercentage>=BodyFatPercentage_J[1] && BodyFatPercentage<BodyFatPercentage_J[2]) {BodyFatPercentage_week[i,4] <-"J"}
  if(BodyFatPercentage>=BodyFatPercentage_K[1] && BodyFatPercentage<BodyFatPercentage_K[2]) {BodyFatPercentage_week[i,4] <-"K"}
  if(BodyFatPercentage>=BodyFatPercentage_L[1] ) {BodyFatPercentage_day[i,4] <-"L"}
}
names(BodyFatPercentage_week) <- c("week","year","BodyFatPercentage","BodyFatPercentage_indication")
BodyFatPercentage_indication <- factor(BodyFatPercentage_week$BodyFatPercentage_indication,ordered = TRUE)
barplot(prop.table(table(BodyFatPercentage_indication)))









