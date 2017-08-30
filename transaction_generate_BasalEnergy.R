# build up transaction for Association Data Mining
# aggrication by day , variable = HKQuantityTypeIdentifierBasalEnergyBurned
BasalEnergy_day <- aggregate(as.numeric(df_BasalEnergyBurned$value) ,
                             by = list(date = as.Date(df_BasalEnergyBurned$creationDate)), FUN=sum)
names(BasalEnergy_day) <- c("date","BasalEnergy(kcal)")
weekNo <- as.integer(format(as.Date(BasalEnergy_day$date),"%U"))+1
year <- format(as.Date(BasalEnergy_day$date),"%Y")
BasalEnergy_day <- cbind(BasalEnergy_day,weekNo)
BasalEnergy_day <- cbind(BasalEnergy_day,year)
names(BasalEnergy_day) <- c("date","BasalEnergy(kcal)","week number","year")
max(BasalEnergy_day$`BasalEnergy(kcal)`)
# set up intervals of BasalEnergy
BasalEnergy_A <- c(0, 500)
BasalEnergy_B <- c(500, 500*2)
BasalEnergy_C <- c(500*2, 500*3)
BasalEnergy_D <- c(500*3, 500*4)
BasalEnergy_E <- c(500*4, 500*5)
BasalEnergy_F <- c(500*5, 500*6)
BasalEnergy_G <- c(500*6, 500*7)
BasalEnergy_H <- c(500*7, 500*8)
BasalEnergy_I <- c(500*8, 500*9)
BasalEnergy_J <- c(500*9, 500*10)
BasalEnergy_K <- c(500*10, 500*11)
BasalEnergy_L <- c(500*11, 500*5000)

for(i in 1:nrow(BasalEnergy_day)) {
  BasalEnergy = BasalEnergy_day[i,2]
  if(BasalEnergy>=BasalEnergy_A[1] && BasalEnergy<BasalEnergy_A[2]) {BasalEnergy_day[i,5] <-"A"}
  if(BasalEnergy>=BasalEnergy_B[1] && BasalEnergy<BasalEnergy_B[2]) {BasalEnergy_day[i,5] <-"B"}
  if(BasalEnergy>=BasalEnergy_C[1] && BasalEnergy<BasalEnergy_C[2]) {BasalEnergy_day[i,5] <-"C"}
  if(BasalEnergy>=BasalEnergy_D[1] && BasalEnergy<BasalEnergy_D[2]) {BasalEnergy_day[i,5] <-"D"}
  if(BasalEnergy>=BasalEnergy_E[1] && BasalEnergy<BasalEnergy_E[2]) {BasalEnergy_day[i,5] <-"E"}
  if(BasalEnergy>=BasalEnergy_F[1] && BasalEnergy<BasalEnergy_F[2]) {BasalEnergy_day[i,5] <-"F"}
  if(BasalEnergy>=BasalEnergy_G[1] && BasalEnergy<BasalEnergy_G[2]) {BasalEnergy_day[i,5] <-"G"}
  if(BasalEnergy>=BasalEnergy_H[1] && BasalEnergy<BasalEnergy_H[2]) {BasalEnergy_day[i,5] <-"H"}
  if(BasalEnergy>=BasalEnergy_I[1] && BasalEnergy<BasalEnergy_I[2]) {BasalEnergy_day[i,5] <-"I"}
  if(BasalEnergy>=BasalEnergy_J[1] && BasalEnergy<BasalEnergy_J[2]) {BasalEnergy_day[i,5] <-"J"}
  if(BasalEnergy>=BasalEnergy_K[1] && BasalEnergy<BasalEnergy_K[2]) {BasalEnergy_day[i,5] <-"K"}
  if(BasalEnergy>=BasalEnergy_L[1] ) {BasalEnergy_day[i,5] <-"L"}
}
names(BasalEnergy_day) <- c("date","BasalEnergy(kcal)","week number","year","BasalEnergy_indication")
BasalEnergy_indication <- factor(BasalEnergy_day$BasalEnergy_indication,ordered = TRUE)
barplot(prop.table(table(BasalEnergy_indication)))


BasalEnergy_week <- aggregate(as.numeric(BasalEnergy_day$`BasalEnergy(kcal)`) ,
                           by = list(week =BasalEnergy_day$week,year=BasalEnergy_day$year ), FUN=mean)
for(i in 1:nrow(BasalEnergy_week)) {
  BasalEnergy = BasalEnergy_week[i,3]
  if(BasalEnergy>=BasalEnergy_A[1] && BasalEnergy<BasalEnergy_A[2]) {BasalEnergy_week[i,4] <-"A"}
  if(BasalEnergy>=BasalEnergy_B[1] && BasalEnergy<BasalEnergy_B[2]) {BasalEnergy_week[i,4] <-"B"}
  if(BasalEnergy>=BasalEnergy_C[1] && BasalEnergy<BasalEnergy_C[2]) {BasalEnergy_week[i,4] <-"C"}
  if(BasalEnergy>=BasalEnergy_D[1] && BasalEnergy<BasalEnergy_D[2]) {BasalEnergy_week[i,4] <-"D"}
  if(BasalEnergy>=BasalEnergy_E[1] && BasalEnergy<BasalEnergy_E[2]) {BasalEnergy_week[i,4] <-"E"}
  if(BasalEnergy>=BasalEnergy_F[1] && BasalEnergy<BasalEnergy_F[2]) {BasalEnergy_week[i,4] <-"F"}
  if(BasalEnergy>=BasalEnergy_G[1] && BasalEnergy<BasalEnergy_G[2]) {BasalEnergy_week[i,4] <-"G"}
  if(BasalEnergy>=BasalEnergy_H[1] && BasalEnergy<BasalEnergy_H[2]) {BasalEnergy_week[i,4] <-"H"}
  if(BasalEnergy>=BasalEnergy_I[1] && BasalEnergy<BasalEnergy_I[2]) {BasalEnergy_week[i,4] <-"I"}
  if(BasalEnergy>=BasalEnergy_J[1] && BasalEnergy<BasalEnergy_J[2]) {BasalEnergy_week[i,4] <-"J"}
  if(BasalEnergy>=BasalEnergy_K[1] && BasalEnergy<BasalEnergy_K[2]) {BasalEnergy_week[i,4] <-"K"}
  if(BasalEnergy>=BasalEnergy_L[1] ) {BasalEnergy_day[i,4] <-"L"}
}
names(BasalEnergy_week) <- c("week","year","BasalEnergy(kcal)","BasalEnergy_indication")
BasalEnergy_indication <- factor(BasalEnergy_week$BasalEnergy_indication,ordered = TRUE)
barplot(prop.table(table(BasalEnergy_indication)))






