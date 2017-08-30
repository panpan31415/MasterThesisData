# build up transaction for Association Data Mining
# aggrication by day , variable = HKQuantityTypeIdentifierActiveEnergyBurned
ActiveEnergy_day <- aggregate(as.numeric(df_ActiveEnergyBurned$value) , by = list(date = as.Date(df_ActiveEnergyBurned$creationDate)), FUN=sum)
names(ActiveEnergy_day) <- c("date","ActiveEnergy")
weekNo <- as.integer(format(as.Date(ActiveEnergy_day$date),"%U"))+1
year <- format(as.Date(ActiveEnergy_day$date),"%Y")
ActiveEnergy_day <- cbind(ActiveEnergy_day,weekNo)
ActiveEnergy_day <- cbind(ActiveEnergy_day,year)
names(ActiveEnergy_day) <- c("date","ActiveEnergy(kcal)","week number","year")
max(ActiveEnergy_day$`ActiveEnergy(kcal)`)
# set up intervals of ActiveEnergy
ActiveEnergy_A <- c(0, 100)
ActiveEnergy_B <- c(100, 100*2)
ActiveEnergy_C <- c(100*2, 100*3)
ActiveEnergy_D <- c(100*3, 100*4)
ActiveEnergy_E <- c(100*4, 100*5)
ActiveEnergy_F <- c(100*5, 100*6)
ActiveEnergy_G <- c(100*6, 100*7)
ActiveEnergy_H <- c(100*7, 100*8)
ActiveEnergy_I <- c(100*8, 100*9)
ActiveEnergy_J <- c(100*9, 100*10)
ActiveEnergy_K <- c(100*10, 100*11)
ActiveEnergy_L <- c(100*11, 100*1000)

for(i in 1:nrow(ActiveEnergy_day)) {
  ActiveEnergy = ActiveEnergy_day[i,2]
  if(ActiveEnergy>=ActiveEnergy_A[1] && ActiveEnergy<ActiveEnergy_A[2]) {ActiveEnergy_day[i,5] <-"A"}
  if(ActiveEnergy>=ActiveEnergy_B[1] && ActiveEnergy<ActiveEnergy_B[2]) {ActiveEnergy_day[i,5] <-"B"}
  if(ActiveEnergy>=ActiveEnergy_C[1] && ActiveEnergy<ActiveEnergy_C[2]) {ActiveEnergy_day[i,5] <-"C"}
  if(ActiveEnergy>=ActiveEnergy_D[1] && ActiveEnergy<ActiveEnergy_D[2]) {ActiveEnergy_day[i,5] <-"D"}
  if(ActiveEnergy>=ActiveEnergy_E[1] && ActiveEnergy<ActiveEnergy_E[2]) {ActiveEnergy_day[i,5] <-"E"}
  if(ActiveEnergy>=ActiveEnergy_F[1] && ActiveEnergy<ActiveEnergy_F[2]) {ActiveEnergy_day[i,5] <-"F"}
  if(ActiveEnergy>=ActiveEnergy_G[1] && ActiveEnergy<ActiveEnergy_G[2]) {ActiveEnergy_day[i,5] <-"G"}
  if(ActiveEnergy>=ActiveEnergy_H[1] && ActiveEnergy<ActiveEnergy_H[2]) {ActiveEnergy_day[i,5] <-"H"}
  if(ActiveEnergy>=ActiveEnergy_I[1] && ActiveEnergy<ActiveEnergy_I[2]) {ActiveEnergy_day[i,5] <-"I"}
  if(ActiveEnergy>=ActiveEnergy_J[1] && ActiveEnergy<ActiveEnergy_J[2]) {ActiveEnergy_day[i,5] <-"J"}
  if(ActiveEnergy>=ActiveEnergy_K[1] && ActiveEnergy<ActiveEnergy_K[2]) {ActiveEnergy_day[i,5] <-"K"}
  if(ActiveEnergy>=ActiveEnergy_L[1] ) {ActiveEnergy_day[i,5] <-"L"}
}
names(ActiveEnergy_day) <- c("date","ActiveEnergy(kcal)","week number","year","ActiveEnergy_indication")
ActiveEnergy_indication <- factor(ActiveEnergy_day$ActiveEnergy_indication,ordered = TRUE)
barplot(prop.table(table(ActiveEnergy_indication)))

ActiveEnergy_week <- aggregate(as.numeric(ActiveEnergy_day$`ActiveEnergy(kcal)`) ,
                       by = list(week =ActiveEnergy_day$week,year=ActiveEnergy_day$year ), FUN=mean)
for(i in 1:nrow(ActiveEnergy_week)) {
  ActiveEnergy = ActiveEnergy_week[i,3]
  if(ActiveEnergy>=ActiveEnergy_A[1] && ActiveEnergy<ActiveEnergy_A[2]) {ActiveEnergy_week[i,4] <-"A"}
  if(ActiveEnergy>=ActiveEnergy_B[1] && ActiveEnergy<ActiveEnergy_B[2]) {ActiveEnergy_week[i,4] <-"B"}
  if(ActiveEnergy>=ActiveEnergy_C[1] && ActiveEnergy<ActiveEnergy_C[2]) {ActiveEnergy_week[i,4] <-"C"}
  if(ActiveEnergy>=ActiveEnergy_D[1] && ActiveEnergy<ActiveEnergy_D[2]) {ActiveEnergy_week[i,4] <-"D"}
  if(ActiveEnergy>=ActiveEnergy_E[1] && ActiveEnergy<ActiveEnergy_E[2]) {ActiveEnergy_week[i,4] <-"E"}
  if(ActiveEnergy>=ActiveEnergy_F[1] && ActiveEnergy<ActiveEnergy_F[2]) {ActiveEnergy_week[i,4] <-"F"}
  if(ActiveEnergy>=ActiveEnergy_G[1] && ActiveEnergy<ActiveEnergy_G[2]) {ActiveEnergy_week[i,4] <-"G"}
  if(ActiveEnergy>=ActiveEnergy_H[1] && ActiveEnergy<ActiveEnergy_H[2]) {ActiveEnergy_week[i,4] <-"H"}
  if(ActiveEnergy>=ActiveEnergy_I[1] && ActiveEnergy<ActiveEnergy_I[2]) {ActiveEnergy_week[i,4] <-"I"}
  if(ActiveEnergy>=ActiveEnergy_J[1] && ActiveEnergy<ActiveEnergy_J[2]) {ActiveEnergy_week[i,4] <-"J"}
  if(ActiveEnergy>=ActiveEnergy_K[1] && ActiveEnergy<ActiveEnergy_K[2]) {ActiveEnergy_week[i,4] <-"K"}
  if(ActiveEnergy>=ActiveEnergy_L[1] ) {ActiveEnergy_day[i,4] <-"L"}
}
names(ActiveEnergy_week) <- c("week","year","ActiveEnergy(kcal)","ActiveEnergy_indication")
ActiveEnergy_indication <- factor(ActiveEnergy_week$ActiveEnergy_indication,ordered = TRUE)
barplot(prop.table(table(ActiveEnergy_indication)))





