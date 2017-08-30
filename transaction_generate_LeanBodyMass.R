# build up transaction for Association Data Mining
# aggrication by day , variable = HKQuantityTypeIdentifierLeanBodyMass

LeanBodyMass_day <- aggregate(as.numeric(df_LeanBodyMass$value) , by = list(date = as.Date(df_LeanBodyMass$creationDate)), FUN=mean)
names(LeanBodyMass_day) <- c("date","LeanBodyMass")
weekNo <- as.integer(format(as.Date(LeanBodyMass_day$date),"%U"))+1
year <- format(as.Date(LeanBodyMass_day$date),"%Y")
LeanBodyMass_day <- cbind(LeanBodyMass_day,weekNo)
LeanBodyMass_day <- cbind(LeanBodyMass_day,year)
names(LeanBodyMass_day) <- c("date","LeanBodyMass","week","year")
max(LeanBodyMass_day$LeanBodyMass)
min(LeanBodyMass_day$LeanBodyMass)
# set up intervals of LeanBodyMass
LeanBodyMass_A <- c(57, 57+0.4*1)
LeanBodyMass_B <- c(57+0.4*1, 57+0.4*2)
LeanBodyMass_C <- c(57+0.4*2, 57+0.4*3)
LeanBodyMass_D <- c(57+0.4*3, 57+0.4*4)
LeanBodyMass_E <- c(57+0.4*4, 57+0.4*5)
LeanBodyMass_F <- c(57+0.4*5, 57+0.4*6)
LeanBodyMass_G <- c(57+0.4*6, 57+0.4*7)
LeanBodyMass_H <- c(57+0.4*7, 57+0.4*8)
LeanBodyMass_I <- c(57+0.4*8, 57+0.4*9)
LeanBodyMass_J <- c(57+0.4*9, 57+0.4*10)
LeanBodyMass_K <- c(57+0.4*10, 57+0.4*11)
LeanBodyMass_L <- c(57+0.4*11, 57+0.4*1000)

for(i in 1:nrow(LeanBodyMass_day)) {
  LeanBodyMass = LeanBodyMass_day[i,2]
  if(LeanBodyMass>=LeanBodyMass_A[1] && LeanBodyMass<LeanBodyMass_A[2]) {LeanBodyMass_day[i,5] <-"A"}
  if(LeanBodyMass>=LeanBodyMass_B[1] && LeanBodyMass<LeanBodyMass_B[2]) {LeanBodyMass_day[i,5] <-"B"}
  if(LeanBodyMass>=LeanBodyMass_C[1] && LeanBodyMass<LeanBodyMass_C[2]) {LeanBodyMass_day[i,5] <-"C"}
  if(LeanBodyMass>=LeanBodyMass_D[1] && LeanBodyMass<LeanBodyMass_D[2]) {LeanBodyMass_day[i,5] <-"D"}
  if(LeanBodyMass>=LeanBodyMass_E[1] && LeanBodyMass<LeanBodyMass_E[2]) {LeanBodyMass_day[i,5] <-"E"}
  if(LeanBodyMass>=LeanBodyMass_F[1] && LeanBodyMass<LeanBodyMass_F[2]) {LeanBodyMass_day[i,5] <-"F"}
  if(LeanBodyMass>=LeanBodyMass_G[1] && LeanBodyMass<LeanBodyMass_G[2]) {LeanBodyMass_day[i,5] <-"G"}
  if(LeanBodyMass>=LeanBodyMass_H[1] && LeanBodyMass<LeanBodyMass_H[2]) {LeanBodyMass_day[i,5] <-"H"}
  if(LeanBodyMass>=LeanBodyMass_I[1] && LeanBodyMass<LeanBodyMass_I[2]) {LeanBodyMass_day[i,5] <-"I"}
  if(LeanBodyMass>=LeanBodyMass_J[1] && LeanBodyMass<LeanBodyMass_J[2]) {LeanBodyMass_day[i,5] <-"J"}
  if(LeanBodyMass>=LeanBodyMass_K[1] && LeanBodyMass<LeanBodyMass_K[2]) {LeanBodyMass_day[i,5] <-"K"}
  if(LeanBodyMass>=LeanBodyMass_L[1] ) {LeanBodyMass_day[i,5] <-"L"}
}
names(LeanBodyMass_day) <- c("date","LeanBodyMass","week","year","LeanBodyMass_indication")
LeanBodyMass_indication <- factor(LeanBodyMass_day$LeanBodyMass_indication,ordered = TRUE)
barplot(prop.table(table(LeanBodyMass_indication)))

LeanBodyMass_week <- aggregate(as.numeric(LeanBodyMass_day$LeanBodyMass) ,
                                by = list(week =LeanBodyMass_day$week,year=LeanBodyMass_day$year ), FUN=mean)
for(i in 1:nrow(LeanBodyMass_week)) {
  LeanBodyMass = LeanBodyMass_week[i,3]
  if(LeanBodyMass>=LeanBodyMass_A[1] && LeanBodyMass<LeanBodyMass_A[2]) {LeanBodyMass_week[i,4] <-"A"}
  if(LeanBodyMass>=LeanBodyMass_B[1] && LeanBodyMass<LeanBodyMass_B[2]) {LeanBodyMass_week[i,4] <-"B"}
  if(LeanBodyMass>=LeanBodyMass_C[1] && LeanBodyMass<LeanBodyMass_C[2]) {LeanBodyMass_week[i,4] <-"C"}
  if(LeanBodyMass>=LeanBodyMass_D[1] && LeanBodyMass<LeanBodyMass_D[2]) {LeanBodyMass_week[i,4] <-"D"}
  if(LeanBodyMass>=LeanBodyMass_E[1] && LeanBodyMass<LeanBodyMass_E[2]) {LeanBodyMass_week[i,4] <-"E"}
  if(LeanBodyMass>=LeanBodyMass_F[1] && LeanBodyMass<LeanBodyMass_F[2]) {LeanBodyMass_week[i,4] <-"F"}
  if(LeanBodyMass>=LeanBodyMass_G[1] && LeanBodyMass<LeanBodyMass_G[2]) {LeanBodyMass_week[i,4] <-"G"}
  if(LeanBodyMass>=LeanBodyMass_H[1] && LeanBodyMass<LeanBodyMass_H[2]) {LeanBodyMass_week[i,4] <-"H"}
  if(LeanBodyMass>=LeanBodyMass_I[1] && LeanBodyMass<LeanBodyMass_I[2]) {LeanBodyMass_week[i,4] <-"I"}
  if(LeanBodyMass>=LeanBodyMass_J[1] && LeanBodyMass<LeanBodyMass_J[2]) {LeanBodyMass_week[i,4] <-"J"}
  if(LeanBodyMass>=LeanBodyMass_K[1] && LeanBodyMass<LeanBodyMass_K[2]) {LeanBodyMass_week[i,4] <-"K"}
  if(LeanBodyMass>=LeanBodyMass_L[1] ) {LeanBodyMass_day[i,4] <-"L"}
}
names(LeanBodyMass_week) <- c("week","year","mean","LeanBodyMass_indication")
LeanBodyMass_indication <- factor(LeanBodyMass_week$LeanBodyMass_indication,ordered = TRUE)
barplot(prop.table(table(LeanBodyMass_indication)))





