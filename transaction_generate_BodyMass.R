# build up transaction for Association Data Mining
# aggrication by day , variable = HKQuantityTypeIdentifierBodyMass
BodyMass_day <- aggregate(as.numeric(df_BodyMass$value) ,
                                   by = list(date = as.Date(df_BodyMass$creationDate)), FUN=mean)
names(BodyMass_day) <- c("date","BodyMass")
weekNo <- as.integer(format(as.Date(BodyMass_day$date),"%U"))+1
year <- format(as.Date(BodyMass_day$date),"%Y")
BodyMass_day <- cbind(BodyMass_day,weekNo)
BodyMass_day <- cbind(BodyMass_day,year)
names(BodyMass_day) <- c("date","BodyMass","week number","year")
max(df_BodyMass$value)
min(df_BodyMass$value)
# set up intervals of BodyMass
BodyMass_A <- c(68, 68+0.45)
BodyMass_B <- c(68+0.45, 68+0.45*2)
BodyMass_C <- c(68+0.45*2, 68+0.45*3)
BodyMass_D <- c(68+0.45*3, 68+0.45*4)
BodyMass_E <- c(68+0.45*4, 68+0.45*5)
BodyMass_F <- c(68+0.45*5, 68+0.45*6)
BodyMass_G <- c(68+0.45*6, 68+0.45*7)
BodyMass_H <- c(68+0.45*7, 68+0.45*8)
BodyMass_I <- c(68+0.45*8, 68+0.45*9)
BodyMass_J <- c(68+0.45*9, 68+0.45*10)
BodyMass_K <- c(68+0.45*10, 68+0.45*11)
BodyMass_L <- c(68+0.45*11, 68+0.45*12)

for(i in 1:nrow(BodyMass_day)) {
  BodyMass = BodyMass_day[i,2]
  if(BodyMass>=BodyMass_A[1] && BodyMass<BodyMass_A[2]) {BodyMass_day[i,5] <-"A"}
  if(BodyMass>=BodyMass_B[1] && BodyMass<BodyMass_B[2]) {BodyMass_day[i,5] <-"B"}
  if(BodyMass>=BodyMass_C[1] && BodyMass<BodyMass_C[2]) {BodyMass_day[i,5] <-"C"}
  if(BodyMass>=BodyMass_D[1] && BodyMass<BodyMass_D[2]) {BodyMass_day[i,5] <-"D"}
  if(BodyMass>=BodyMass_E[1] && BodyMass<BodyMass_E[2]) {BodyMass_day[i,5] <-"E"}
  if(BodyMass>=BodyMass_F[1] && BodyMass<BodyMass_F[2]) {BodyMass_day[i,5] <-"F"}
  if(BodyMass>=BodyMass_G[1] && BodyMass<BodyMass_G[2]) {BodyMass_day[i,5] <-"G"}
  if(BodyMass>=BodyMass_H[1] && BodyMass<BodyMass_H[2]) {BodyMass_day[i,5] <-"H"}
  if(BodyMass>=BodyMass_I[1] && BodyMass<BodyMass_I[2]) {BodyMass_day[i,5] <-"I"}
  if(BodyMass>=BodyMass_J[1] && BodyMass<BodyMass_J[2]) {BodyMass_day[i,5] <-"J"}
  if(BodyMass>=BodyMass_K[1] && BodyMass<BodyMass_K[2]) {BodyMass_day[i,5] <-"K"}
  if(BodyMass>=BodyMass_L[1] ) {BodyMass_day[i,5] <-"L"}
}
names(BodyMass_day) <- c("date","BodyMass","week number","year","BodyMass_indication")
BodyMass_indication <- factor(BodyMass_day$BodyMass_indication,ordered = TRUE)
barplot(prop.table(table(BodyMass_indication)))

BodyMass_week <- aggregate(as.numeric(BodyMass_day$BodyMass) ,
                               by = list(week =BodyMass_day$week,year=BodyMass_day$year ), FUN=mean)
for(i in 1:nrow(BodyMass_week)) {
  BodyMass = BodyMass_week[i,3]
  if(BodyMass>=BodyMass_A[1] && BodyMass<BodyMass_A[2]) {BodyMass_week[i,4] <-"A"}
  if(BodyMass>=BodyMass_B[1] && BodyMass<BodyMass_B[2]) {BodyMass_week[i,4] <-"B"}
  if(BodyMass>=BodyMass_C[1] && BodyMass<BodyMass_C[2]) {BodyMass_week[i,4] <-"C"}
  if(BodyMass>=BodyMass_D[1] && BodyMass<BodyMass_D[2]) {BodyMass_week[i,4] <-"D"}
  if(BodyMass>=BodyMass_E[1] && BodyMass<BodyMass_E[2]) {BodyMass_week[i,4] <-"E"}
  if(BodyMass>=BodyMass_F[1] && BodyMass<BodyMass_F[2]) {BodyMass_week[i,4] <-"F"}
  if(BodyMass>=BodyMass_G[1] && BodyMass<BodyMass_G[2]) {BodyMass_week[i,4] <-"G"}
  if(BodyMass>=BodyMass_H[1] && BodyMass<BodyMass_H[2]) {BodyMass_week[i,4] <-"H"}
  if(BodyMass>=BodyMass_I[1] && BodyMass<BodyMass_I[2]) {BodyMass_week[i,4] <-"I"}
  if(BodyMass>=BodyMass_J[1] && BodyMass<BodyMass_J[2]) {BodyMass_week[i,4] <-"J"}
  if(BodyMass>=BodyMass_K[1] && BodyMass<BodyMass_K[2]) {BodyMass_week[i,4] <-"K"}
  if(BodyMass>=BodyMass_L[1] ) {BodyMass_day[i,4] <-"L"}
}
names(BodyMass_week) <- c("week","year","BodyMass","BodyMass_indication")
BodyMass_indication <- factor(BodyMass_week$BodyMass_indication,ordered = TRUE)
barplot(prop.table(table(BodyMass_indication)))


