# build up transaction for Association Data Mining
# aggrication by day , variable = HKCategoryTypeIdentifierAppleStandHour

# set up time of recorded value to minute
df_StandHour$unit <- "min"
# calculate how many miniutes of stand by compare the start time and end time of each observation
df_StandHour$value <- as.integer(difftime(df_StandHour$endDate,df_StandHour$startDate,units = "min")) 
# create dateFrame of Standhour of each day
StandHour_day <- aggregate(as.numeric(df_StandHour$value) , by = list(date = as.Date(df_StandHour$creationDate)), FUN=sum)
names(StandHour_day) <- c("date","StandHour")
weekNo <- as.integer(format(as.Date(StandHour_day$date),"%U"))+1
year <- format(as.Date(StandHour_day$date),"%Y")
StandHour_day <- cbind(StandHour_day,weekNo)
StandHour_day <- cbind(StandHour_day,year)
names(StandHour_day) <- c("date","StandHour","week","year")


# max and min variables is used to set up intervals
# max(StandHour_day$StandHour)
# min(StandHour_day$StandHour)
# set up intervals of StandHour
StandHour_A <- c(0, 90)
StandHour_B <- c(90*1, 90*2)
StandHour_C <- c(90*2, 90*3)
StandHour_D <- c(90*3, 90*4)
StandHour_E <- c(90*4, 90*5)
StandHour_F <- c(90*5, 90*6)
StandHour_G <- c(90*6, 90*7)
StandHour_H <- c(90*7, 90*8)
StandHour_I <- c(90*8, 90*9)
StandHour_J <- c(90*9, 90*10)
StandHour_K <- c(90*10, 90*11)
StandHour_L <- c(90*11, 90*1000)
# Assign indications to standhour of each day
for(i in 1:nrow(StandHour_day)) {
  StandHour = StandHour_day[i,2]
  if(StandHour>=StandHour_A[1] && StandHour<StandHour_A[2]) {StandHour_day[i,5] <-"A"}
  if(StandHour>=StandHour_B[1] && StandHour<StandHour_B[2]) {StandHour_day[i,5] <-"B"}
  if(StandHour>=StandHour_C[1] && StandHour<StandHour_C[2]) {StandHour_day[i,5] <-"C"}
  if(StandHour>=StandHour_D[1] && StandHour<StandHour_D[2]) {StandHour_day[i,5] <-"D"}
  if(StandHour>=StandHour_E[1] && StandHour<StandHour_E[2]) {StandHour_day[i,5] <-"E"}
  if(StandHour>=StandHour_F[1] && StandHour<StandHour_F[2]) {StandHour_day[i,5] <-"F"}
  if(StandHour>=StandHour_G[1] && StandHour<StandHour_G[2]) {StandHour_day[i,5] <-"G"}
  if(StandHour>=StandHour_H[1] && StandHour<StandHour_H[2]) {StandHour_day[i,5] <-"H"}
  if(StandHour>=StandHour_I[1] && StandHour<StandHour_I[2]) {StandHour_day[i,5] <-"I"}
  if(StandHour>=StandHour_J[1] && StandHour<StandHour_J[2]) {StandHour_day[i,5] <-"J"}
  if(StandHour>=StandHour_K[1] && StandHour<StandHour_K[2]) {StandHour_day[i,5] <-"K"}
  if(StandHour>=StandHour_L[1] ) {StandHour_day[i,5] <-"L"}
}
# Assign indications to standhour of each day
names(StandHour_day) <- c("date","StandHour","week","year","StandHour_indication")
StandHour_indication <- factor(StandHour_day$StandHour_indication,ordered = TRUE)
barplot(prop.table(table(StandHour_indication)))
# create dateFrame of Standhour of week
StandHour_week <- aggregate(as.numeric(StandHour_day$StandHour) ,
                           by = list(week =StandHour_day$week,year=StandHour_day$year ), FUN=mean)
for(i in 1:nrow(StandHour_week)) {
  StandHour = StandHour_week[i,3]
  if(StandHour>=StandHour_A[1] && StandHour<StandHour_A[2]) {StandHour_week[i,4] <-"A"}
  if(StandHour>=StandHour_B[1] && StandHour<StandHour_B[2]) {StandHour_week[i,4] <-"B"}
  if(StandHour>=StandHour_C[1] && StandHour<StandHour_C[2]) {StandHour_week[i,4] <-"C"}
  if(StandHour>=StandHour_D[1] && StandHour<StandHour_D[2]) {StandHour_week[i,4] <-"D"}
  if(StandHour>=StandHour_E[1] && StandHour<StandHour_E[2]) {StandHour_week[i,4] <-"E"}
  if(StandHour>=StandHour_F[1] && StandHour<StandHour_F[2]) {StandHour_week[i,4] <-"F"}
  if(StandHour>=StandHour_G[1] && StandHour<StandHour_G[2]) {StandHour_week[i,4] <-"G"}
  if(StandHour>=StandHour_H[1] && StandHour<StandHour_H[2]) {StandHour_week[i,4] <-"H"}
  if(StandHour>=StandHour_I[1] && StandHour<StandHour_I[2]) {StandHour_week[i,4] <-"I"}
  if(StandHour>=StandHour_J[1] && StandHour<StandHour_J[2]) {StandHour_week[i,4] <-"J"}
  if(StandHour>=StandHour_K[1] && StandHour<StandHour_K[2]) {StandHour_week[i,4] <-"K"}
  if(StandHour>=StandHour_L[1] ) {StandHour_day[i,4] <-"L"}
}
names(StandHour_week) <- c("week","year","mean","StandHour_indication")
StandHour_indication <- factor(StandHour_week$StandHour_indication,ordered = TRUE)
barplot(prop.table(table(StandHour_indication)))

