# build up transaction for Association Data Mining
# aggrication by day , variable = HKCategoryTypeIdentifierSleepAnalysis

# set up time of recorded value to minute
df_SleepAnalysis$unit <- "min"
# calculate how many miniutes of spleep by compare the start time and end time of each observation
df_SleepAnalysis$value <- as.integer(difftime(df_SleepAnalysis$endDate,df_SleepAnalysis$startDate,units = "min")) 


SleepAnalysis_day <- aggregate(as.numeric(df_SleepAnalysis$value) , by = list(date = as.Date(df_SleepAnalysis$creationDate)), FUN=sum)
names(SleepAnalysis_day) <- c("date","SleepAnalysis")
weekNo <- as.integer(format(as.Date(SleepAnalysis_day$date),"%U"))+1
year <- format(as.Date(SleepAnalysis_day$date),"%Y")
SleepAnalysis_day <- cbind(SleepAnalysis_day,weekNo)
SleepAnalysis_day <- cbind(SleepAnalysis_day,year)
names(SleepAnalysis_day) <- c("date","SleepAnalysis","week","year")
max(SleepAnalysis_day$SleepAnalysis)
min(SleepAnalysis_day$SleepAnalysis)
# set up intervals of SleepAnalysis
SleepAnalysis_A <- c(0, 60*1)
SleepAnalysis_B <- c(60*1, 60*2)
SleepAnalysis_C <- c(60*2, 60*3)
SleepAnalysis_D <- c(60*3, 60*4)
SleepAnalysis_E <- c(60*4, 60*5)
SleepAnalysis_F <- c(60*5, 60*6)
SleepAnalysis_G <- c(60*6, 60*7)
SleepAnalysis_H <- c(60*7, 60*8)
SleepAnalysis_I <- c(60*8, 60*9)
SleepAnalysis_J <- c(60*9, 60*10)
SleepAnalysis_K <- c(60*10, 60*11)
SleepAnalysis_L <- c(60*11, 60*1000)

for(i in 1:nrow(SleepAnalysis_day)) {
  SleepAnalysis = SleepAnalysis_day[i,2]
  if(SleepAnalysis>=SleepAnalysis_A[1] && SleepAnalysis<SleepAnalysis_A[2]) {SleepAnalysis_day[i,5] <-"A"}
  if(SleepAnalysis>=SleepAnalysis_B[1] && SleepAnalysis<SleepAnalysis_B[2]) {SleepAnalysis_day[i,5] <-"B"}
  if(SleepAnalysis>=SleepAnalysis_C[1] && SleepAnalysis<SleepAnalysis_C[2]) {SleepAnalysis_day[i,5] <-"C"}
  if(SleepAnalysis>=SleepAnalysis_D[1] && SleepAnalysis<SleepAnalysis_D[2]) {SleepAnalysis_day[i,5] <-"D"}
  if(SleepAnalysis>=SleepAnalysis_E[1] && SleepAnalysis<SleepAnalysis_E[2]) {SleepAnalysis_day[i,5] <-"E"}
  if(SleepAnalysis>=SleepAnalysis_F[1] && SleepAnalysis<SleepAnalysis_F[2]) {SleepAnalysis_day[i,5] <-"F"}
  if(SleepAnalysis>=SleepAnalysis_G[1] && SleepAnalysis<SleepAnalysis_G[2]) {SleepAnalysis_day[i,5] <-"G"}
  if(SleepAnalysis>=SleepAnalysis_H[1] && SleepAnalysis<SleepAnalysis_H[2]) {SleepAnalysis_day[i,5] <-"H"}
  if(SleepAnalysis>=SleepAnalysis_I[1] && SleepAnalysis<SleepAnalysis_I[2]) {SleepAnalysis_day[i,5] <-"I"}
  if(SleepAnalysis>=SleepAnalysis_J[1] && SleepAnalysis<SleepAnalysis_J[2]) {SleepAnalysis_day[i,5] <-"J"}
  if(SleepAnalysis>=SleepAnalysis_K[1] && SleepAnalysis<SleepAnalysis_K[2]) {SleepAnalysis_day[i,5] <-"k"}
  if(SleepAnalysis>=SleepAnalysis_L[1] ) {SleepAnalysis_day[i,5] <-"L"}
}
names(SleepAnalysis_day) <- c("date","SleepAnalysis","week","year","SleepAnalysis_indication")
SleepAnalysis_indication <- factor(SleepAnalysis_day$SleepAnalysis_indication,ordered = TRUE)
barplot(prop.table(table(SleepAnalysis_indication)))

SleepAnalysis_week <- aggregate(as.numeric(SleepAnalysis_day$SleepAnalysis) ,
                            by = list(week =SleepAnalysis_day$week,year=SleepAnalysis_day$year ), FUN=mean)
for(i in 1:nrow(SleepAnalysis_week)) {
  SleepAnalysis = SleepAnalysis_week[i,3]
  if(SleepAnalysis>=SleepAnalysis_A[1] && SleepAnalysis<SleepAnalysis_A[2]) {SleepAnalysis_week[i,4] <-"A"}
  if(SleepAnalysis>=SleepAnalysis_B[1] && SleepAnalysis<SleepAnalysis_B[2]) {SleepAnalysis_week[i,4] <-"B"}
  if(SleepAnalysis>=SleepAnalysis_C[1] && SleepAnalysis<SleepAnalysis_C[2]) {SleepAnalysis_week[i,4] <-"C"}
  if(SleepAnalysis>=SleepAnalysis_D[1] && SleepAnalysis<SleepAnalysis_D[2]) {SleepAnalysis_week[i,4] <-"D"}
  if(SleepAnalysis>=SleepAnalysis_E[1] && SleepAnalysis<SleepAnalysis_E[2]) {SleepAnalysis_week[i,4] <-"E"}
  if(SleepAnalysis>=SleepAnalysis_F[1] && SleepAnalysis<SleepAnalysis_F[2]) {SleepAnalysis_week[i,4] <-"F"}
  if(SleepAnalysis>=SleepAnalysis_G[1] && SleepAnalysis<SleepAnalysis_G[2]) {SleepAnalysis_week[i,4] <-"G"}
  if(SleepAnalysis>=SleepAnalysis_H[1] && SleepAnalysis<SleepAnalysis_H[2]) {SleepAnalysis_week[i,4] <-"H"}
  if(SleepAnalysis>=SleepAnalysis_I[1] && SleepAnalysis<SleepAnalysis_I[2]) {SleepAnalysis_week[i,4] <-"I"}
  if(SleepAnalysis>=SleepAnalysis_J[1] && SleepAnalysis<SleepAnalysis_J[2]) {SleepAnalysis_week[i,4] <-"J"}
  if(SleepAnalysis>=SleepAnalysis_K[1] && SleepAnalysis<SleepAnalysis_K[2]) {SleepAnalysis_week[i,4] <-"K"}
  if(SleepAnalysis>=SleepAnalysis_L[1] ) {SleepAnalysis_day[i,4] <-"SleepAnalysis_L"}
}
names(SleepAnalysis_week) <- c("week","year","mean","SleepAnalysis_indication")
SleepAnalysis_indication <- factor(SleepAnalysis_week$SleepAnalysis_indication,ordered = TRUE)
barplot(prop.table(table(SleepAnalysis_indication)))


