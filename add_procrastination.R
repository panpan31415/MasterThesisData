# Generate procrastination data
procrastination_week <- data.frame(date="",year="",week="",Procra_value="",stringsAsFactors = FALSE)
tmp_date <- as.Date("2015-07-30")
End_date <- as.Date("2017-07-27")
i <- 1
while(tmp_date < End_date)
{
  procrastination_week[i,1] <- format(tmp_date,format = "%Y-%m-%d +0200")
  procrastination_week[i,2] <- format(tmp_date,"%Y")
  procrastination_week[i,3] <- as.integer(format(tmp_date,"%U"))+1
  procrastination_week[i,4] <- sample(1:10, 1)
  tmp_date <- tmp_date + 7
  i <- i+1
}
rm(End_date,i,tmp_date)

