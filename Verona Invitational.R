#Create the Verona record set 6 Sept 2015 updated 4 Sept 2016

source("global.R")
library(lubridate)
library(dplyr)

dfVV <- readLines(con=file("Varsity Verona Individual 2106.txt",open="r"))
dfV_JV <- readLines(con=file("JV Verona Individual 2016.txt",open="r"))

#Create the Varsity data frame with all the records
df_Verona_Varsity <- make_dfV(record_set=as.list(dfVV))
Verona_Varsity <- make_df_final(df_Verona_Varsity,"2016-09-02","Verona Invitational_Varsity",3,5,6,7,TRUE)

#Create the JV data frame with all the records
Verona_JV1 <- make_df(record_set=as.list(dfV_JV))
Verona_JV <- make_df_final(Verona_JV1,"2016-09-03","Verona Invitational_JV",3,4,5,6,TRUE)

write.csv(Verona_Varsity, "Verona_Invitational_Varsity_2016.csv", row.names=FALSE)
write.csv(Verona_JV, "Verona_Invitational_JV_2016.csv", row.names=FALSE)

#note that when Excel opens the file, the race times are rounded to the nearest tenth second because
#Excel interprets a string like 19:11.79 as 12:19:12 AM (as a clock time rather than a duration in minutes and seconds)
#However the actual time is recoverable by looking at the decimal part of the race time and converting 
#back to seconds.and then specify the race_time and pace_time as text columns.  then the character strings 
#appear correctly but of course no calculations are possible

Verona_Varsity_noDiv <- Verona_Varsity[,c(1:5,7:11)]
save(Verona_Varsity_noDiv,Verona_JV,file="Verona.RData")
