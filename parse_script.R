#reading text files 
# KL 31 Aug 2015

source("global.R")
library(lubridate)
library(dplyr)


df1 <- readLines(con=file("FR-SOPH times.txt",open="r"))
df2 <- readLines(con=file("JR-SR times.txt",open="r"))

#will return a warning about lacking EOL marker

#Create the data frame with all the records
df_WestBend_JRSR <- make_df(record_set=as.list(df2))
df_WestBend_FRSO <- make_df(record_set=as.list(df1))
df_WestBend <- rbind(df_WestBend_JRSR,df_WestBend_FRSO)
date_WestBend <- as.Date("2015-08-29")
race_name <- "West Bend"
df_WestBend <- cbind(race_name,date_WestBend,df_WestBend)
#create factors for class and school
df_WestBend[,5:6] <- lapply(df_WestBend[,5:6],as.factor)

#convert the character times to numeric values (decimal minutes)

df_WestBend$RaceTime_Min <- sapply(strsplit(df_WestBend[,7],":"),
                          function(x) {
                            x <- as.numeric(x)
                            x[1]+x[2]/60
                          }
                      )

df_WestBend$PaceTime_Min <- sapply(strsplit(df_WestBend[,8],":"),
                                  function(x) {
                                    x <- as.numeric(x)
                                    x[1]+x[2]/60
                                  }
                            )

#sort the dataframe by winning times
df_WestBend <- arrange(df_WestBend,RaceTime_Min)
save(df_WestBend,file="WestBend.RData")
#export the data file
write.csv(df_WestBend,"WestBendTimes.csv",row.names=FALSE)
