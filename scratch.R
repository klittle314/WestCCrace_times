#Define regular expressions
#
#regular expression for 1:1000
rexp1 <- "\\b[1-9][0-9]{0,3}"
#regular expression for alpha characters
rexp2 <- "[a-zA-Z]"
#regular expression for numeral characters
rexp3 <- "[0-9]"


dfVV <- readLines(con=file("Varsity Verona Individual.txt",open="r"))
dfV_JV <- readLines(con=file("JV Verona Individual.txt",open="r"))

#will return a warning about lacking EOL marker
testV <- as.list(dfVV)
j <- 1
record <- trim(testV[[j]])
if(grepl("SR",record)) {
  str7 <- strsplit(record,"SR")
  class_runner <- "SR"
} else if(grepl("JR",record)) {
  str7 <- strsplit(record,"JR")
  class_runner <- "JR"
} else if(grepl("SO",record)) {
  str7 <- strsplit(record,"SO")
  class_runner <- "SO"
} else {
  str7 <- strsplit(record,"FR")
  class_runner <- "FR"
}
#split on the runner name
name_runner1 <- strsplit(str7[[1]][1],rexp1)
name_runner <- trim(name_runner1[[1]][2])

#split on the division
division1 <- strsplit(str7[[1]][2],rexp2)
division <- trim(division1[[1]][1])

#
#split on the school name
school_runner1 <- strsplit(str7[[1]][2],rexp3)
school_runner <- trim(school_runner1[[1]][2])

#split on the times
times1 <- strsplit(str7[[1]][2],rexp2)
times2 <- times1[[1]][length(times1[[1]])]
times3 <-strsplit(trim(times2[[1]])," ")
race_time <- times3[[1]][1]
pace_time <- times3[[1]][2]




#Create the data frame with all the records
df_Verona_Varsity <- make_dfV(record_set=as.list(dfVV))


#make final df
# class_idx is the column number of the class of the runner
# school_idx is the column number of the school of the runner
# race_time_idx is the column number of the race time of the runner
# pace_time_idx is the column number of the pace time of the runner
# pace_error is a logical variable; if TRUE, pace_time calculated from race time, else taken from file.
make_df_final <- function(df,date_race,
                          race_name,
                          class_idx,
                          school_idx,
                          race_time_idx,
                          pace_time_idx,
                          pace_error=FALSE) {
  #create factors for class and school
  df_out <- df
  df_out[,c(class_idx,school_idx)] <- lapply(df_out[,c(class_idx,school_idx)],as.factor)
  
  #convert the character times to numeric values (decimal minutes)
  
  df_out$RaceTime_Min <- sapply(strsplit(df_out[,race_time_idx],":"),
                                     function(x) {
                                       x <- as.numeric(x)
                                       x[1]+x[2]/60
                                     }
                                )
  if(pace_error) {
    df_out$PaceTime_Min <- df_out$RaceTime_Min/3.10686
  } else {
    df_out$PaceTime_Min <- sapply(strsplit(df_out[,pace_time_idx],":"),
                                     function(x) {
                                       x <- as.numeric(x)
                                       x[1]+x[2]/60
                                     }
                                  )
  }
  
  date_use <- as.Date(date_race)
  df_out <- cbind(race_name,date_use,df_out)
  df_out <- arrange(df_out,RaceTime_Min)
  
}

Verona_varsity <- make_df_final(df_Verona_Varsity,"2015-09-05","Verona Invitational_Varsity",3,5,6,7,TRUE)

Verona_JV1 <- make_df(record_set=as.list(dfV_JV))
Verona_JV <- make_df_final(Verona_JV1,"2015-09-05","Verona Invitational_JV",3,4,5,6,TRUE)


record_set <- dfWI_JV
df_out <- as.data.frame(t(make_record(j=1,list_1=record_set)), stringsAsFactors = FALSE)

for(k in 2:length(record_set)){
  row_new <- t(make_record(j=k,list_1=record_set))
  print(row_new)
  df_out <- rbind.data.frame(df_out,row_new)
}
