#global:  functions and constants for XC 2015 race data files

#Define regular expressions
#
#regular expression for 1:1000
rexp1 <- "\\b[1-9][0-9]{0,3}"
#regular expression for alpha characters
rexp2 <- "[a-zA-Z]"
#regular expression for numeral characters
rexp3 <- "[0-9]"
#regular expression for a race time like 19:34
rexp4 <- "\\d\\d:\\d\\d"

#Functions
#trim leading and trailing white spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#parse a record into component fields
make_record <- function(j,list_1 = record_list) {
  index <- j
  record <- trim(list_1[[j]])
  if(grepl("SR",record)) {
    str7 <- strsplit(record,"SR")
    class_runner <- "SR"
  } else if(grepl("JR",record)) {
    str7 <- strsplit(record,"JR")
    class_runner <- "JR"
  } else if(grepl("SO",record)) {
    str7 <- strsplit(record,"SO")
    class_runner <- "SO"
  } else if(grepl("FR",record)) {
    str7 <- strsplit(record,"FR")
    class_runner <- "FR"
  } else {
    str7 <- strsplit(record,"NA")  
    class_runner <- "NA"
  }
  #split on the runner name
  name_runner1 <- strsplit(str7[[1]][1],rexp1)
  name_runner <- trim(name_runner1[[1]][2])
  #split on the school name
  school_runner1 <- strsplit(str7[[1]][2],rexp3)
  school_runner <- trim(school_runner1[[1]][1])
  #split on the times
  times1 <- strsplit(str7[[1]][2],rexp2)
  times2 <- times1[[1]][length(times1[[1]])]
  times3 <-strsplit(trim(times2[[1]])," ")
  race_time <- times3[[1]][1]
  pace_time <- times3[[1]][2]
  record_out <- c(index,name_runner,class_runner,school_runner,race_time,pace_time)
}

#create a data frame given a list of records (one record for each runner)
make_df <- function(record_set) {
  #initialize data frame
  df_out <- as.data.frame(t(make_record(j=1,list_1=record_set)), stringsAsFactors = FALSE)
  #build the data frame
  for(k in 2:length(record_set)){
    row_new <- t(make_record(j=k,list_1=record_set))
    df_out <- rbind.data.frame(df_out,row_new)
  }
  #name the dataframe
  names(df_out) <- c("place","name","class","school","race_time","pace_time")
  return(df_out)
}


#Verona race has Division records for Varsity but not for JV
make_recordV <- function(j,list_1 = record_list) {
  index <- j
  record <- trim(list_1[[j]])
  if(grepl("SR",record)) {
    str7 <- strsplit(record,"SR")
    class_runner <- "SR"
  } else if(grepl("JR",record)) {
    str7 <- strsplit(record,"JR")
    class_runner <- "JR"
  } else if(grepl("SO",record)) {
    str7 <- strsplit(record,"SO")
    class_runner <- "SO"
  } else if(grepl("FR",record)) {
    str7 <- strsplit(record,"FR")
    class_runner <- "FR"
  } else {
    str7 <- strsplit(record,"NA")  
    class_runner <- "NA"
  }
  #split on the runner name
  name_runner1 <- strsplit(str7[[1]][1],rexp1)
  name_runner <- trim(name_runner1[[1]][2])
  #split on the division
  division1 <- strsplit(str7[[1]][2],rexp2)
  division <- trim(division1[[1]][1])
  
  
  #split on the school name
  school_runner1 <- strsplit(str7[[1]][2],rexp3)
  school_runner <- trim(school_runner1[[1]][2])
  #split on the times
  times1 <- strsplit(str7[[1]][2],rexp2)
  times2 <- times1[[1]][length(times1[[1]])]
  times3 <-strsplit(trim(times2[[1]])," ")
  race_time <- times3[[1]][1]
  pace_time <- times3[[1]][2]
  record_out <- c(index,name_runner,class_runner,division,school_runner,race_time,pace_time)
}
###################3
#make record for Marquette Invitational:  has names as Last Name, First Name; appends team position as last entry
make_recordMI <- function(j,list_1 = record_list) {    
    index <- j
    record <- trim(list_1[[j]])
  if(grepl("SR",record)) {
    str7 <- strsplit(record,"SR")
    class_runner <- "SR"
  } else if(grepl("JR",record)) {
      str7 <- strsplit(record,"JR")
      class_runner <- "JR"
  } else if(grepl("SO",record)) {
      str7 <- strsplit(record,"SO")
      class_runner <- "SO"
  } else if(grepl("FR",record)) {
      str7 <- strsplit(record,"FR")
      class_runner <- "FR"
  } else {
     str7 <- strsplit(record,"NA")  
    class_runner <- "NA"
  }
  #split on the runner namem reverse Last Name and First Name
  name_runner1 <- strsplit(str7[[1]][1],rexp1)
  name_runner <- trim(name_runner1[[1]][2])
  fix_name <- strsplit(name_runner," ")
  name_runner <- paste0(fix_name[[1]][3]," ",fix_name[[1]][1])
  #split on the school name
  school_runner1 <- strsplit(str7[[1]][2],rexp3)
  school_runner <- trim(school_runner1[[1]][1])
  #split on the times
  times1 <- strsplit(str7[[1]][2],rexp2)
  times2 <- times1[[1]][length(times1[[1]])]
  times3 <-strsplit(trim(times2[[1]])," ")
  #pace time precedes race time
  race_time <- times3[[1]][2]
  pace_time <- times3[[1]][1]
  record_out <- c(index,name_runner,class_runner,school_runner,race_time,pace_time)
}

#Janesville races have splits for V and FR_SO races that must be removed
make_recordJNSVI <- function(j,list_1 = record_list) {
  index <- j
  record <- trim(list_1[[j]])
  if(grepl("SR",record)) {
    str7 <- strsplit(record,"SR")
    class_runner <- "SR"
  } else if(grepl("JR",record)) {
    str7 <- strsplit(record,"JR")
    class_runner <- "JR"
  } else if(grepl("SO",record)) {
    str7 <- strsplit(record,"SO")
    class_runner <- "SO"
  } else if(grepl("FR",record)) {
    str7 <- strsplit(record,"FR")
    class_runner <- "FR"
  } else {
    str7 <- strsplit(record,"NA")  
    class_runner <- "NA"
  }
  #split on the runner name
  name_runner1 <- strsplit(str7[[1]][1],rexp1)
  str_length <- length(name_runner1[[1]])
  name_runner <- trim(name_runner1[[1]][str_length])
  #split on the school name
  school_runner1 <- strsplit(str7[[1]][2],rexp3)
  school_runner <- trim(school_runner1[[1]][1])
  #split on the times
  times1 <- strsplit(str7[[1]][2],rexp2)
  times2 <- times1[[1]][length(times1[[1]])]
  times3 <-strsplit(trim(times2[[1]])," ")
  #could generalize this to look for the k-1th and kth entries?  not always the case for each race?
  str_length1 <- length(times3[[1]])
  race_time <- times3[[1]][str_length1-1]
  pace_time <- times3[[1]][str_length1]
  record_out <- c(index,name_runner,class_runner,school_runner,race_time,pace_time)
}

#########Big 8 Class race has the columns in yet another order

make_recordB8 <- function(j,list_1 = record_list) {
  index <- j
  record <- trim(list_1[[j]])
  if(grepl("SR",record)) {
    str7 <- strsplit(record,"SR")
    class_runner <- "SR"
  } else if(grepl("JR",record)) {
    str7 <- strsplit(record,"JR")
    class_runner <- "JR"
  } else if(grepl("SO",record)) {
    str7 <- strsplit(record,"SO")
    class_runner <- "SO"
  } else if(grepl("FR",record)) {
    str7 <- strsplit(record,"FR")
    class_runner <- "FR"
  } else {
    str7 <- strsplit(record,"NA")  
    class_runner <- "NA"
  }
  #remove ellipsis if present and account for school name after race time
  str7[[1]][2] <- gsub(".","",str7[[1]][2],fixed=TRUE)
  #split on the runner name
  name_runner1 <- strsplit(str7[[1]][2],rexp4)
  name_runner <- trim(name_runner1[[1]][1])
  #split on the school name
  school_runner1 <- strsplit(str7[[1]][2],rexp4)
  school_runner <- trim(school_runner1[[1]][2])
  #   #split on the times
  #   times1 <- strsplit(trim(str7[[1]][2]),rexp2)
  #   times2 <- times1[[1]][length(times1[[1]])]
  race_time1 <-trim(gsub(rexp2,"",str7[[1]][2]))
  #account for hyphenated names that append hyphen to race time
  race_time <-trim(sub("-","",race_time1, fixed=TRUE))
  pace_time <- NA
  record_out <- c(index,name_runner,class_runner,school_runner,race_time,pace_time)
}

#Sectionals make record function
make_recordSec <- function(j,list_1 = record_list) {
  index <- j
  record <- trim(list_1[[j]])
#class label for sectionals is upper-lower case, change to all upper for consistency
  if(grepl("SR",record)) {
    str7 <- strsplit(record,"SR")
    class_runner <- "SR"
  } else if(grepl("JR",record)) {
    str7 <- strsplit(record,"JR")
    class_runner <- "JR"
  } else if(grepl("SO",record)) {
    str7 <- strsplit(record,"SO")
    class_runner <- "SO"
  } else if(grepl("FR",record)) {
    str7 <- strsplit(record,"FR")
    class_runner <- "FR"
  } else {
    str7 <- strsplit(record,"NA")  
    class_runner <- "NA"
  }
  #split on the runner name
  name_runner1 <- strsplit(str7[[1]][1],rexp1)
  # need 4th element of the string to account for the bib number and duplicate place number 
  name_runner <- trim(name_runner1[[1]][4])
  #split on the school name
  school_runner1 <- strsplit(str7[[1]][2],rexp3)
  school_runner <- trim(school_runner1[[1]][1])
  #split on the times
  times1 <- strsplit(str7[[1]][2],rexp2)
  times2 <- times1[[1]][length(times1[[1]])]
  times3 <-strsplit(trim(times2[[1]])," ")
    race_time <- times3[[1]][1]
    pace_time <- times3[[1]][2]
  record_out <- c(index,name_runner,class_runner,school_runner,race_time,pace_time)
}

#State record has the school name in all Caps, which confuses the string split operation using SO (e.g.
# MADISON has the SO string) so insert the space after two letter class code 
make_recordState <- function(j,list_1 = record_list) {
  index <- j
  record <- trim(list_1[[j]])
  if(grepl("Sr ",record)) {
    str7 <- strsplit(record,"Sr ")
    class_runner <- "SR"
  } else if(grepl("Jr ",record)) {
    str7 <- strsplit(record,"Jr ")
    class_runner <- "JR"
  } else if(grepl("So ",record)) {
    str7 <- strsplit(record,"So ")
    class_runner <- "SO"
  } else if(grepl("Fr ",record)) {
    str7 <- strsplit(record,"Fr ")
    class_runner <- "FR"
  } else {
    str7 <- strsplit(record,"NA")  
    class_runner <- "NA"
  }
  #split on the runner name
  name_runner1 <- strsplit(str7[[1]][1],rexp1)
  name_runner <- trim(name_runner1[[1]][2])
  #split on the school name
  school_runner1 <- strsplit(str7[[1]][2],rexp3)
  school_runner <- trim(school_runner1[[1]][1])
  #split on the times
  times1 <- strsplit(str7[[1]][2],rexp2)
  times2 <- times1[[1]][length(times1[[1]])]
  times3 <-strsplit(trim(times2[[1]])," ")
  race_time <- times3[[1]][1]
  pace_time <- times3[[1]][2]
  record_out <- c(index,name_runner,class_runner,school_runner,race_time,pace_time)
}



#create a data frame given a list of records (one record for each runner) including division, Verona
make_dfV <- function(record_set) {
  #initialize data frame
  df_out <- as.data.frame(t(make_recordV(j=1,list_1=record_set)), stringsAsFactors = FALSE)
  #build the data frame
  for(k in 2:length(record_set)){
    row_new <- t(make_recordV(j=k,list_1=record_set))
    df_out <- rbind.data.frame(df_out,row_new)
  }
  #name the dataframe
  names(df_out) <- c("place","name","class","division","school","race_time","pace_time")
  return(df_out)
}


#create a data frame given a list of records, Marquette Invitational
make_dfMI <- function(record_set) {
  #initialize data frame
  df_out <- as.data.frame(t(make_recordMI(j=1,list_1=record_set)), stringsAsFactors = FALSE)
  #build the data frame
  for(k in 2:length(record_set)){
    row_new <- t(make_recordMI(j=k,list_1=record_set))
    df_out <- rbind.data.frame(df_out,row_new)
  }
  #name the dataframe
  names(df_out) <- c("place","name","class","school","race_time","pace_time")
  return(df_out)
}

#create a df for Janesville Invitational
make_dfJNSVI <- function(record_set) {
  #initialize data frame
  df_out <- as.data.frame(t(make_recordJNSVI(j=1,list_1=record_set)), stringsAsFactors = FALSE)
  #build the data frame
  for(k in 2:length(record_set)){
    row_new <- t(make_recordJNSVI(j=k,list_1=record_set))
    df_out <- rbind.data.frame(df_out,row_new)
  }
  #name the dataframe
  names(df_out) <- c("place","name","class","school","race_time","pace_time")
  return(df_out)
}


########Big 8 df maker
make_dfB8 <- function(record_set) {
  #initialize data frame
  df_out <- as.data.frame(t(make_recordB8(j=1,list_1=record_set)), stringsAsFactors = FALSE)
  #build the data frame
  for(k in 2:length(record_set)){
    row_new <- t(make_recordB8(j=k,list_1=record_set))
    df_out <- rbind.data.frame(df_out,row_new)
  }
  #name the dataframe
  names(df_out) <- c("place","name","class","school","race_time","pace_time")
  return(df_out)
}


#Sectional data frame 
make_dfSec <- function(record_set) {
  #initialize data frame
  df_out <- as.data.frame(t(make_recordSec(j=1,list_1=record_set)), stringsAsFactors = FALSE)
  #build the data frame
  for(k in 2:length(record_set)){
    row_new <- t(make_recordSec(j=k,list_1=record_set))
    df_out <- rbind.data.frame(df_out,row_new)
  }
  #name the dataframe
  names(df_out) <- c("place","name","class","school","race_time","pace_time")
  return(df_out)
}


#State Data Frame
make_dfState <- function(record_set) {
  #initialize data frame
  df_out <- as.data.frame(t(make_recordState(j=1,list_1=record_set)), stringsAsFactors = FALSE)
  #build the data frame
  for(k in 2:length(record_set)){
    row_new <- t(make_recordState(j=k,list_1=record_set))
    df_out <- rbind.data.frame(df_out,row_new)
  }
  #name the dataframe
  names(df_out) <- c("place","name","class","school","race_time","pace_time")
  return(df_out)
}

#make final df with date and name of the race and numerical conversion of race and pace times.
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
  
  date_race <- as.Date(date_race)
  df_out <- cbind(race_name,date_race,df_out)
  df_out <- arrange(df_out,RaceTime_Min)
  
}

###########
#Final funtions for prepping master file
#insert the squad
insert_squad <- function(df,squad_name) {
  squad_name1 <- rep(c(squad_name),times=nrow(df))
  df <- cbind.data.frame(df[,1],squad_name1,df[,2:10])
  names(df)[2] <- "squad_name"
  return(df)
}

name_race_fix <- function(df){
  names(df)[1] <- "race_name"
  df
}

########convert digits to letters for classes and extract tabs (used for Lake Farm races)
make_list_ready <- function(x) {
  x <- gsub("\t"," ",x)
  x <- gsub(" 12 "," SR ", x)
  x <- gsub(" 11 "," JR ", x)
  x <- gsub(" 10 "," SO ", x)
  x <- gsub(" 9 "," FR ", x)
  record_list <- as.list(x)
}

########convert decimal minute time to minutes and seconds
convert_pace <- function(x) {
  p2 <- as.character(floor(x))
  p11 <- round(((x - floor(x))*60),digits=0)
  if(p11 < 10) {
    p1 <- as.character(p11)
    time_x <- paste0(p2,":0",p1)
  } else  {
    p1 <- as.character(p11)
    time_x <- paste0(p2,":",p1)
  }
  return(time_x)
}
