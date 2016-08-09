#reading text files 
# KL 31 Aug 2015

#trim leading and trailing white spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#####################

df1 <- readLines(con=file("FR-SOPH times.txt",open="r"))
df2 <- readLines(con=file("JR-SR times.txt",open="r"))

#will return a warning about lacking EOL marker

record_list <- as.list(df2)

#Define regular expressions
#
#regular expression for 1:1000
rexp1 <- "\\b[1-9][0-9]{0,3}"
#regular expression for alpha characters
rexp2 <- "[a-zA-Z]"
#regular expression for numeral characters
rexp3 <- "[0-9]"




#initialize dataframe
df_out <- as.data.frame(t(make_record(j=1,list_1=as.list(df2))), stringsAsFactors = FALSE)
#build the data frame
for(k in 2:length(record_list)){
  row_new <- t(make_record(j=k,list_1=as.list(df2)))
  df_out <- rbind.data.frame(df_out,row_new)
}

#name the dataframe
names(df_out) <- c("place","name","class","school","race_time","pace_time")



test999 <- make_df(record_set=as.list(df2))
