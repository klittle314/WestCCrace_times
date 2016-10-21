df1 <- read.csv("test_athleticnet.csv")
#convert times to decimal minutes
df1$time <- as.character(df1$time)
time_chunks <- strsplit(df1$time,split=c(":"),fixed=TRUE)

                       
char_get <- function(x,index){
  char_out <- x[index]
} 

seconds_decimal <- as.numeric(sapply(time_chunks,char_get,index=2))
time_mins <- as.numeric(sapply(time_chunks,char_get,index=1))
df1$time_decimal <- time_mins + seconds_decimal/60
