source("global.R")
library(lubridate)
library(dplyr)


df1_state <- readLines(con=file("Girls CC final.txt",open="r"))
head(df1_state)

make_list_ready <- function(x) {
  x <- gsub("\t"," ",x)
  x <- gsub("12 - "," Sr ", x)
  x <- gsub("11 - "," Jr ", x)
  x <- gsub("10 - "," So ", x)
  x <- gsub("9 - "," Fr ", x)
  record_list <- as.list(x)
}

df2_state <- make_list_ready(df1_state)
state_df <- list(rep(NA,190))

for(i in 1:190) {
  state_df[[1]][i] <- paste(df2_state[[2*i-1]],df2_state[[2*i]])
}

fix_grades <- function(x) {
  x <- gsub(" 12 - "," Sr ", x)
  x <- gsub(" 11 - "," Jr ", x)
  x <- gsub(" 10 - "," So ", x)
  x <- gsub(" 9 - "," Fr ", x)
  return(x)
}

state_df1 <- fix_grades(state_df[[1]])
state_df2 <- as.list(state_df1)
write.csv(state_df1,"state_df1.csv",row.names=FALSE)

df_State <- make_dfState(record_set=state_df2)

df_State_Final <- make_df_final(df_State,"2015-10-31","State Div 1",3,4,5,6,TRUE)

State_Final <- insert_squad(df_State_Final,"State")
State_Final <- name_race_fix(State_Final)

save(State_Final,file="State.Rdata")
levels(State_Final$school)
#change West name to be upper and lower case to match the other names
State_Final$school <- gsub("MADISON WEST", "Madison West", State_Final$school)
save(State_Final,file="State.Rdata")

