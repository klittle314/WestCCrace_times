#West Invite uses two digit for class, not abbreviations.  
#pre process the files to make the substituion then apply the simple function
source("global.R")
library(lubridate)
library(dplyr)


dfWI_V <- readLines(con=file("V West Invite 10 Sept.txt",open="r"))
#tab delimited
#dfWI_V <- read.table(file="V West Invite 10 Sept.txt",sep="\t",colClasses="character")
dfWI_V <- gsub("\t"," ",dfWI_V)
dfWI_V <- gsub(" 12 "," SR ", dfWI_V)
dfWI_V <- gsub(" 11 "," JR ", dfWI_V)
dfWI_V <- gsub(" 10 "," SO ", dfWI_V)
dfWI_V <- gsub(" 9 "," FR ", dfWI_V)

WestInvite_V1 <- make_df(record_set=as.list(dfWI_V))
WestInvite_V <- make_df_final(WestInvite_V1,"2015-09-10","West Invitational_V",3,4,5,6,FALSE)

dfWI_JV <- readLines(con=file("JV West Invite 10 Sept.txt",open="r"))
#tab delimited
#dfWI_JV <- read.table(file="V West Invite 10 Sept.txt",sep="\t",colClasses="character")
dfWI_JV <- gsub("\t"," ",dfWI_JV)
dfWI_JV <- gsub(" 12 "," SR ", dfWI_JV)
dfWI_JV <- gsub(" 11 "," JR ", dfWI_JV)
dfWI_JV <- gsub(" 10 "," SO ", dfWI_JV)
dfWI_JV <- gsub(" 9 "," FR ", dfWI_JV)


WestInvite_JV1 <- make_df(record_set=as.list(dfWI_JV))


WestInvite_JV <- make_df_final(WestInvite_JV1,"2015-09-10","West Invitational_JV",3,4,5,6,FALSE)
save(WestInvite_V,WestInvite_JV,file="WestInvite.RData")
