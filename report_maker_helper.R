#script to set up the code for the XC final files
library(rmarkdown)
library(lubridate)
library(plyr)
library(reshape2)
library(xlsx)
load("WestGirlsCumData.RData")
source("global.R")

df_all <- WestGirlsCC_CumResults_31Oct2015
#revise the names of the races
df_all$Race <- as.character(df_all$race_name)
r1 <- df_all$Race
rn1 <- as.character(df_all$race_name)
sq1 <- as.character(df_all$squad_name) 
for(i in 1:nrow(df_all)){
  if(identical(rn1[i],"MadCityChamp") && identical(sq1[i], "Varsity")){
      r1[i] <- "MadCityChamionship_Varsity"
  } else if(identical(rn1[i],"MadCityChamp") && identical(sq1[i], "JVarsity")) {
      r1[i] <- "MadCityChamionship_JV"
  } else if(identical(rn1[i],"West Bend") && identical(sq1[i], "FR-SO")) {
      r1[i] <- "West Bend_FR-SO"
  } else if(identical(rn1[i],"West Bend") && identical(sq1[i], "JR-SR")) {
      r1[i] <- "West Bend_JR-SR"
  } else if(identical(rn1[i],"Marquette Invitational_V")) {
      r1[i] <- "Marquette Invitational_Varsity"
  } else if(identical(rn1[i],"West Invitational_V")) {
      r1[i] <- "West Invitational_Varsity"
  }
  
}

df_all$Race <- as.factor(r1)

df_all$correct_pace <- sapply(df_all$PaceTime_Min,convert_pace)

names(df_all)[names(df_all)=="date_race"] <- "Date"
names(df_all)[names(df_all)=="place"] <- "Place"
names(df_all)[names(df_all)=="race_time"] <- "Time"
names(df_all)[names(df_all)=="correct_pace"] <- "Pace"


race_names <- levels(df_all$Race)
#min function with na.rm=TRUE
min_fun <- function(x){
  x <- min(x,na.rm=TRUE)
}
#calculate the winning times for the races and insert into the data frame of individual times
win_time<- tapply(df_all[,"RaceTime_Min"],df_all$Race,min_fun)

race_win_time <- cbind.data.frame(levels(df_all$Race),win_time)

race_win_time$min_sec <- sapply(race_win_time$win_time,convert_pace)

names(race_win_time)[1] <- "Race"

WestCum_2015Season <- merge(df_all,race_win_time)
names(WestCum_2015Season)[15] <- "Winning Time"
WestCum_2015Season <- arrange(WestCum_2015Season,Date)

WestOnlyFull <- droplevels(WestCum_2015Season[WestCum_2015Season$school=="Madison West",])

#this join fills in the winning race times
# WestOnlyFull <- merge(WestOnly,race_win_time)
# 
# names(WestOnlyFull)[15] <- "Winning Time"
# 
# #now put the dataframe back in time order, after the merge on race names used race alpha order
# WestOnlyFull <- arrange(WestOnlyFull,Date)
save(WestCum_2015Season,file="SeasonCumData.RData")
save(WestOnlyFull,file="BanquetData.RData")

dir1 <- getwd()
pathout <- paste0(dir1,"/summary",format(Sys.time(),"%Y-%m-%d_%H_%M"))
dir.create(pathout)
file.copy("report2.Rmd",paste0(pathout,"/report2.Rmd"),overwrite=TRUE)
file.copy("BanquetData.RData",paste0(pathout,"/BanquetData.RData"),overwrite=TRUE)
setwd(pathout)

getwd()
runner_names <- levels(as.factor(WestOnlyFull$name))
idx <- c(76:100)
#idx <- runner_names
for(id in idx){
  runner <- runner_names[id]
  sub1_df <- droplevels(WestOnlyFull[WestOnlyFull$name == runner,])
  sub11_df <- sub1_df[,c("Date","RaceTime_Min","win_time")]
  sub12_df <- melt(sub11_df,id="Date")
  sub2_df <- sub1_df[,c(1,4,5,15,9,13)]
  render("report2.Rmd",output_file=paste0(runner,'.docx'))
}
setwd(dir1)

runner_names
