#create master file

#load individual race data files
# list_data <- list(c("WestInvite.RData"),c("WestBend.RData"),c("Verona.RData"))
# load_local <- function(x){
#   load(x)
# }
# try1 <- lapply(list_data,load_local)
load("WestBend.RData")
load("Verona.RData")
load("WestInvite.RData")
load("MarquetteInvite.RData")
load("JnsvInvite.RData")
library(xlsx)
library(plyr)
library(dplyr)
library(reshape2)
source("global.R")
library(tidyr)

# #Final funtions for prepping master file NOW IN GLOBAL FILE
# #insert the squad
# insert_squad <- function(df,squad_name) {
#   squad_name1 <- rep(c(squad_name),times=nrow(df))
#   df <- cbind.data.frame(df[,1],squad_name1,df[,2:10])
#   names(df)[2] <- "squad_name"
#   return(df)
# }
# 
# name_race_fix <- function(df){
#   names(df)[1] <- "race_name"
#   df
# }

Verona_Varsity_noDiv1 <- insert_squad(Verona_Varsity_noDiv,"Varsity")

Verona_JV_Final <- insert_squad(Verona_JV,"Junior Varsity")
WestInvite_V_Final <- insert_squad(WestInvite_V,"Varsity")
WestInvite_JV_Final <- insert_squad(WestInvite_JV,"Junior Varsity")

squad_WestBend <- df_WestBend$class
squad_WestBend <- gsub("FR","FR-SO",squad_WestBend)
squad_WestBend <- gsub("SO","FR-SO",squad_WestBend)
squad_WestBend <- gsub("JR","JR-SR",squad_WestBend)
squad_WestBend <- gsub("SR","JR-SR",squad_WestBend)
squad_WestBend <- gsub("FR-SO-SO","FR-SO",squad_WestBend)
squad_WestBend <- gsub("JR-JR-SR", "JR-SR", squad_WestBend)
squad_WestBend <- gsub("FR-FR-SO","FR-SO",squad_WestBend)
WestBend_Final <- cbind.data.frame(df_WestBend[,1],squad_WestBend,df_WestBend[2:10])
names(WestBend_Final)[2] <- "squad_name"
names(WestBend_Final)[3] <- "date_race"

#now sort the West Bend races by squad?
WestBend_Final <- arrange(WestBend_Final,squad_name)

list_df <- list(WestBend_Final,Verona_Varsity_noDiv1,Verona_JV_Final,WestInvite_V_Final,WestInvite_JV_Final)

fix_dfs <- lapply(list_df,name_race_fix)


WestGirlsCC_CumResults_12Sept2015Rev <- rbind(fix_dfs[[1]],fix_dfs[[2]],fix_dfs[[3]],
                                           fix_dfs[[4]],fix_dfs[[5]])

save(WestGirlsCC_CumResults_12Sept2015Rev,file="WestGirlsCumData.RData")

#Insert
MarquetteInvite_V_Final<- insert_squad(MarquetteInvite_V,"Varsity")
MarquetteInvite_V_Final <- name_race_fix(MarquetteInvite_V_Final)

MarquetteInvite_JV_Final<- insert_squad(MarquetteInvite_JV,"Junior Varsity")
MarquetteInvite_JV_Final <- name_race_fix(MarquetteInvite_JV_Final)

write.xlsx(WestGirlsCC_CumResults_12Sept2015Rev,"WestGirlsCC_CumResults_12Sept2015Rev.xlsx",
           sheetName="2015 Results", row.names=FALSE)

WestGirlsCC_CumResults_19Sept2015 <- rbind(WestGirlsCC_CumResults_12Sept2015Rev,
                                           MarquetteInvite_V_Final,MarquetteInvite_JV_Final)

#fix names
clean_df <- WestGirlsCC_CumResults_19Sept2015
clean_df$name <- gsub("Isabelle Bartholom","Isabelle Bartholomew",clean_df$name)
clean_df$name <- gsub("Isabelle Bartholomewew","Isabelle Bartholomew",clean_df$name)
clean_df$name <- gsub("Ella Goetzler","Ella Gotzler",clean_df$name)
clean_df$name <- gsub("Anna Rumbelon","Anna Rumbelow",clean_df$name)
clean_df$name <- gsub("Leyla Moy","Leyla May",clean_df$name)
clean_df$name <- gsub("st...","st",clean_df$name)
clean_df$name <- gsub("Maya McAuliffe-Sc", "Maya McAuliffe-Schroeder", clean_df$name)
clean_df$name <- gsub("May McAuliffe-Schroeder", "Maya McAuliffe-Schroeder", clean_df$name)


test_names <- sort(clean_df[clean_df$school=="Madison West",5])
unique(test_names)
WestGirlsCC_CumResults_19Sept2015 <- clean_df

save(WestGirlsCC_CumResults_19Sept2015,file="WestGirlsCumData.RData")
write.xlsx(WestGirlsCC_CumResults_19Sept2015,"WestGirlsCC_CumResults_19Sept2015.xlsx",
           sheetName="2015 Results", row.names=FALSE)



#26 Sept
load("WestGirlsCumData.RData")
#insert squad names for Janesville
JnsvI_Varsity_Final <- insert_squad(JnsvI_Varsity,"Varsity")
JnsvI_Varsity_Final <- name_race_fix(JnsvI_Varsity_Final)
JnsvI_JV_Final <- insert_squad(JnsvI_JV,"Junior Varsity")
JnsvI_JV_Final <- name_race_fix(JnsvI_JV_Final)
JnsvI_FR_SO_Final <- name_race_fix(insert_squad(JnsvI_FR_SO, "FR-SO"))

WestGirlsCC_CumResults_26Sept2015 <- rbind(WestGirlsCC_CumResults_19Sept2015,
                                           JnsvI_Varsity_Final,JnsvI_JV_Final,JnsvI_FR_SO_Final)


#fix names
clean_df <- WestGirlsCC_CumResults_26Sept2015
clean_df$name <- gsub("Isabelle Bartholom","Isabelle Bartholomew",clean_df$name)
clean_df$name <- gsub("Isabelle Bartholomewew","Isabelle Bartholomew",clean_df$name)
clean_df$name <- gsub("Ella Goetzler","Ella Gotzler",clean_df$name)
clean_df$name <- gsub("Anna Rumbelon","Anna Rumbelow",clean_df$name)
clean_df$name <- gsub("Leyla Moy","Leyla May",clean_df$name)
clean_df$name <- gsub("st...","st",clean_df$name)
clean_df$name <- gsub("Maya McAuliffe-Sc", "Maya McAuliffe-Schroeder", clean_df$name)
clean_df$name <- gsub("May McAuliffe-Schroeder", "Maya McAuliffe-Schroeder", clean_df$name)
clean_df$name <- gsub("Rachel Babic", "Rahel Babic", clean_df$name)
clean_df$name <- gsub("Rachel Babik", "Rahel Babic",clean_df$name)
clean_df$name <- gsub("Christ Weygandt","Christina Weygandt",clean_df$name)

test_names <- sort(clean_df[clean_df$school=="Madison West",5])
unique(test_names)
WestGirlsCC_CumResults_26Sept2015Rev1 <- clean_df

save(WestGirlsCC_CumResults_26Sept2015Rev1,file="WestGirlsCumData.RData")
write.xlsx(WestGirlsCC_CumResults_26Sept2015Rev1,"WestGirlsCC_CumResults_26Sept2015Rev1.xlsx",
           sheetName="2015 Results", row.names=FALSE)



load("WestGirlsCumData.RData")
#add Big 8 invite data
WestGirlsCC_CumResults_29Sept2015 <- rbind(WestGirlsCC_CumResults_26Sept2015Rev1,
                                           dfBig8_So_29Sept_Final,dfBig8_Fr_29Sept_Final,
                                           dfBig8_Jr_29Sept_Final,dfBig8_Sr_29Sept_Final)

test_names <- sort(WestGirlsCC_CumResults_29Sept2015[WestGirlsCC_CumResults_29Sept2015$school=="Madison West",5])
unique(test_names)
clean_df <- WestGirlsCC_CumResults_29Sept2015
clean_df$name <- gsub("Emma Tracy","Emma Tracey",clean_df$name)
clean_df$name <- gsub("Rachel Babic", "Rahel Babic", clean_df$name)
clean_df$name <- gsub("Rachel Babik", "Rahel Babic",clean_df$name)
WestGirlsCC_CumResults_29Sept2015 <- clean_df

save(WestGirlsCC_CumResults_29Sept2015,file="WestGirlsCumData.RData")
test_names <- sort(WestGirlsCC_CumResults_29Sept2015[WestGirlsCC_CumResults_29Sept2015$school=="Madison West",5])
unique(test_names)
write.xlsx(WestGirlsCC_CumResults_29Sept2015,"WestGirlsCC_CumResults_29Sept2015.xlsx",
           sheetName="2015 Results", row.names=FALSE)

##########################10 Oct 2015 update
load("WestGirlsCumData.RData")
WestGirlsCC_CumResults_9Oct2015 <- rbind(WestGirlsCC_CumResults_29Sept2015,
                                         dfMadCity_V_9Oct_Final,dfMadCity_JV_9Oct_Final)

test_names <- sort(WestGirlsCC_CumResults_9Oct2015[WestGirlsCC_CumResults_9Oct2015$school=="Madison West",5])
unique(test_names)

clean_df <- WestGirlsCC_CumResults_9Oct2015
clean_df$name <- gsub("Leyla Moy","Leyla May",clean_df$name)
clean_df$name <- gsub("Rachel Babic", "Rahel Babic", clean_df$name)
clean_df$name <- gsub("Sara Schmidt", "Sarah Schmitt", clean_df$name)
WestGirlsCC_CumResults_9Oct2015 <- clean_df
test_names <- sort(WestGirlsCC_CumResults_9Oct2015[WestGirlsCC_CumResults_9Oct2015$school=="Madison West",5])
unique(test_names)

write.xlsx(WestGirlsCC_CumResults_9Oct2015,"WestGirlsCC_CumResults_9Oct2015Rev.xlsx",
           sheetName="2015 Results", row.names=FALSE)

save(WestGirlsCC_CumResults_9Oct2015,file="WestGirlsCumData.RData")

#########################18 Oct 2015 update
load("WestGirlsCumData.RData")
load("JnsvBig8.RData")
WestGirlsCC_CumResults_17Oct2015 <- rbind(WestGirlsCC_CumResults_9Oct2015,
                                        JnsvB8_Varsity_Final,
                                        JnsvB8_JV1_Final,
                                        JnsvB8_JV2_Final)

test_names <- sort(WestGirlsCC_CumResults_17Oct2015[WestGirlsCC_CumResults_17Oct2015$school=="Madison West",5])
unique(test_names)
clean_df <- WestGirlsCC_CumResults_17Oct2015
clean_df$name <- gsub("Sara Schmidt", "Sarah Schmitt", clean_df$name)
sort(unique(clean_df[clean_df$school=="Madison West",5]))


write.xlsx(WestGirlsCC_CumResults_17Oct2015,"WestGirlsCC_CumResults_17Oct2015.xlsx",
           sheetName="2015 Results", row.names=FALSE)
write.xlsx(WestGirlsCC_CumResults_17Oct2015[West],"WestGirlsCC_CumResults_17Oct2015.xlsx",
           sheetName="West Only", row.names=FALSE,append=TRUE)

#create Madison West Table of times in date order
WestGirlsCC_CumResults_17Oct2015_WestOnly <- droplevels(WestGirlsCC_CumResults_17Oct2015[
                          WestGirlsCC_CumResults_17Oct2015$school=="Madison West",])

write.xlsx(WestGirlsCC_CumResults_17Oct2015_WestOnly,"WestGirlsCC_CumResults_17Oct2015.xlsx",
           sheetName="West Only", row.names=FALSE,append=TRUE)

# WestRaceTimes1 <- WestGirlsCC_CumResults_17Oct2015_WestOnly[,c(3,5,8)]
# WestRaceTimesTable <- dcast(WestRaceTimes1, name ~ date_race )
WestRaceTimes2 <- WestGirlsCC_CumResults_17Oct2015_WestOnly[,c(3,5,6,8)]
WestRaceTimesTable<- dcast(WestRaceTimes2, name+class ~ date_race)


write.xlsx(WestRaceTimesTable,"WestGirlsCC_CumResults_17Oct2015.xlsx",
           sheetName="Times Table", row.names=FALSE,append=TRUE)


#24 Oct 2015 update
load("WestGirlsCumData.RData")
WestGirlsCC_CumResults_24Oct2015 <- rbind(WestGirlsCC_CumResults_17Oct2015,
                                          Sectionals_Final)

test_names <- sort(WestGirlsCC_CumResults_24Oct2015[WestGirlsCC_CumResults_24Oct2015$school=="Madison West",5])
unique(test_names)

clean_df <- WestGirlsCC_CumResults_24Oct2015
clean_df$name <- gsub("Sara Schmidt", "Sarah Schmitt", clean_df$name)
sort(unique(clean_df[clean_df$school=="Madison West",5]))

WestGirlsCC_CumResults_24Oct2015 <- clean_df

write.xlsx(WestGirlsCC_CumResults_24Oct2015,"WestGirlsCC_CumResults_24Oct2015.xlsx",
           sheetName="2015 Results", row.names=FALSE)
write.xlsx(WestGirlsCC_CumResults_24Oct2015[West],"WestGirlsCC_CumResults_24Oct2015.xlsx",
           sheetName="West Only", row.names=FALSE,append=TRUE)

#create Madison West Table of times in date order
WestGirlsCC_CumResults_24Oct2015_WestOnly <- droplevels(WestGirlsCC_CumResults_24Oct2015[
  WestGirlsCC_CumResults_24Oct2015$school=="Madison West",])

save(WestGirlsCC_CumResults_24Oct2015,file="WestGirlsCumData.RData")
write.xlsx(WestGirlsCC_CumResults_24Oct2015_WestOnly,"WestGirlsCC_CumResults_24Oct2015.xlsx",
           sheetName="West Only", row.names=FALSE,append=TRUE)

# WestRaceTimes1 <- WestGirlsCC_CumResults_24Oct2015_WestOnly[,c(3,5,8)]
# WestRaceTimesTable <- dcast(WestRaceTimes1, name ~ date_race )
WestRaceTimes2 <- WestGirlsCC_CumResults_24Oct2015_WestOnly[,c(3,5,6,8)]
WestRaceTimesTable<- dcast(WestRaceTimes2, name+class ~ date_race)


write.xlsx(WestRaceTimesTable,"WestGirlsCC_CumResults_24Oct2015.xlsx",
           sheetName="Times Table", row.names=FALSE,append=TRUE)

#31 Oct 2015 State Finals update
load("WestGirlsCumData.RData")
WestGirlsCC_CumResults_31Oct2015 <- rbind(WestGirlsCC_CumResults_24Oct2015,
                                          State_Final)

test_names <- sort(WestGirlsCC_CumResults_31Oct2015[WestGirlsCC_CumResults_31Oct2015$school=="Madison West",5])
unique(test_names)

write.xlsx(WestGirlsCC_CumResults_31Oct2015,"WestGirlsCC_CumResults_31Oct2015.xlsx",
           sheetName="2015 Results", row.names=FALSE)
WestOnly <- droplevels(WestGirlsCC_CumResults_31Oct2015[WestGirlsCC_CumResults_31Oct2015$school=="Madison West",])
write.xlsx(WestOnly,"WestGirlsCC_CumResults_31Oct2015.xlsx",sheetName="West Only", row.names=FALSE,append=TRUE)

WestRaceTimes2 <- WestOnly[,c(3,5,6,8)]
WestRaceTimesTable<- dcast(WestRaceTimes2, name+class ~ date_race)


write.xlsx(WestRaceTimesTable,"WestGirlsCC_CumResults_31Oct2015.xlsx",
           sheetName="Times Table", row.names=FALSE,append=TRUE)

save(WestGirlsCC_CumResults_31Oct2015,file="WestGirlsCumData.RData")

###############################
# test of individual girl sheet
###############################
library(rmarkdown)
WestOnly$name <- as.factor(WestOnly$name)
WestOnly$correct_pace <- sapply(WestOnly$PaceTime_Min,convert_pace)

save(WestOnly,file="WestOnly.RData")
names_girls <- levels(WestOnly$name)
