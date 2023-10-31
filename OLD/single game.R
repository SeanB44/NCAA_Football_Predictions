library(httr)
#library(ElemStatLearn)
library(caTools)
library(neuralnet)
library(dplyr)
library(ggplot2)
library(expss)
library(e1071)
library(caret)
library(XML)
library(readxl)
library(tidyr)
library(tidyverse)
library(openxlsx)
pacman::p_load_current_gh("sportsdataverse/cfbfastR", dependencies = TRUE, update = TRUE)
library(cfbfastR)
library(zoo)
library(ggimage)
library(gt)
library(RCurl)
library(rvest)
#########################
usethis::edit_r_environ()
#Sys.setenv(CFBD_API_KEY = "OJOCuvsvPW4SIhLZPbr3/Pw8vaoASLKFX5yKTD28+Su7YvPZAPfHQl55pB3CADjm")
##########################

### Specify Week ###
date<-"2023-09-22"
wk<-4
yr<-2023

### Set up data ###
# Read in Schedules for Week
espn_schedule<-espn_cfb_schedule(2023, week=wk)%>%
  select(game_id, home_team_location, away_team_location, home_score, away_score)

cfbd_schedule<-cfbd_game_info(year=2023, week=wk)%>%
  select(game_id,season, home_team, away_team)

espn_schedule$game_id<-as.integer(espn_schedule$game_id)

game_schedule<-left_join(cfbd_schedule, espn_schedule, by = "game_id")

game_schedule<-game_schedule%>%
  select(game_id, season, home_team, away_team, home_team_location, away_team_location)%>%
  rename(year = season)%>%
  rename(home_team_espn = home_team_location)%>%
  rename(away_team_espn = away_team_location)

### Initiate team info fo all teams ##
team_info.dat<-cfbd_team_info(year = yr)%>%
  select(team_id, school, abbreviation, conference)

# Specify Home and Away teams
home_team<-game_schedule$home_team
away_team<-game_schedule$away_team
espn_home<-game_schedule$home_team_espn
espn_away<-game_schedule$away_team_espn

### Read in Downloaded SRS Data ###
srs.dat.raw<-data.frame(read.xlsx(paste0("./Data/SRS_2023_WK_",wk,".xlsx"), colNames = TRUE, cols = c(2,5,6,7,8,9)))
# SRS name Changes to align with Schedule
srs.dat.raw$School[which(!srs.dat.raw$School %in% team_info.dat$school)]
srs.dat.raw$School<-ifelse(srs.dat.raw$School=="Miami (FL)", "Miami",
                           ifelse(srs.dat.raw$School=="Texas Christian", "TCU",
                                  ifelse(srs.dat.raw$School=="Louisiana-Monroe", "Louisiana Monroe",
                                         ifelse(srs.dat.raw$School=="Middle Tennessee State", "Middle Tennessee",
                                                ifelse(srs.dat.raw$School=="UTSA", "UT San Antonio",
                                                       ifelse(srs.dat.raw$School=="Nevada-Las Vegas", "UNLV",
                                                              ifelse(srs.dat.raw$School=="North Carolina State", "NC State",
                                                                     ifelse(srs.dat.raw$School=="Hawaii", "Hawai'i",
                                                                            ifelse(srs.dat.raw$School=="Massachusetts", "UMass",
                                                                                   ifelse(srs.dat.raw$School=="Pitt", "Pittsburgh",
                                                                                          ifelse(srs.dat.raw$School=="Sam Houston", "Sam Houston State",      
                                                                                                 ifelse(srs.dat.raw$School=="San Jose State", "San José State", srs.dat.raw$School))))))))))))
### FPI Rankings ###
# Load
fpi.names<-espn_ratings_fpi(2023)%>%
  select(team_id,year,team_name,fpi)
# Join FPI with team_info.dat for name alignment
fpi.dat<-left_join(fpi.names, team_info.dat, by = "team_id")%>%
  select(year, school, fpi)

### Power Rankings from teamrankings.com ###
# URL of the website
url <- paste0("https://www.teamrankings.com/ncf/rankings/teams/?date=",date)
# Read the HTML code of the page
html_code <- read_html(url)
# Use the html_nodes function to extract the table
table_html <- html_code %>% html_nodes("table") %>% .[[1]]
# Use the html_table function to convert the table 
# HTML code into a data frame
table_df <- table_html %>% html_table()
# Inspect the first few rows of the data frame
head(table_df)
# Align names
table_df$Team<-ifelse(table_df$Team=="Central Florida Knights", "UCF", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Florida Atlantic Owls", "Florida Atlantic", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Florida Gators", "Florida", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Louisiana St. Tigers", "LSU", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Louisiana Tech Bulldogs", "Louisiana Tech", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="UL Monroe Warhawks", "Louisiana Monroe", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Louisiana Ragin' Cajuns", "Louisiana", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Massachusetts Minutemen", "UMass", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Miami Hurricanes", "Miami", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Michigan Wolverines", "Michigan", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Mississippi Rebels", "Ole Miss", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Southern Methodist Mustangs", "SMU", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Southern California Trojans", "USC", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Texas Christian Horned Frogs", "TCU", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Texas El Paso Miners", "UTEP", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Texas St. Bobcats", "Texas State", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Texas Longhorns", "Texas", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Texas Tech Red Raiders", "Texas Tech", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Texas-San Antonio Roadrunners", "UT San Antonio", table_df$Team)
table_df$Team<-ifelse(table_df$Team=="Utah Utes", "Utah", table_df$Team)

# # Confirm name matching
# name_match_temp<-data.frame(
#   PwrRk = table_df$Team[order(table_df$Team)],
#   team_info_name = team_info.dat$school[order(team_info.dat$school)]
# )
#view(name_match_temp)

# Order Dataframes for school merging
table_df<-table_df[order(table_df$Team),]
team_info.dat<-team_info.dat[order(team_info.dat$school),]
table_df<-table_df%>%
  select(-`In Conf.`)

# Rename columns
names(table_df)<-c("school","Predictive_PwrRk", "Home_PwrRk", "Away_PwrRk", "Last5_PwrRk", "SOS_PwrRk" )
# Update school name
table_df$school<-team_info.dat$school
# Create Power ranking dataframe
PwrRk.dat<-table_df

### Initiate list - Setting up for collect weeks games ###
Weeks_Games<-list()

##### Loop through to collect all data #####
#i<-9
for (i in 1:length(home_team)) {
temp.dat<-NULL
# Initiate
temp.dat<-data.frame(
  year = c(2023,2023),
  week = wk,
  school = c(home_team[i], away_team[i])
)
# Talent
temp.dat<-left_join(temp.dat,cfbd_team_talent(year=2023)%>%
  filter(school == home_team[i] | school == away_team[i])%>%
  select(year, school, talent), 
  by = c("year", "school"))

# Recruiting
temp.dat<-left_join(temp.dat, cfbd_recruiting_team(2023)%>%
  filter(team == home_team[i] | team == away_team[i])%>%
  select(year,team, points)%>%
  rename(recruiting_score = points)%>%
  rename(school = team), 
  by = c("year", "school"))

# Transfer - (needs manipulations)
transfer.dat<-cfbd_recruiting_transfer_portal(2023)%>%
  filter(origin == home_team[i] | origin == away_team[i] | destination == home_team[i | destination == away_team[i]])%>%
  select(season, first_name, last_name, origin, destination, rating, stars)%>%
  rename(year = season)
transfer.dat[order(transfer.dat$rating, decreasing = T),]
transfer.dat$rating<-ifelse(transfer.dat$origin==home_team[i] | transfer.dat$origin == away_team[i], -1*as.numeric(transfer.dat$rating),
       ifelse(transfer.dat$destination==home_team[i] | transfer.dat$destination == away_team[i],as.numeric(transfer.dat$rating), 0 ))
transfer.dat$rating<-ifelse(is.na(transfer.dat$rating), 0, transfer.dat$rating)                                                                    
transfer.dat$origin<-ifelse(is.na(transfer.dat$origin), "", transfer.dat$origin)   
transfer.dat$destination<-ifelse(is.na(transfer.dat$destination), "", transfer.dat$destination)   
transfer.dat<-transfer.dat%>%
  mutate(school = ifelse(origin == home_team[i] | destination == home_team[i], home_team[i], 
                         ifelse(origin == away_team[i] | destination == away_team[i], away_team[i], "")))%>%
  group_by(school)%>%
  summarise(portal_score = sum(rating))
temp.dat<-left_join(temp.dat, transfer.dat, by = "school")

# Elo
temp.dat<-left_join(temp.dat,cfbd_ratings_elo(2023, week = wk)%>%
  rename(school = team)%>%
  filter(school == home_team[i] | school == away_team[i])%>%
  select(-conference), 
  by = c("year", "school"))

# SRS - (needs manipulations)
srs.dat.temp<-srs.dat.raw%>%
  filter(School == home_team[i] | School == away_team[i])%>%
  mutate(year = 2022)%>%
  rename(school = School)%>%
  select(year, school, W,L,OSRS,DSRS,SRS)
srs.dat.temp$year<-as.integer(srs.dat.temp$year)
temp.dat<-temp.dat[order(temp.dat$school, decreasing = F),]
srs.dat.temp<-srs.dat.temp[order(srs.dat.temp$school, decreasing = F),]
temp.dat<-cbind(temp.dat, srs.dat.temp)
temp.dat<-temp.dat[,c(1,2,3,4,5,6,7,10,11,12,13,14)]

# Transfer Portal
temp.dat$portal_score<-ifelse(is.na(temp.dat$portal_score),0,temp.dat$portal_score)

# Rankings
temp.dat<-left_join(temp.dat,cfbd_rankings(2023, week = wk)%>%
  filter(poll == "AP Top 25")%>%
  filter(school == home_team[i] | school==away_team[i])%>%
  rename(year = season)%>%
  select(year, school, rank, first_place_votes, points)%>%
  rename(AP_points = points)%>%
  rename(AP_rank = rank),
  by = c("year", "school"))

# Pregame Win Prob
temp.dat<-left_join(temp.dat,cfbd_metrics_wp_pregame(year = 2023, season_type = "regular")%>%
  rename(year = season)%>%
  rename(home = home_team)%>%
  rename(away = away_team)%>%
  filter(home==home_team[i])%>%
  filter(away==away_team[i])%>%
  select(year, home,spread, home_win_prob, away_win_prob)%>%
  rename(school = home),
  by = c("year", "school"))

# FPI
fpi.temp<-fpi.dat%>%
  filter(school == home_team[i] | school == away_team[i])
temp.dat<-left_join(temp.dat, fpi.temp, by = c("year","school"))

# SP+ Ranks
temp.dat<-left_join(temp.dat,cfbd_ratings_sp(2023)%>%
                        select(year, team, rating)%>%
                        rename(school = team)%>%
                        rename(sp_plus = rating)%>%
                        filter(school == home_team[i]| school == away_team[i]),
                      by = c("year", "school"))
# Pull in Game ID
temp.dat$game<-game_schedule$game_id[i]
#names(temp.dat)
temp.dat<-temp.dat<-temp.dat[,c(ncol(temp.dat),1:3,8,9,13,14,15,17,18,16,7,12,10,11,19,20,4,5,6 )]
names(temp.dat)

# Fix NA's in Win Prob
temp.dat[which(temp.dat$school == away_team[i]),10]<-temp.dat$away_win_prob[which(temp.dat$school==home_team[i])]
temp.dat[which(temp.dat$school == away_team[i]),11]<-temp.dat$home_win_prob[which(temp.dat$school==home_team[i])]
temp.dat[which(temp.dat$school == away_team[i]),12]<-(-1*temp.dat$spread)[which(temp.dat$school==home_team[i])]

# Advanced Stats
temp.dat<-left_join(temp.dat,cfbd_stats_season_advanced(year = 2023, start_week = 1, end_week = wk)%>%
  select(season, team, off_total_ppa,off_havoc_total,def_total_ppa,def_havoc_total)%>%
  rename(school = team)%>%
  rename(year = season),
  by = c("year", "school"))

# Regular Stats
temp.dat<-left_join(temp.dat,cfbd_stats_season_team(year = yr, start_week = 1, end_week = wk)%>%
                      select(season, team, games,turnovers, third_down_convs, penalty_yds, penalties)%>%
                      mutate(turnovers = turnovers/games)%>%
                      mutate(penalties = penalties/games)%>%
                      mutate(penalty_yds = penalty_yds/games)%>%
                      mutate(third_down_convs = third_down_convs/100)%>%
                      select(-games)%>%
                      rename(school = team)%>%
                      rename(year = season),
                    by = c("year", "school"))

# Returning Production
temp.dat<-left_join(temp.dat,cfbd_player_returning(2023)%>%
  select(season, team, total_ppa, percent_ppa)%>%
  rename(year = season)%>%
  rename(school = team),
  by = c("year", "school"))

# Postgame Win Prob - (Requires some  manipulation)
post_wp.dat<-cfbd_game_info(yr, season = "regular")%>%
  filter(week<wk)%>%
  mutate(home = home_team)%>%
  mutate(away = away_team)%>%
  select(home, away, home_post_win_prob, away_post_win_prob)
post_wp.dat<-post_wp.dat%>%
  filter(home == home_team[i] | home == away_team[i] | away == home_team[i] | away == away_team[i])
post_wp.dat_new<-data.frame(school = "",
                            post_wp = 0)
post_wp.dat_new<-rbind(post_wp.dat[post_wp.dat$home==home_team[i],c(1,3)],
                       post_wp.dat[post_wp.dat$home==away_team[i],c(1,3)],
                       post_wp.dat[post_wp.dat$away==home_team[i],c(2,4)],
                       post_wp.dat[post_wp.dat$away==away_team[i],c(2,4)],use.names=F)
names(post_wp.dat_new)<-c("school","postgame_wp")
post_wp.dat_new$postgame_wp<-as.numeric(post_wp.dat_new$postgame_wp)
post_wp.dat_new<-data.frame(aggregate(postgame_wp ~ school, data=post_wp.dat_new, mean))
temp.dat<-left_join(temp.dat, post_wp.dat_new, by = c( "school"))                      
             
# Label Home and Away teams
temp.dat$is_home<-ifelse(temp.dat$school==home_team[i], "Home" ,"Away")
temp.dat<-temp.dat[,c(1,2,ncol(temp.dat),3:(ncol(temp.dat)-1))]

# Power Ranking
temp.dat<-left_join(temp.dat, PwrRk.dat, by = "school")
names(temp.dat)
# Final reordering
temp.dat<-temp.dat[,c(2,1,4,3,5:19,34,35,36,37,38,33,20:32 )]
Weeks_Games[[i]]<-temp.dat
#view(cbind(temp.dat[1,], temp.dat[2,])) # for future

Weeks_Games[[i]]$postgame_wp<-round(Weeks_Games[[i]]$postgame_wp,3)
Weeks_Games[[i]][,c(30:38)]<-round(Weeks_Games[[i]][,c(30:38)],3)

names(Weeks_Games)[[i]]<-paste0(paste0(home_team[i]))

}

write.xlsx(Weeks_Games, paste0("./Output/Week ", wk," Game Data.xlsx"))




##########################################################################################
# Weeks_Games_stacked<-Weeks_Games[[1]]
# for (j in 2:i) {
#   Weeks_Games_stacked<-rbind(Weeks_Games_stacked,Weeks_Games[[j]])
# }
# Weeks_Games_stacked$postgame_wp<-round(Weeks_Games_stacked$postgame_wp,3)
# Weeks_Games_stacked[,c(30:38)]<-round(Weeks_Games_stacked[,c(30:38)],3)

# write.xlsx(Weeks_Games_stacked, paste0("./Output/Week ", wk," Game Data.xlsx"))

### TO do for full model
# fix differences in team names between CFB Reference ad cfbd
# spread game into one row
# Add Advanced Stats (Add to this model too)
# Add returning prduction (add to this model too)
# incorporate schedule to pull in home team and away team, ideally this will have results and a game ID also

