library(httr)
library(caTools)
library(neuralnet)
library(dplyr)
library(ggplot2)
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
date<-"2023-09-25"
wk<-4
yr<-2023

### Set up data ###
## Read in Schedules for Week


### Initiate team info fo all teams ###
team_info.dat<-cfbd_team_info(year = yr)%>%
  select(team_id, school, abbreviation, conference)


### SRS Data ###
url <- paste0("https://www.sports-reference.com/cfb/years/",yr,"2023-ratings.html")
html_code <- read_html(url)
table_html <- html_code %>% html_nodes("table") %>% .[[1]]
# Use the html_table function to convert the table 
# HTML code into a data frame
srs.dat.raw<- table_html %>% html_table()
# Inspect the first few rows of the data frame
head(srs.dat.raw)
colnames(srs.dat.raw)<-srs.dat.raw[1,]
srs.dat.raw<-srs.dat.raw[-1,c(2,5,6,7,8,9)]

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
# Confirm change
srs.dat.raw$School[which(!srs.dat.raw$School %in% team_info.dat$school)]
srs.dat.raw<-srs.dat.raw[-which(!srs.dat.raw$School %in% team_info.dat$school),]

### FPI Rankings ###
## Load
fpi.names<-espn_ratings_fpi(yr)%>%
  select(team_id,year,team_name,fpi)

#Join FPI with team_info.dat for name alignment
fpi.dat<-left_join(fpi.names, team_info.dat, by = "team_id")%>%
  select(year, school, fpi)

### FEI Efficiency Data ###
url <- paste0("https://www.bcftoys.com/",yr,"-fei/")
html_code <- read_html(url)
table_html <- html_code %>% html_nodes("table") %>% .[[1]]
table_df <- table_html %>% html_table()
head(table_df)
colnames(table_df)<-table_df[2,]
table_df<-table_df[-c(1:2),]
table_df<-table_df[-c(which(is.na(table_df$Team))),]
table_df$Team[which(!table_df$Team%in%team_info.dat$school)]
table_df$Team<-ifelse(table_df$Team=="Massachusetts", "UMass", 
       ifelse(table_df$Team == "Sam Houston", "Sam Houston State",
              ifelse(table_df$Team=="Hawaii", "Hawai'i", 
                     ifelse(table_df$Team=="UTSA", "UT San Antonio", 
                            ifelse(table_df$Team=="UL Monroe", "Louisiana Monroe", 
                                   ifelse(table_df$Team == "San Jose State", "San José State",table_df$Team))))))
FEI.dat<-table_df%>%
  select(Team, FEI)%>%
  rename(school = Team)

### Power Rankings from teamrankings.com ###
url <- paste0("https://www.teamrankings.com/ncf/rankings/teams/?date=",date)
html_code <- read_html(url)
table_html <- html_code %>% html_nodes("table") %>% .[[1]]
table_df <- table_html %>% html_table()
head(table_df)
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
Weeks_Games<-data.frame()
cfb_teams<-team_info.dat$school
##### Loop through to collect all data #####
#i<-7
for (i in 1:length(team_info.dat$school)) {
  temp.dat<-NULL
  # Initiate
  temp.dat<-data.frame(
    year = yr,
    school = cfb_teams
  )
  # Talent
  temp.dat<-left_join(temp.dat,cfbd_team_talent(year=yr)%>%
                        #filter(school == cfb_teams)%>%
                        select(year, school, talent), 
                      by = c("year", "school"))
  
  # Recruiting
  temp.dat<-left_join(temp.dat, cfbd_recruiting_team(yr)%>%
                        # filter(team == home_team[i] | team == away_team[i])%>%
                        select(year,team, points)%>%
                        rename(recruiting_score = points)%>%
                        rename(school = team), 
                      by = c("year", "school"))
  
  # Transfer - (needs manipulations)
  transfer.dat<-cfbd_recruiting_transfer_portal(yr)%>%
    #filter(origin == home_team[i] | origin == away_team[i] | destination == home_team[i | destination == away_team[i]])%>%
    select(season, first_name, last_name, origin, destination, rating, stars)%>%
    rename(year = season)
  transfer.dat[order(transfer.dat$rating, decreasing = T),]
  transfer.dat$origin.rating<-(-1*as.numeric(transfer.dat$rating))
  transfer.dat$destination.rating<- as.numeric(transfer.dat$rating)                                                                  
  transfer.dat$origin<-ifelse(is.na(transfer.dat$origin), "", transfer.dat$origin)   
  transfer.dat$destination<-ifelse(is.na(transfer.dat$destination), "", transfer.dat$destination)   
  transfer.dat.origin<-transfer.dat%>%
    group_by(origin)%>%
    summarise(departure_score = sum(origin.rating))%>%
    mutate(school = origin)%>%
    select(school, departure_score)
  transfer.dat.destination<-transfer.dat%>%
    group_by(destination)%>%
    summarise(arrival_score = sum(destination.rating))%>%
    mutate(school=destination)%>%
    select(school, arrival_score)
  transfer.dat<-left_join(transfer.dat.origin, transfer.dat.destination)
  
  transfer.dat$departure_score<-ifelse(is.na(transfer.dat$departure_score), 0, transfer.dat$departure_score)
  transfer.dat$arrival_score<-ifelse(is.na(transfer.dat$arrival_score), 0, transfer.dat$arrival_score)  
  transfer.dat$portal_score<-transfer.dat$arrival_score-transfer.dat$departure_score
         
  temp.dat<-left_join(temp.dat, transfer.dat[,c(1,4)], by = "school")
  
  # Elo
  temp.dat<-left_join(temp.dat,cfbd_ratings_elo(yr, week = wk)%>%
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
  temp.dat<-left_join(temp.dat,cfbd_rankings(yr, week = wk)%>%
                        filter(poll == "AP Top 25")%>%
                        filter(school == home_team[i] | school==away_team[i])%>%
                        rename(year = season)%>%
                        select(year, school, rank, first_place_votes, points)%>%
                        rename(AP_points = points)%>%
                        rename(AP_rank = rank),
                      by = c("year", "school"))
  
  # Pregame Win Prob
  temp.dat<-left_join(temp.dat,cfbd_metrics_wp_pregame(year = yr, season_type = "regular")%>%
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
  temp.dat<-left_join(temp.dat,cfbd_ratings_sp(yr)%>%
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
  temp.dat<-left_join(temp.dat,cfbd_stats_season_advanced(year = yr, start_week = 1, end_week = wk)%>%
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
                        rename(school = team)%>%
                        rename(year = season),
                      by = c("year", "school"))
  temp.dat<-temp.dat%>%
    mutate(off_total_ppa=off_total_ppa/games)%>%
    mutate(def_total_ppa=def_total_ppa/games)%>%
    mutate(off_havoc_total=off_havoc_total)%>%
    mutate(def_havoc_total=def_havoc_total)%>%
    select(-games)
    
  # Returning Production
  temp.dat<-left_join(temp.dat,cfbd_player_returning(yr)%>%
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
  
  # Reordering
  temp.dat<-temp.dat[,c(2,1,4,3,5:19,34,35,36,37,38,33,20:32 )]
  temp.dat<-temp.dat[order(temp.dat$is_home),]
  
  ## Manipulations for ease of running model ##
  # Create Win percentage
  temp.dat<-temp.dat%>%
    mutate(Win_pct = W/(W+L))
  temp.dat<-temp.dat[,c(1:7,ncol(temp.dat),8:(ncol(temp.dat)-1))]
  
  # Make AP Rank, AP points, and first_place_votes Categorical
  temp.dat<-temp.dat%>%
    mutate(AP_rank = ifelse(!is.na(AP_rank), 1, 0))%>%
    mutate(first_place_votes = ifelse(is.na(first_place_votes), 0, first_place_votes))%>%
    mutate(AP_points = ifelse(is.na(AP_points), 0, AP_points))

  temp.dat<-left_join(temp.dat, FEI.dat, by = "school")
                        
  temp.dat<-cbind(temp.dat[1,], temp.dat[2,]) # for future
  temp.dat<-temp.dat[,-c(41,42,43,44)]

  names(temp.dat)[4:40]<-paste0(names(temp.dat)[4:40],"_away")
  names(temp.dat)[41:76]<-paste0(names(temp.dat)[41:76],"_home")
  names(temp.dat)[41:76]<-str_remove(names(temp.dat)[41:76], ".1")
  
  temp.dat<-left_join(temp.dat, cfbd_game_info(yr, week=wk)%>%
    select(game_id, home_points, away_points)%>%
    rename(game=game_id),
    by = "game")

  temp.dat<-temp.dat%>%
    mutate(difference = home_points - away_points)%>%
    mutate(total = home_points + away_points)%>%
    mutate(home_win = ifelse(home_points>away_points,1,0))%>%
    mutate(home_cover = ifelse(difference >= spread_away, 1, 0))
  
  Weeks_Games<-rbind(Weeks_Games,temp.dat[1,])
}

write.xlsx(Weeks_Games, paste0("./Output/Week_", wk,"_year_",yr,"_Data.xlsx"))


### TO do for full model
# fix differences in team names between CFB Reference ad cfbd
# spread game into one row
# Add Advanced Stats (Add to this model too)
# Add returning prduction (add to this model too)
# incorporate schedule to pull in home team and away team, ideally this will have results and a game ID also

