source("./Functions.R")

### Specify Week ###
# date<-"2023-09-14"
# date<-"2023-09-20"
# date<-"2023-09-27"
# date<-"2022-12-01"
# date<-"2022-11-22"
# date<-"2022-11-15"
# date<-"2022-11-08"
# date<-"2021-12-02"
# date<-"2021-11-23"
# date<-"2021-11-15"
# date<-"2021-11-09"
# date<- "2023-10-11"
# date<-"2023-10-19"
date<-"2023-10-25"

# wk<-3
# wk<-4
# wk<-5
# wk<-14
# wk<-13
# wk<-12
# wk<-11
# wk<-15
# wk<-14
# wk<-13
# wk<-12
# wk<-7
# wk<-8
wk<-9

# yr<-2022
# yr<-2021
yr<-2023


### Set up data ###
## Read in Schedules for Week
# ESPN
espn_schedule<-espn_cfb_schedule(yr, week=wk)%>%
  select(game_id, home_team_location, away_team_location, home_score, away_score)
espn_schedule$game_id<-as.integer(espn_schedule$game_id)

#CFBD
cfbd_schedule<-cfbd_game_info(year=yr, week=wk)%>%
  select(game_id,season, home_team, away_team)

#Join Schedules
game_schedule<-left_join(cfbd_schedule, espn_schedule, by = "game_id")

game_schedule<-game_schedule%>%
  select(game_id, season, home_team, away_team, home_team_location, away_team_location)%>%
  rename(year = season)%>%
  rename(home_team_espn = home_team_location)%>%
  rename(away_team_espn = away_team_location)

## Initiate team info fo all teams
team_info.dat<-cfbd_team_info(year = yr)%>%
  select(team_id, school, abbreviation, conference)

## Specify Home and Away teams from CFBD and ESPN Data
home_team<-game_schedule$home_team
away_team<-game_schedule$away_team
espn_home<-game_schedule$home_team_espn
espn_away<-game_schedule$away_team_espn

## Read in SRS Data
url <- paste0("https://www.sports-reference.com/cfb/years/",yr,"-ratings.html")
html_code <- read_html(url) # Read the HTML code of the page
table_html <- html_code %>% html_nodes("table") %>% .[[1]] # Use the html_nodes function to extract the table
# Use the html_table function to convert the table 
srs.dat.raw <- table_html %>% html_table() # HTML code into a data frame
head(srs.dat.raw)
colnames(srs.dat.raw)<-srs.dat.raw[1,]
srs.dat.raw<-srs.dat.raw[-1,]

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
## FPI Rankings
# Load
fpi.names<-espn_ratings_fpi(yr)%>%
  select(team_id,year,team_name,fpi)

#Join FPI with team_info.dat for name alignment
fpi.dat<-left_join(fpi.names, team_info.dat, by = "team_id")%>%
  select(year, school, fpi)

## FEI Efficiency Data\
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
  select(Team, FEI, OFEI, DFEI)%>%
  rename(school = Team)

## Power Rankings from teamrankings.com
url <- paste0("https://www.teamrankings.com/ncf/rankings/teams/?date=",date)
html_code <- read_html(url)
table_html <- html_code %>% html_nodes("table") %>% .[[1]]
table_df <- table_html %>% html_table()
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
Weeks_Games<-data.frame()

##### Loop through to collect all data #####
#i<-3
for (i in 1:length(home_team)) {
  temp.dat<-NULL
  # Initiate
  temp.dat<-data.frame(
    year = c(yr,yr),
    week = wk,
    school = c(home_team[i], away_team[i])
  )
  # Talent
  temp.dat<-left_join(temp.dat,cfbd_team_talent(year=yr)%>%
                        filter(school == home_team[i] | school == away_team[i])%>%
                        select(year, school, talent), 
                      by = c("year", "school"))
  
  # Recruiting
  temp.dat<-left_join(temp.dat, cfbd_recruiting_team(yr)%>%
                        filter(team == home_team[i] | team == away_team[i])%>%
                        select(year,team, points)%>%
                        rename(recruiting_score = points)%>%
                        rename(school = team), 
                      by = c("year", "school"))
  
  # Transfer - (needs manipulations)
  transfer.dat<-cfbd_recruiting_transfer_portal(yr)%>%
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
  temp.dat<-left_join(temp.dat,cfbd_ratings_elo(yr, week = wk)%>%
                        rename(school = team)%>%
                        filter(school == home_team[i] | school == away_team[i])%>%
                        select(-conference), 
                      by = c("year", "school"))
  
  # SRS - (needs manipulations)
  names(srs.dat.raw)[2]<-"team"
  srs.dat.raw<-srs.dat.raw[,1:9]
  srs.dat.temp<-srs.dat.raw%>%
    filter(team == home_team[i] | team == away_team[i])%>%
    mutate(year = yr)%>%
    rename(school = team)%>%
    select(year, school, W,L,SRS)
  srs.dat.temp$year<-as.integer(srs.dat.temp$year)
  temp.dat<-temp.dat[order(temp.dat$school, decreasing = F),]
  srs.dat.temp<-srs.dat.temp[order(srs.dat.temp$school, decreasing = F),]
  temp.dat<-cbind(temp.dat, srs.dat.temp)
  temp.dat<-temp.dat[,c(1,2,3,4,5,6,7,10,11,12)]
  
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
                        filter(week==wk)%>%
                        select(year, home,spread, home_win_prob, away_win_prob)%>%
                        rename(school = home),
                      by = c("year", "school"))
  if(yr == 2021){
    temp.dat<-temp.dat[1:2, ]
  }else{
    
  }
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

  names(temp.dat)
  temp.dat<-temp.dat<-temp.dat[,c(ncol(temp.dat),1:3,8,9,11,12,13,15,16,7,10,17,18,4,5,6,14)]
  
  # Fix NA's in Win Prob
  temp.dat[which(temp.dat$school == away_team[i]),11]<-temp.dat$away_win_prob[which(temp.dat$school==home_team[i])]
  temp.dat[which(temp.dat$school == away_team[i]),10]<-temp.dat$home_win_prob[which(temp.dat$school==home_team[i])]
  temp.dat[which(temp.dat$school == away_team[i]),ncol(temp.dat)]<-(-1*temp.dat$spread)[which(temp.dat$school==home_team[i])]
  
  # Advanced Stats
  temp.dat<-left_join(temp.dat,cfbd_stats_season_advanced(year = yr, start_week = 1, end_week = 1)%>%
                        select(season, team, off_total_ppa,off_havoc_total,off_explosiveness,def_total_ppa,def_havoc_total, def_explosiveness)%>%
                        rename(school = team)%>%
                        rename(year = season),
                      by = c("year", "school"))
  
  # Regular Stats
  temp.dat<-left_join(temp.dat,cfbd_stats_season_team(year = yr, start_week = 1, end_week = wk-1)%>%
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
    select(-games)
    
  # Returning Production
  temp.dat<-left_join(temp.dat,cfbd_player_returning(yr)%>%
                        select(season, team, total_ppa, percent_ppa)%>%
                        rename(year = season)%>%
                        rename(returning_ppa = total_ppa)%>%
                        rename(pct_returning_ppa = percent_ppa)%>%
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
  post_wp.dat_new$postgame_wp<-round(as.numeric(post_wp.dat_new$postgame_wp),3)
  post_wp.dat_new<-data.frame(aggregate(postgame_wp ~ school, data=post_wp.dat_new, mean))
  temp.dat<-left_join(temp.dat, post_wp.dat_new, by = c( "school"))                      
  
  # Label Home and Away teams
  temp.dat$is_home<-ifelse(temp.dat$school==home_team[i], "Home" ,"Away")
  temp.dat<-temp.dat[,c(1,2,ncol(temp.dat),3:(ncol(temp.dat)-1))]
  
  # Power Ranking
  temp.dat<-left_join(temp.dat, PwrRk.dat, by = "school")
  names(temp.dat)
  
  # Reordering
  temp.dat<-temp.dat[,c(2,4,1,3,5:16,31:38, 17:19, 21:30, 20)]
  temp.dat<-temp.dat[order(temp.dat$is_home),]

  ## Manipulations for ease of running model ##
  # Create Win percentage
  temp.dat$W<-as.numeric(temp.dat$W)
  temp.dat$L<-as.numeric(temp.dat$L)
  temp.dat<-temp.dat%>%
    mutate(Win_pct = round(W/(W+L),3))
  temp.dat<-temp.dat[,c(1:7,ncol(temp.dat),8:(ncol(temp.dat)-1))]
  
  # Make AP Rank, AP points, and first_place_votes Categorical
  temp.dat<-temp.dat%>%
    mutate(AP_rank = ifelse(!is.na(AP_rank), 1, 0))%>%
    mutate(first_place_votes = ifelse(is.na(first_place_votes), 0, first_place_votes))%>%
    mutate(AP_points = ifelse(is.na(AP_points), 0, AP_points))

  temp.dat<-left_join(temp.dat, FEI.dat, by = "school")
                        
  temp.dat<-cbind(temp.dat[1,], temp.dat[2,]) # for future
  temp.dat<-temp.dat[,-c(43:45)]

  names(temp.dat)[4:42]<-c("away",paste0(names(temp.dat)[5:42],"_away"))
  names(temp.dat)[43:81]<-c("home",paste0(names(temp.dat)[44:81],"_home"))
  names(temp.dat)<-str_remove(names(temp.dat), ".1")
  
  #move game to first column
  temp.dat<-temp.dat[,c(3,1,2,4:ncol(temp.dat))]
  # put home team first
  temp.dat<-temp.dat[,c(1:3,43:ncol(temp.dat), 4:42)]
  temp.dat<-left_join(temp.dat, cfbd_game_info(yr, week=wk)%>%
    select(game_id, home_points, away_points)%>%
    rename(game=game_id),
    by = "game")

  temp.dat<-temp.dat%>%
    mutate(difference = home_points - away_points)%>%
    mutate(total = home_points + away_points)%>%
    mutate(home_win = ifelse(home_points>away_points,1,0))%>%
    mutate(home_cover = ifelse(difference >= spread_away, 1, 0))
  temp.dat$home_win_prob_away<-NULL
  temp.dat$away_win_prob_home<-NULL
  temp.dat<-temp.dat%>%
    rename(win_prob_home = home_win_prob_home)%>%
    rename(win_prob_away = away_win_prob_away)
  Weeks_Games<-rbind(Weeks_Games,temp.dat[1,])
}

write.xlsx(Weeks_Games, paste0("./Output/Modeling/Week_", wk,"_year_",yr,"_Data.xlsx"))
