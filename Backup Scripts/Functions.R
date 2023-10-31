## Libraries ##
library(httr)
library(dplyr)
library(ggplot2)
library(e1071)
library(caret)
library(XML)
library(readxl)
library(tidyr)
library(tidyverse)
library(openxlsx)
# # pacman::p_load_current_gh("sportsdataverse/cfbfastR", dependencies = TRUE, update = TRUE)
library(cfbfastR)
library(zoo)
library(RCurl)
library(rvest)
library (ISLR2)
library(leaps)
usethis::edit_r_environ()
#Sys.setenv(CFBD_API_KEY = "OJOCuvsvPW4SIhLZPbr3/Pw8vaoASLKFX5yKTD28+Su7YvPZAPfHQl55pB3CADjm")

## Export Workbook Function ###
# model<-final.model
# WinProb<-prob.model
# year<-2023
# wk<-9
# date<-today()
# wb<-loadWorkbook('../Tools/CFB Prediction Tool - Week 8 - 2023 (2023-10-21).xlsx')
Gen_Tool_cfb<-function(model = diff.model, WinProb = winProb.model, year = 2023, wk, date = today(), wb){
  
  ## Export coefficients 
  (coefs<-t(data.frame(model$coefficients)))
  writeData(wb, 'Coefficients', coefs)
  
  (coefs_prob<-t(data.frame(WinProb$coefficients)))
  writeData(wb, 'Coefficients - Win Probability', coefs_prob)
  
  ## Initiate team info dataframe
  teams<-cfbd_team_info()%>%
    select(team_id,school, conference)
  
  ## Pull current ELO data
  teams<-left_join(teams, cfbd_ratings_elo(year=year, week=wk)%>%
                     select(team, elo)%>%
                     rename(school=team),
                   by="school")
  
  ## Pull Current FPI
  teams<-left_join(teams, espn_ratings_fpi(year)%>%
                     select(team_id, fpi),
                   by = "team_id")
  
  ## Pull Predictive Power Rank
  url <- paste0("https://www.teamrankings.com/ncf/rankings/teams/?date=",date)
  html_code <- read_html(url) # Read the HTML code of the page
  table_html <- html_code %>% html_nodes("table") %>% .[[1]] # Use the html_nodes function to extract the table
  
  # Use the html_table function to convert the table HTML code into a data frame
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
  
  # Confirm name matching
  name_match_temp<-data.frame(
    PwrRk = table_df$Team[order(table_df$Team)],
    team_info_name = team_info.dat$school[order(team_info.dat$school)]
  )
  name_match_temp
  
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
  PwrRk.dat$PWr_Rk_Avg<-rowMeans(PwrRk.dat[,-1])
  
  PwrRk.dat<-PwrRk.dat%>%
    select(school, PWr_Rk_Avg)
  teams<-left_join(teams, PwrRk.dat, by="school")
  
  ## Postgame WP - Need to make 2 seperate dataframes then combine
  # home team wp
  home_post_wp.dat<-cfbd_game_info(year, season = "regular")%>%
    filter(week<wk)%>%
    mutate(school = home_team)%>%
    mutate(post_win_prob = home_post_win_prob)%>%
    select(school, post_win_prob)
  # away team wp
  away_post_wp.dat<-cfbd_game_info(year, season = "regular")%>%
    filter(week<wk)%>%
    mutate(school = away_team)%>%
    mutate(post_win_prob = away_post_win_prob)%>%
    select(school, post_win_prob)
  # combine
  post_wp.dat<-rbind(home_post_wp.dat,
                     away_post_wp.dat)
  # clean up
  names(post_wp.dat)<-c("school","postgame_wp")
  post_wp.dat$postgame_wp<-round(as.numeric(post_wp.dat$postgame_wp),3)
  post_wp.dat<-data.frame(aggregate(postgame_wp ~ school, data=post_wp.dat, mean))
  # join
  teams<-left_join(teams, post_wp.dat, by="school")
  
  ## SP Plus
  teams<-left_join(teams,cfbd_ratings_sp(year)%>%
                     select(team, rating)%>%
                     rename(school = team)%>%
                     rename(sp_plus = rating),
                   #filter(school == home_team[i]| school == away_team[i]),
                   by = c("school"))
  ## AP Rank
  temp.AP<-cfbd_rankings(year, week = wk)%>%
    filter(poll == "AP Top 25")%>%
    rename(year = season)%>%
    select(year, school, rank)%>%
    rename(AP_rank = rank)
  # make binary - ranked/not ranked
  temp.AP$AP_rank<-ifelse(temp.AP$AP_rank>0, 1,0)
  temp.AP<-temp.AP[,-1]
  teams<-left_join(teams, temp.AP, by = "school")
  
  ## SRS
  # read in data
  url <- paste0("https://www.sports-reference.com/cfb/years/",year,"-ratings.html")
  html_code <- read_html(url) # Read the HTML code of the page
  table_html <- html_code %>% html_nodes("table") %>% .[[1]] # Use the html_nodes function to extract the table
  # Use the html_table function to convert the table HTML code into a data frame
  table_df <- table_html %>% html_table()
  # Inspect the first few rows of the data frame
  head(table_df)
  #correct column names
  colnames(table_df)<-table_df[1,]
  table_df<-table_df[-1,]
  # SRS name Changes to align with Schedule
  table_df$School[which(!table_df$School %in% team_info.dat$school)]
  table_df$School<-ifelse(table_df$School=="Miami (FL)", "Miami",
                          ifelse(table_df$School=="Texas Christian", "TCU",
                                 ifelse(table_df$School=="Louisiana-Monroe", "Louisiana Monroe",
                                        ifelse(table_df$School=="Middle Tennessee State", "Middle Tennessee",
                                               ifelse(table_df$School=="UTSA", "UT San Antonio",
                                                      ifelse(table_df$School=="Nevada-Las Vegas", "UNLV",
                                                             ifelse(table_df$School=="North Carolina State", "NC State",
                                                                    ifelse(table_df$School=="Hawaii", "Hawai'i",
                                                                           ifelse(table_df$School=="Massachusetts", "UMass",
                                                                                  ifelse(table_df$School=="Pitt", "Pittsburgh",
                                                                                         ifelse(table_df$School=="Sam Houston", "Sam Houston State",      
                                                                                                ifelse(table_df$School=="San Jose State", "San José State", table_df$School))))))))))))
  
  # select necessary data
  table_df<-table_df%>%
    select(c(School, SRS, W, L))%>%
    rename(school = School)
  table_df$W<-as.numeric(table_df$W)
  table_df$L<-as.numeric(table_df$L)
  
  # Calculate winning percentage and remove unneeded W/L columns
  table_df$win_pct<-(table_df$W/(table_df$W+table_df$L))
  table_df$W<-NULL
  table_df$L<-NULL
  teams<-left_join(teams, table_df, by = "school")
  
  ## FEI Efficiency Data 
  url <- paste0("https://www.bcftoys.com/",year,"-fei/")
  html_code <- read_html(url)
  table_html <- html_code %>% html_nodes("table") %>% .[[1]]
  # Use the html_table function to convert the table 
  # HTML code into a data frame
  table_df <- table_html %>% html_table()
  # Inspect the first few rows of the data frame
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
  teams<-left_join(teams, FEI.dat, by = "school")
  
  # Reorder for workbook
  coefs
  names(teams)
  teams<-teams[, c(1:3, 6,11, 4, 10, 5, 12, 8, 7, 9)]
  writeData(wb, 'Data', teams)
  # Save
  saveWorkbook(wb, paste0("../Tools/CFB Prediction Tool - Week ",wk," - ", year, " (", Sys.Date(),").xlsx"), overwrite = T)
}
