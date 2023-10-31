# library(httr)
# #library(caTools)
# #library(neuralnet)
# library(dplyr)
# library(ggplot2)
# library(e1071)
# library(caret)
# library(XML)
# library(readxl)
# library(tidyr)
# library(tidyverse)
# library(openxlsx)
# # # pacman::p_load_current_gh("sportsdataverse/cfbfastR", dependencies = TRUE, update = TRUE)
# library(cfbfastR)
# # library(zoo)
# # library(ggimage)
# # library(gt)
# library(RCurl)
# library(rvest)
# # #install.packages("ISLR2")
# library (ISLR2)
# # #install.packages("leaps")
# library(leaps)
# #########################
# usethis::edit_r_environ()
# #Sys.setenv(CFBD_API_KEY = "OJOCuvsvPW4SIhLZPbr3/Pw8vaoASLKFX5yKTD28+Su7YvPZAPfHQl55pB3CADjm")
# ##########################
source('./Functions.R')
####### PREDICTING POINT DIFFERENTIAL #####
### Load Data ###
raw.dat.full<-rbind(read.xlsx("./Output/Modeling/Week_3_year_2023_Data.xlsx"),
      read.xlsx("./Output/Modeling/Week_4_year_2023_Data.xlsx"),
      read.xlsx("./Output/Modeling/Week_5_year_2023_Data.xlsx"),
      read.xlsx("./Output/Modeling/Week_14_year_2022_Data.xlsx"),
      read.xlsx("./Output/Modeling/Week_13_year_2022_Data.xlsx"),
      read.xlsx("./Output/Modeling/Week_12_year_2022_Data.xlsx"),
      read.xlsx("./Output/Modeling/Week_11_year_2022_Data.xlsx"),
      read.xlsx("./Output/Modeling/Week_14_year_2021_Data.xlsx"),
      read.xlsx("./Output/Modeling/Week_13_year_2021_Data.xlsx"),
      read.xlsx("./Output/Modeling/Week_12_year_2021_Data.xlsx"),
      read.xlsx("./Output/Modeling/Week_7_year_2023_Data.xlsx"),
      read.xlsx("./Output/Modeling/Week_8_year_2023_Data.xlsx")
      
      )
###### Data Prep #######
## Initiate team info fo all teams
team_info.dat<-cfbd_team_info(year = 2023)%>%
  select(team_id, school, abbreviation, conference)

## Data Cleaning 
names(raw.dat.full)
raw.dat.full$home_cover<-NULL
raw.dat.full$difference<-(-1*raw.dat.full$difference)
# Put in temporarily desired order
raw.dat.full<-raw.dat.full[,c(1:3,5,43,8:41,46:ncol(raw.dat.full))]
# Filter out teams not in FBS
raw.dat.full$school_home[!raw.dat.full$school_home %in%team_info.dat$school]
raw.dat.full$school_away[!raw.dat.full$school_away %in%team_info.dat$school]
raw.dat.full<-raw.dat.full[-which(!raw.dat.full$school_home %in% team_info.dat$school),]
raw.dat.full<-raw.dat.full[-which(!raw.dat.full$school_away %in% team_info.dat$school),]
# Check NA's
colSums(is.na(raw.dat.full))
# Save down initial cleaned data
full.dat<-raw.dat.full

###### Data Manipulations ######

## Select IVs
IV_idx<-which(colnames(full.dat) %in% c("Win_pct_home", "Win_pct_away",
               "AP_rank_home", "AP_rank_away",                         
              #"win_prob_home", "win_prob_away", 
              "elo_home", "elo_away", 
              "SRS_home", "SRS_away", 
              "fpi_home", "fpi_away",
              "sp_plus_home", "sp_plus_away",
              "returning_ppa_home", "returning_ppa_away",
              "pct_returning_ppa_home", "pct_returning_ppa_away",
              "postgame_wp_home", "postgame_wp_away",
              "Predictive_PwrRk_home", "Predictive_PwrRk_away",
              "Home_PwrRk_home", "Away_PwrRk_away", 
              "Last5_PwrRk_home","Last5_PwrRk_away", 
              "SOS_PwrRk_home","SOS_PwrRk_away",
              #"talent_home", "talent_away",
              "recruiting_score_home", "recruiting_score_away",
              "portal_score_home", "portal_score_away",
              "off_total_ppa_home", "off_total_ppa_away",
              "off_havoc_total_home","off_havoc_total_away",
              "off_explosiveness_home", "off_explosiveness_away",
              "def_total_ppa_home","def_total_ppa_away", 
              "def_havoc_total_home", "def_havoc_total_away",
              "def_explosiveness_home", "def_explosiveness_away",
              "turnovers_home", "turnovers_away",
              "third_down_convs_home", "third_down_convs_away",
              "penalty_yds_home", "penalty_yds_away",
              "FEI_home", "FEI_away",
              "spread_home"#, #"spread_away"#,#Only one spread needed
              # "OFEI_home", "OFEI_away",
              # "DFEI_home","DFEI_away"
))

full.dat<-full.dat[,c(1:5,IV_idx,76,78)] 
for (i in 6:ncol(full.dat)) {
 full.dat[,i]<- as.numeric(full.dat[,i])
}
full.dat<-na.omit(full.dat)

## Check Correlations for Multicollinearity
cor.matrix<-cor(full.dat[,-c(1:5)])

# To Remove due to high MC:
 #win_prob_home, win_prob_away
 #ofei,dfei 
names(full.dat)

### Parse down potentially unimportant variables based on correlation with difference
# Select IVs (remove win_prob_home, win_prob_away, ofei and dfei from IV_idx_1)
IV_idx_2<-which(colnames(raw.dat.full) %in% c("Win_pct_home", "Win_pct_away",
                                        "AP_rank_home", "AP_rank_away",
                                        "elo_home", "elo_away", 
                                        "SRS_home", "SRS_away", 
                                        "fpi_home", "fpi_away",
                                        "sp_plus_home", "sp_plus_away",
                                        "returning_ppa_home", "returning_ppa_away",
                                        "pct_returning_ppa_home", "pct_returning_ppa_away",
                                        "postgame_wp_home", "postgame_wp_away",
                                        "Predictive_PwrRk_home", "Predictive_PwrRk_away",
                                        "Home_PwrRk_home", "Away_PwrRk_away", 
                                        "Last5_PwrRk_home","Last5_PwrRk_away", 
                                        "SOS_PwrRk_home","SOS_PwrRk_away",
                                        #"talent_home", "talent_away",
                                        "recruiting_score_home", "recruiting_score_away",
                                        "portal_score_home", "portal_score_away",
                                        "off_total_ppa_home", "off_total_ppa_away",
                                        "off_havoc_total_home","off_havoc_total_away",
                                        "off_explosiveness_home", "off_explosiveness_away",
                                        "def_total_ppa_home","def_total_ppa_away", 
                                        "def_havoc_total_home", "def_havoc_total_away",
                                        "def_explosiveness_home", "def_explosiveness_away",
                                        "turnovers_home", "turnovers_away",
                                        "third_down_convs_home", "third_down_convs_away",
                                        "penalty_yds_home", "penalty_yds_away",
                                        "FEI_home", "FEI_away",
                                        "spread_home"
))

mod.dat<-raw.dat.full[,c(1:5,IV_idx_2,76,78)] 

for (i in 6:ncol(mod.dat)) {
  mod.dat[,i]<- as.numeric(mod.dat[,i])
}
mod.dat<-na.omit(mod.dat)
names(mod.dat)

# Look at variables most correlated with difference
(cor.matrix.diff<-t(cor(mod.dat$difference, mod.dat[,-c(1:5)])))
cor.matrix.diff[order(cor.matrix.diff, decreasing=T),]

## Most correlated with Difference (>+/-0.25)
# Positive
    # elo_home, 
    # FEI_home,
    # postgame_wp_home,
    # AP_rank_home, 
    # Last5_PwrRk_away, 
    # Predictive_PwrRk_away, 
    # returning_ppa_home, 
# recruiting_score_home,
    # fpi_home, 
    # sp_plus_home, 
    # SRS_home, 
    # Win_pct_home
# Negative
    # returning_ppa_away, 
    # AP_rank_away,
    # SOS_PwrRk_home,
    # postgame_wp_away,
    # Home_PwrRk_home,
    # SRS_away, 
    # Win_pct_away,
    # FEI_away,
    # fpi_away, 
    # Last5_PwrRk_home, 
    # sp_plus_away, 
    # Predictive_PwrRk_home, 
    # elo_away, 
    # spread_home 


###### Variable Mutation ######
## Examine if taking difference between two teams for each variable is more predictive

#win percentage difference
mod.dat<-mod.dat%>%
  mutate(win_pct_diff = Win_pct_home-Win_pct_away)%>%
  select(-c(Win_pct_home, Win_pct_away)) #Creat Win_Pct_diff
cor(mod.dat$difference, mod.dat$win_pct_diff) #0.57

## Elo Difference
mod.dat$elo_diff<-mod.dat$elo_home-mod.dat$elo_away
cor(mod.dat$difference, mod.dat$elo_diff)#0.79
mod.dat$elo_home<-mod.dat$elo_away<-NULL

## SRS Difference
mod.dat$SRS_diff<-mod.dat$SRS_home-mod.dat$SRS_away
cor(mod.dat$difference, mod.dat$SRS_diff)#0.66
mod.dat$SRS_home<-mod.dat$SRS_away<-NULL

## FPI Difference
mod.dat$fpi_diff<-mod.dat$fpi_home-mod.dat$fpi_away
cor(mod.dat$difference, mod.dat$fpi_diff)#0.78
mod.dat$fpi_home<-mod.dat$fpi_away<-NULL

## FEI Difference
mod.dat$fei_diff<-mod.dat$FEI_home-mod.dat$FEI_away
cor(mod.dat$difference, mod.dat$fei_diff)#0.75
mod.dat$FEI_home<-mod.dat$FEI_away<-NULL

## SP+ Difference
mod.dat$sp_plus_diff<-mod.dat$sp_plus_home-mod.dat$sp_plus_away
cor(mod.dat$difference, mod.dat$sp_plus_diff)#0.75
mod.dat$sp_plus_home<-mod.dat$sp_plus_away<-NULL

## postgame WP Difference
mod.dat$postgame_wp_diff<-mod.dat$postgame_wp_home-mod.dat$postgame_wp_away
cor(mod.dat$difference, mod.dat$postgame_wp_diff)#0.50 (Better but not great)
mod.dat$postgame_wp_home<-mod.dat$postgame_wp_away<-NULL

## AP vs. Non-AP difference
#Create binary indicator variables for each possible matchup (Non AP vs Non AP Ranked is reference)
mod.dat$AP_both<-ifelse(mod.dat$AP_rank_home==1 & mod.dat$AP_rank_away==1,1,0)
mod.dat$AP_home<-ifelse(mod.dat$AP_rank_home==1 & mod.dat$AP_rank_away==0,1,0)
mod.dat$AP_away<-ifelse(mod.dat$AP_rank_home==0 & mod.dat$AP_rank_away==1,1,0)
mod.dat$AP_rank_home<-NULL
mod.dat$AP_rank_away<-NULL
cor(mod.dat$difference, mod.dat[,(ncol(mod.dat)-2):ncol(mod.dat)])
mod.dat$AP_both<-NULL

# Returning PPA Difference
mod.dat<-mod.dat%>%
  mutate(returning_ppa_diff = returning_ppa_home-returning_ppa_away)%>%
  select(-c(pct_returning_ppa_home, pct_returning_ppa_away))
cor(mod.dat$difference, mod.dat$returning_ppa_diff)# 0.37 #Consider
# mod.dat$returning_ppa_diff<-NULL


# Average power ranks together
for (i in 1:nrow(mod.dat)) {
  mod.dat$Pwr_Rk_home[i]<-mean(mod.dat$Predictive_PwrRk_home[i],mod.dat$Home_PwrRk_home[i], mod.dat$Last5_PwrRk_home[i], mod.dat$SOS_PwrRk_home[i])
  mod.dat$Pwr_Rk_away[i]<-mean(mod.dat$Predictive_PwrRk_away[i],mod.dat$Away_PwrRk_away[i], mod.dat$Last5_PwrRk_away[i], mod.dat$SOS_PwrRk_away[i])
  
}
cor(mod.dat$difference, mod.dat$Pwr_Rk_home)
cor(mod.dat$difference, mod.dat$Pwr_Rk_away)
cor(mod.dat$difference, mod.dat$Predictive_PwrRk_home)
cor(mod.dat$difference, mod.dat$Predictive_PwrRk_away)
cor(mod.dat$difference, mod.dat$Last5_PwrRk_home)
cor(mod.dat$difference, mod.dat$Last5_PwrRk_away)
cor(mod.dat$difference, mod.dat$SOS_PwrRk_home)
mod.dat$Pwr_Rk_diff<-mod.dat$Pwr_Rk_home-mod.dat$Pwr_Rk_away
cor(mod.dat$difference, mod.dat$Pwr_Rk_diff)#0.62

# Predictive PowerRank is all thats worth keeping
mod.dat<-mod.dat%>%
  select(-c(Home_PwrRk_home, SOS_PwrRk_home, Last5_PwrRk_home,Pwr_Rk_home,Predictive_PwrRk_home,
            Away_PwrRk_away, SOS_PwrRk_away, Last5_PwrRk_away, Pwr_Rk_away,Predictive_PwrRk_away))
(cor.matrix<-cor(mod.dat[,-c(1:5)]))

## Recruiting Difference
mod.dat$recruiting_diff<-mod.dat$recruiting_score_home-mod.dat$recruiting_score_away
cor(mod.dat$difference, mod.dat$recruiting_diff)#0.39 (Better but not great)
mod.dat$recruiting_diff<-NULL

## Look at total roster changes (portal, recruiting and returning production ratings)
# function to calculate z sore to put 3 roster variables on same scale
calculate_z_score <- function(x) {
  z_score <- (x - mean(x)) / sd(x)
  return(z_score)
}
mod.dat$portal_score_home<-calculate_z_score(mod.dat$portal_score_home)
mod.dat$portal_score_away<-calculate_z_score(mod.dat$portal_score_away)
mod.dat$recruiting_score_home<-calculate_z_score(mod.dat$recruiting_score_home)
mod.dat$recruiting_score_away<-calculate_z_score(mod.dat$recruiting_score_away)
mod.dat$returning_ppa_home<-calculate_z_score(mod.dat$returning_ppa_home)
mod.dat$recruiting_score_away<-calculate_z_score(mod.dat$returning_ppa_away)
#home
for (i in 1:nrow(mod.dat)) {
mod.dat$return_recruit_portal_home[i]<-mean(mod.dat$returning_ppa_home[i], mod.dat$recruiting_score_home[i], mod.dat$portal_score_home[i])
}
#away
for (i in 1:nrow(mod.dat)) {
  mod.dat$return_recruit_portal_away[i]<-mean(mod.dat$returning_ppa_away[i], mod.dat$recruiting_score_away[i], mod.dat$portal_score_away[i])
}
cor(mod.dat$difference,mod.dat$return_recruit_portal_home) #0.297
cor(mod.dat$difference,mod.dat$return_recruit_portal_away) #-0.232 not good
mod.dat$return_recruit_portal_diff<-mod.dat$return_recruit_portal_home-mod.dat$return_recruit_portal_away
cor(mod.dat$difference,mod.dat$return_recruit_portal_diff) #0.23 (no better)

mod.dat$return_recruit_portal_away<-mod.dat$return_recruit_portal_home<-mod.dat$recruiting_score_home<-mod.dat$recruiting_score_away<-mod.dat$portal_score_away<-mod.dat$portal_score_home<-NULL
mod.dat$return_recruit_portal_diff<-NULL
mod.dat$returning_ppa_home<-mod.dat$returning_ppa_away<-mod.dat$returning_ppa_diff<-NULL

# # Talent
# plot(mod.dat$difference, mod.dat$talent_home) #to remove
# plot(mod.dat$difference, mod.dat$talent_away) #to remove
# mod.dat$talent_home<-mod.dat$talent_away<-NULL

# # Making spread positive to correspond with backwards logic of spreads vs differences
# mod.dat$spread_home<-(-1*mod.dat$spread_home)
 
## Remove Advanced stats (none were very correlated)
names(mod.dat)
mod.dat<-mod.dat%>%
  select(-c(off_havoc_total_home, off_explosiveness_home,
            off_havoc_total_away, off_explosiveness_away,
            def_havoc_total_home, def_havoc_total_away,
            def_explosiveness_home, def_explosiveness_away,
            off_total_ppa_home, off_total_ppa_away,def_total_ppa_home, def_total_ppa_away,
            turnovers_home, turnovers_away,
            penalty_yds_home, penalty_yds_away,
            third_down_convs_home, third_down_convs_away
            ))

# # Predictive Power Rank Diff
# mod.dat$Predictive_PwrRk_diff<-mod.dat$Predictive_PwrRk_home-mod.dat$Predictive_PwrRk_away
# cor(mod.dat$difference, mod.dat$Predictive_PwrRk_diff) #0.62
# mod.dat$Predictive_PwrRk_home<-mod.dat$Predictive_PwrRk_away<-NULL

## Re-check correlations among remaining variables
names(mod.dat)
cor.matrix.diffs<-t(cor(mod.dat$difference, mod.dat[,-c(1:5)]))
cor.matrix.diffs[order(cor.matrix.diffs[,1], decreasing = T),]

## Order Variables for modelling
names(mod.dat)
mod.dat$home_win<-NULL #do not need Win or Loss, modeling difference

mod.dat<-mod.dat[,c(1:6,8,17,9:16,7)] 
names(mod.dat)
mod.dat<-na.omit(mod.dat)

###### Modelling ######
set.seed(109)
train<-sample(1:nrow(mod.dat), nrow(mod.dat)*.667, replace = F)
test<-which(!c(1:nrow(mod.dat)) %in% train)
str(mod.dat)

### MODEL 1: RAW DATA, ALL VARIABLES ###
model1.dat<-mod.dat[train,]
mod1.test.dat<-mod.dat[test,]
fit.1 <- lm(difference ~., data = model1.dat[,-c(1:5)])
summary(fit.1) #rsq=0.7441
# Predict
mod1.preds<-round(predict(fit.1, mod1.test.dat[, -c(1:5)] ), 1)
mod1.test.dat$predicted<-mod1.preds
# Total Absolute Difference
sum(abs(mod1.test.dat$predicted-mod1.test.dat$difference)) #1160.7
# Mean Absolute Difference
mean(abs(mod1.test.dat$predicted-mod1.test.dat$difference)) #8.47
plot(mod1.test.dat$predicted,mod1.test.dat$difference)
# Correct side
mod1.test.dat$side<-ifelse((mod1.test.dat$predicted<=0 & mod1.test.dat$difference<=0) | (mod1.test.dat$predicted>=0 & mod1.test.dat$difference>=0),1,0 )
sum(mod1.test.dat$side)/nrow(mod1.test.dat) #83.9
# Cover Spread
mod1.test.dat$spread<-(-1*mod1.test.dat$spread_home) #market predicted home-team point difference
mod1.test.dat$cover<-ifelse(mod1.test.dat$difference>=mod1.test.dat$spread,1,0) #if actual difference exceeded spread
mod1.test.dat$pred_cover<-ifelse(mod1.test.dat$predicted>=mod1.test.dat$spread,1,0) #if predicted difference exceeded spread
mod1.test.dat$pred_cover_correct<-ifelse(mod1.test.dat$cover==mod1.test.dat$pred_cover,1,0) #if prediction was correct
sum(mod1.test.dat$pred_cover_correct)/nrow(mod1.test.dat) #88.3%
xtabs(~pred_cover+cover, mod1.test.dat) # Most common error is Type I (False Negative),  predicting no cover when actually does cover
# 
# ###### MODEL 1v2: Only significant variables from Model 1 ######
# set.seed(109)
# train<-sample(1:nrow(mod.dat), nrow(mod.dat)*.667, replace = F)
# test<-which(!c(1:nrow(mod.dat)) %in% train)
# str(mod.dat)
# 
# v2model1.dat<-mod.dat[train,]
# v2mod1.test.dat<-mod.dat[test,]
# 
# v2fit.1 <- lm(difference ~ `spread_home`+`Pwr_Rk_diff`+`elo_diff`+`fpi_diff`+`sp_plus_diff`, data = v2model1.dat[,-c(1:5)])
# summary(v2fit.1) #rsq=0.744
# 
# # Predict
# v2mod1.preds<-round(predict(v2fit.1, v2mod1.test.dat[, -c(1:5)] ), 1)
# v2mod1.test.dat$predicted<-v2mod1.preds
# # Total Absolute Difference
# sum(abs(v2mod1.test.dat$predicted-v2mod1.test.dat$difference)) #1175.8
# # Mean Absolute Difference
# mean(abs(v2mod1.test.dat$predicted-v2mod1.test.dat$difference)) #8.58
# plot(v2mod1.test.dat$predicted,v2mod1.test.dat$difference)
# # Correct side
# v2mod1.test.dat$side<-ifelse((v2mod1.test.dat$predicted<0 & v2mod1.test.dat$difference<0) | (v2mod1.test.dat$predicted>0 & v2mod1.test.dat$difference>0),1,0 )
# sum(v2mod1.test.dat$side)/nrow(v2mod1.test.dat) #83.9
# # Cover Spread
# v2mod1.test.dat$spread<-(-1*v2mod1.test.dat$spread_home)
# v2mod1.test.dat$cover<-ifelse(v2mod1.test.dat$difference>=v2mod1.test.dat$spread,1,0)
# v2mod1.test.dat$pred_cover<-ifelse(v2mod1.test.dat$predicted>=v2mod1.test.dat$spread,1,0)
# v2mod1.test.dat$pred_cover_correct<-ifelse(v2mod1.test.dat$cover==v2mod1.test.dat$pred_cover,1,0)
# sum(v2mod1.test.dat$pred_cover_correct)/nrow(v2mod1.test.dat) #87.6
# xtabs(~pred_cover+cover, v2mod1.test.dat) # Most common error is predicting no cover when actually does cover
# 
# ###### MODEL 2: Best Subsets ######
# model2.dat<-mod.dat
# regfit.full.BSS <-regsubsets(difference~., model2.dat[,-c(1:5)], nvmax=15)
# (reg.summary.BSS<-summary(regfit.full.BSS))
# names(reg.summary.BSS)
# 
# which.min(reg.summary.BSS$cp) #Best-8
# (mod2.best.cp<-names(which(reg.summary.BSS$which[8,]==TRUE)))
# min(reg.summary.BSS$cp) #7.27
# plot(reg.summary.BSS$cp, xlab = "Number of Variables",
#      ylab = "Mallow's cp", type = "l")
# 
# which.min(reg.summary.BSS$bic) #Best-5
# min(reg.summary.BSS$bic) #-501.5
# (mod2.best.bic<-names(which(reg.summary.BSS$which[5,]==TRUE)))
# plot(reg.summary.BSS$bic, xlab = "Number of Variables",
#      ylab = "Bayesian Information Criterion (BIC)", type = "l")
# 
# which.max(reg.summary.BSS$adjr2) #Best-8
# (mod2.best.adjr2<-names(which(reg.summary.BSS$which[8,]==TRUE)))
# max(reg.summary.BSS$adjr2) #0.731
# plot(reg.summary.BSS$adjr2, xlab = "Number of Variables",
#      ylab = "Adjusted RSq", type = "l")
# 
# ### Examine what variables are best ###
# mod2.best.cp
# mod2.best.bic
# mod2.best.adjr2
# all(mod2.best.cp == mod2.best.adjr2) #same variables
# 
# # Next model is all variables in all or in 2
# new_var_list_1<-c(mod2.best.cp[-1])  #using common variables (-1 to account for intercept)
#  
# ###### Model 3: Model on new full Variable list ######
# model3.dat<-mod.dat[train,]%>%
#   select(game, year, week, school_home, school_away,all_of(new_var_list_1), difference)
# mod3.test.dat<-model3.dat[test,]%>%
#   select(game, year, week, school_home, school_away,all_of(new_var_list_1), difference)
# 
# fit.3 <- lm(difference ~., data = model3.dat[,-c(1:5)])
# summary(fit.3) #rsq = 0.746
# mod3.preds<-round(predict(fit.3, mod3.test.dat[, -c(1:5)] ), 1)
# mod3.test.dat$predicted<-mod3.preds
# # Total Absolute Difference
# sum(abs(mod3.test.dat$predicted-mod3.test.dat$difference)) #1154.3
# # Mean Absolute Difference
# mean(abs(mod3.test.dat$predicted-mod3.test.dat$difference)) #8.50
# plot(mod3.test.dat$predicted,mod3.test.dat$difference)
# # Correct side
# mod3.test.dat$side<-ifelse((mod3.test.dat$predicted<0 & mod3.test.dat$difference<0) | (mod3.test.dat$predicted>0 & mod3.test.dat$difference>0),1,0 )
# sum(mod3.test.dat$side)/nrow(mod3.test.dat) #83.2%
# # Cover Spread
# mod3.test.dat$spread<-(-1*mod3.test.dat$spread_home)
# mod3.test.dat$cover<-ifelse(mod3.test.dat$difference>=mod3.test.dat$spread,1,0)
# mod3.test.dat$pred_cover<-ifelse(mod3.test.dat$predicted>=mod3.test.dat$spread,1,0)
# mod3.test.dat$pred_cover_correct<-ifelse(mod3.test.dat$cover==mod3.test.dat$pred_cover,1,0)
# sum(mod3.test.dat$pred_cover_correct)/nrow(mod3.test.dat) #73.0%
# xtabs(~pred_cover+cover, mod3.test.dat) # Most common error is predicting no cover when actually does cover
# 
# ###### Model 4: Model on most significant variables - most reduced ######
# new_var_list_2<-c("spread_home","Predictive_PwrRk_diff",
#                   "elo_diff","fpi_diff", "sp_plus_diff"
# ) 
# model4.dat<-mod.dat[train,]%>%
#   select(game, year, week, school_home, school_away,all_of(new_var_list_2), difference)
# mod4.test.dat<-mod.dat[test,]%>%
#   select(game, year, week, school_home, school_away,all_of(new_var_list_2), difference)
# 
# fit.4 <- lm(difference ~., data = model4.dat[,-c(1:5)])
# summary(fit.4) #rsq = 0.745
# mod4.preds<-round(predict(fit.4, mod4.test.dat[, -c(1:5)] ), 1)
# mod4.test.dat$predicted<-mod4.preds
# # Total Absolute Difference
# sum(abs(mod4.test.dat$predicted-mod4.test.dat$difference)) #1175.8
# # Mean Absolute Difference
# mean(abs(mod4.test.dat$predicted-mod4.test.dat$difference)) #8.58
# plot(mod4.test.dat$predicted,mod4.test.dat$difference)
# 
# # Correct side
# mod4.test.dat$side<-ifelse((mod4.test.dat$predicted<0 & mod4.test.dat$difference<0) | (mod4.test.dat$predicted>0 & mod4.test.dat$difference>0),1,0 )
# sum(mod4.test.dat$side)/nrow(mod4.test.dat) #83.9
# # Cover Spread
# mod4.test.dat$spread<-(-1*mod4.test.dat$spread_home)
# mod4.test.dat$cover<-ifelse(mod4.test.dat$difference>=mod4.test.dat$spread,1,0)
# mod4.test.dat$pred_cover<-ifelse(mod4.test.dat$predicted>=mod4.test.dat$spread,1,0)
# mod4.test.dat$pred_cover_correct<-ifelse(mod4.test.dat$cover==mod4.test.dat$pred_cover,1,0)
# sum(mod4.test.dat$pred_cover_correct)/nrow(mod4.test.dat) #74.5
# xtabs(~pred_cover+cover, mod3.test.dat) # Most common error is predicting no cover when actually does cover

#### Model 1 is Best!!!!! #####
##Re-run model 1
set.seed(109)
train<-sample(1:nrow(mod.dat), nrow(mod.dat)*.667, replace = F)
test<-which(!c(1:nrow(mod.dat)) %in% train)

str(mod.dat)

model1.dat<-mod.dat[train,]
mod1.test.dat<-mod.dat[test,]

fit.1 <- lm(difference ~., data = model1.dat[,-c(1:5)])
summary(fit.1) #rsq=0.744
# Predict
mod1.preds<-round(predict(fit.1, mod1.test.dat[, -c(1:5)] ), 1)
mod1.test.dat$predicted<-mod1.preds
# Total Absolute Difference
sum(abs(mod1.test.dat$predicted-mod1.test.dat$difference)) #1160
# Mean Absolute Difference
mean(abs(mod1.test.dat$predicted-mod1.test.dat$difference)) #8.47
plot(mod1.test.dat$predicted,mod1.test.dat$difference)
# Correct side
mod1.test.dat$side<-ifelse((mod1.test.dat$predicted<0 & mod1.test.dat$difference<0) | (mod1.test.dat$predicted>0 & mod1.test.dat$difference>0),1,0 )
sum(mod1.test.dat$side)/nrow(mod1.test.dat) #83.9
# Cover Spread
mod1.test.dat$spread<-(-1*mod1.test.dat$spread_home)
mod1.test.dat$cover<-ifelse(mod1.test.dat$difference>=mod1.test.dat$spread,1,0)
mod1.test.dat$pred_cover<-ifelse(mod1.test.dat$predicted>=mod1.test.dat$spread,1,0)
mod1.test.dat$pred_cover_correct<-ifelse(mod1.test.dat$cover==mod1.test.dat$pred_cover,1,0)
sum(mod1.test.dat$pred_cover_correct)/nrow(mod1.test.dat) #73.0%
xtabs(~pred_cover+cover, mod1.test.dat) # Most common error is predicting no cover when actually does cover

### Specify Final Model ###
final.model<-fit.1
