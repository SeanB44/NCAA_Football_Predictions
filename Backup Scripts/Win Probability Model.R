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
(cor.matrix<-cor(full.dat[,-c(1:5)]))

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
colnames(raw.dat.full)
mod.dat<-raw.dat.full[,c(1:5,IV_idx_2,76,78)] 

for (i in 6:ncol(mod.dat)) {
  mod.dat[,i]<- as.numeric(mod.dat[,i])
}
mod.dat<-na.omit(mod.dat)
names(mod.dat)

# Look at variables most correlated with difference
(cor.matrix.diff<-t(cor(mod.dat$home_win, mod.dat[,-c(1:5)])))
cor.matrix.diff[order(cor.matrix.diff, decreasing=T),]

## Most correlated with Difference (>+/-0.25)
# Positive
      # elo_home,*
      # Win_pct_home*
      # fpi_home*
      # fei_home*
      # SRS_home,* 
      # sp_plus_home,* 
    # postgame_wp_home,* 
    # AP_rank_home, 

# Negative
    # spread_home,*
    # Predictive_PwrRk_home,
      # Win_pct_away,*
      # elo_away,*
    # Last5_PwrRk_home ,
      # SRS_away,* 
    # Home_PwrRk_home,
      # sp_plus_away,*
      # fpi_away,*
      # fei_away,* 
    # postgame_wp_away,*
#ADD: Aggregat Power ranks
   

###### Variable Mutation ######
## Examine if taking difference between two teams for each variable is more predictive

#win percentage difference
mod.dat<-mod.dat%>%
  mutate(win_pct_diff = Win_pct_home-Win_pct_away)%>%
  select(-c(Win_pct_home, Win_pct_away)) #Creat Win_Pct_diff
cor(mod.dat$home_win, mod.dat$win_pct_diff) #0.55

## Elo Difference
mod.dat$elo_diff<-mod.dat$elo_home-mod.dat$elo_away
cor(mod.dat$home_win, mod.dat$elo_diff)#0.62
mod.dat$elo_home<-mod.dat$elo_away<-NULL

## SRS Difference
mod.dat$SRS_diff<-mod.dat$SRS_home-mod.dat$SRS_away
cor(mod.dat$home_win, mod.dat$SRS_diff)#0.58
mod.dat$SRS_home<-mod.dat$SRS_away<-NULL

## FPI Difference
mod.dat$fpi_diff<-mod.dat$fpi_home-mod.dat$fpi_away
cor(mod.dat$home_win, mod.dat$fpi_diff)#0.62
mod.dat$fpi_home<-mod.dat$fpi_away<-NULL

## FEI Difference
mod.dat$fei_diff<-mod.dat$FEI_home-mod.dat$FEI_away
cor(mod.dat$home_win, mod.dat$fei_diff)#0.61
mod.dat$FEI_home<-mod.dat$FEI_away<-NULL

## SP+ Difference
mod.dat$sp_plus_diff<-mod.dat$sp_plus_home-mod.dat$sp_plus_away
cor(mod.dat$home_win, mod.dat$sp_plus_diff)#0.60
mod.dat$sp_plus_home<-mod.dat$sp_plus_away<-NULL

## postgame WP Difference
mod.dat$postgame_wp_diff<-mod.dat$postgame_wp_home-mod.dat$postgame_wp_away
cor(mod.dat$home_win, mod.dat$postgame_wp_diff)#0.44 (Better but not great)
mod.dat$postgame_wp_home<-mod.dat$postgame_wp_away<-NULL

## AP vs. Non-AP difference
#Create binary indicator variables for each possible matchup (Non AP vs Non AP Ranked is reference)
mod.dat$AP_both<-ifelse(mod.dat$AP_rank_home==1 & mod.dat$AP_rank_away==1,1,0)
mod.dat$AP_home<-ifelse(mod.dat$AP_rank_home==1 & mod.dat$AP_rank_away==0,1,0)
mod.dat$AP_away<-ifelse(mod.dat$AP_rank_home==0 & mod.dat$AP_rank_away==1,1,0)
mod.dat$AP_rank_home<-NULL
mod.dat$AP_rank_away<-NULL
cor(mod.dat$home_win, mod.dat[,(ncol(mod.dat)-2):ncol(mod.dat)])
mod.dat$AP_both<-AP_home<-AP_away<-NULL

# Returning PPA Difference
mod.dat<-mod.dat%>%
  mutate(returning_ppa_diff = returning_ppa_home-returning_ppa_away)%>%
  select(-c(pct_returning_ppa_home, pct_returning_ppa_away))
cor(mod.dat$home_win, mod.dat$returning_ppa_diff)# 0.28 
mod.dat$returning_ppa_diff<-NULL


# Average power ranks together
for (i in 1:nrow(mod.dat)) {
  mod.dat$Pwr_Rk_home[i]<-mean(mod.dat$Predictive_PwrRk_home[i],mod.dat$Home_PwrRk_home[i], mod.dat$Last5_PwrRk_home[i], mod.dat$SOS_PwrRk_home[i])
  mod.dat$Pwr_Rk_away[i]<-mean(mod.dat$Predictive_PwrRk_away[i],mod.dat$Away_PwrRk_away[i], mod.dat$Last5_PwrRk_away[i], mod.dat$SOS_PwrRk_away[i])
  
}
cor(mod.dat$home_win, mod.dat$Pwr_Rk_home)
cor(mod.dat$home_win, mod.dat$Pwr_Rk_away)
cor(mod.dat$home_win, mod.dat$Predictive_PwrRk_home)
cor(mod.dat$home_win, mod.dat$Predictive_PwrRk_away)
cor(mod.dat$home_win, mod.dat$Last5_PwrRk_home)
cor(mod.dat$home_win, mod.dat$Last5_PwrRk_away)
cor(mod.dat$home_win, mod.dat$SOS_PwrRk_home)

# Predictive PowerRank is all thats worth keeping
mod.dat$Pwr_rk_diff<-mod.dat$Pwr_Rk_home-mod.dat$Pwr_Rk_away
cor(mod.dat$home_win, mod.dat$Pwr_rk_diff)#0.49
mod.dat$Pwr_Rk_home<-mod.dat$Pwr_Rk_away<-NULL

mod.dat<-mod.dat%>%
  select(-c(Home_PwrRk_home, SOS_PwrRk_home, Last5_PwrRk_home,
            Away_PwrRk_away, SOS_PwrRk_away, Last5_PwrRk_away))
(cor.matrix<-cor(mod.dat[,-c(1:5)]))


## Look at total roster changes (portal, recruiting and returning production ratings)
# function to calculate z sore to put 3 roster variables on same scale
calculate_z_score <- function(x) {
  z_score <- (x - mean(x)) / sd(x)
  return(z_score)
}

## Re-check correlations among remaining variables
names(mod.dat)
mod.dat$difference<-NULL
cor.matrix.diffs<-t(cor(mod.dat$home_win, mod.dat[,-c(1:5)]))
cor.matrix.diffs[order(cor.matrix.diffs[,1], decreasing = T),]

mod.dat<-mod.dat%>%
  select(-c(returning_ppa_home,recruiting_score_home, Predictive_PwrRk_away,
            off_total_ppa_home, turnovers_away, def_havoc_total_home, off_explosiveness_home,
            portal_score_away,def_total_ppa_away,off_havoc_total_away,def_havoc_total_away,
            portal_score_away,def_total_ppa_away,off_havoc_total_away, def_havoc_total_away,
            penalty_yds_away, portal_score_home, third_down_convs_home, def_explosiveness_away,
            def_explosiveness_home, off_havoc_total_home, off_total_ppa_away,def_total_ppa_home,
            turnovers_home, returning_ppa_away,Predictive_PwrRk_home,  penalty_yds_home,      
            recruiting_score_away, off_explosiveness_away, third_down_convs_away
            ))
## Order Variables for modelling
names(mod.dat)

mod.dat<-mod.dat[,c(1:6,8:17, 7)] 
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
fit.1<-glm(`home_win`~., data =model1.dat[,-c(1:5)], family = 'binomial' )
summary(fit.1)
mod1.test.dat$predictions<-predict(fit.1, newdata = mod1.test.dat, type = 'response')
mod1.test.dat$predictions<-ifelse(mod1.test.dat$predictions>0.5, 1, 0)
table(Predicted = mod1.test.dat$predictions, Actual = mod1.test.dat$home_win )
confusionMatrix(as.factor(mod1.test.dat$predictions),as.factor(mod1.test.dat$home_win)) #83.3% Accuraccy
prop.table(table(Prediction = mod1.test.dat$predictions, Actual = mod1.test.dat$home_win))%>%round(2)
#Inaccuracies: 11% false fositive, 6% false negative

summary(fit.1)

# ### MODEL 2: Best Subsets ####
# model2.dat<-mod.dat
# regfit.full.BSS <-regsubsets(home_win~., model2.dat[,-c(1:5)], nvmax=15)
# (reg.summary.BSS<-summary(regfit.full.BSS))
# names(reg.summary.BSS)
# 
# which.min(reg.summary.BSS$cp) #Best-7
# (mod2.best.cp<-names(which(reg.summary.BSS$which[7,]==TRUE)))
# min(reg.summary.BSS$cp) #7.1
# plot(reg.summary.BSS$cp, xlab = "Number of Variables",
#      ylab = "Mallow's cp", type = "l")
# 
# which.min(reg.summary.BSS$bic) #Best-5
# min(reg.summary.BSS$bic) #-237.1
# (mod2.best.bic<-names(which(reg.summary.BSS$which[5,]==TRUE)))
# plot(reg.summary.BSS$bic, xlab = "Number of Variables",
#      ylab = "Bayesian Information Criterion (BIC)", type = "l")
# 
# which.max(reg.summary.BSS$adjr2) #Best-9
# (mod2.best.adjr2<-names(which(reg.summary.BSS$which[9,]==TRUE)))
# max(reg.summary.BSS$adjr2) #0.48
# plot(reg.summary.BSS$adjr2, xlab = "Number of Variables",
#      ylab = "Adjusted RSq", type = "l")
# 
# ### Examine what variables are best ###
# mod2.best.cp
# mod2.best.bic
# mod2.best.adjr2
# # Look at Best-6 (based on plots this makes most sense)
# new_var_list<-names(which(reg.summary.BSS$which[6,]==TRUE))[-1] #-1 to account for intercept
# 
# ## Run Regression model
# model2.dat<-mod.dat[train,]%>%
#   select(game, year, week, school_home, school_away,all_of(new_var_list), home_win)
# mod2.test.dat<-mod.dat[test,]%>%
#   select(game, year, week, school_home, school_away,all_of(new_var_list), home_win)
# 
# fit.2 <- lm(home_win ~., data = model2.dat[,-c(1:5)])
# summary(fit.2) #rsq = 0.499
# mod2.test.dat$predictions<-predict(fit.2, newdata = mod2.test.dat, type = 'response')
# mod2.test.dat$predictions<-ifelse(mod2.test.dat$predictions>0.5, 1, 0)
# table(Predicted = mod2.test.dat$predictions, Actual = mod2.test.dat$home_win )
# confusionMatrix(as.factor(mod2.test.dat$predictions),as.factor(mod2.test.dat$home_win)) #82.6% Accuraccy
# prop.table(table(Prediction = mod2.test.dat$predictions, Actual = mod2.test.dat$home_win))%>%round(2)
# #Inaccuracies: 12% false positive, 6% false negative
# 
# # Total Absolute Difference
# sum(abs(mod2.test.dat$predictions-mod2.test.dat$home_win)) #24
# # Mean Absolute Difference
# mean(abs(mod2.test.dat$predictions-mod2.test.dat$home_win)) #.174
# 
# ### MODEL 3: Significant predictors from first model ###
# model3.dat<-mod.dat[train,]
# mod3.test.dat<-mod.dat[test,]
# summary(fit.1)
# fit.3<-glm(`home_win`~spread_home+win_pct_diff+elo_diff+postgame_wp_diff+Pwr_rk_diff, data =model1.dat[,-c(1:5)], family = 'binomial' )
# summary(fit.3)
# mod3.test.dat$predictions<-predict(fit.3, newdata = mod3.test.dat, type = 'response')
# mod3.test.dat$predictions<-ifelse(mod3.test.dat$predictions>0.5, 1, 0)
# table(Predicted = mod3.test.dat$predictions, Actual = mod3.test.dat$home_win )
# confusionMatrix(as.factor(mod3.test.dat$predictions),as.factor(mod3.test.dat$home_win)) #75% Accuraccy
# prop.table(table(Prediction = mod3.test.dat$predictions, Actual = mod3.test.dat$home_win))%>%round(2)
# #Inaccuracies: 17% false fositive, 6% false negative

summary(fit.1) ### Winner!
# Save as final model
prob.model<-fit.1
