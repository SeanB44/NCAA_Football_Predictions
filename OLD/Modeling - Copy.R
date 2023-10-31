# library(httr)
# library(caTools)
# library(neuralnet)
library(dplyr)
library(ggplot2)
# library(expss)
library(e1071)
library(caret)
library(XML)
library(readxl)
library(tidyr)
library(tidyverse)
library(openxlsx)
# # pacman::p_load_current_gh("sportsdataverse/cfbfastR", dependencies = TRUE, update = TRUE)
library(cfbfastR)
# library(zoo)
# library(ggimage)
# library(gt)
library(RCurl)
# library(rvest)
# #install.packages("ISLR2")
library (ISLR2)
# #install.packages("leaps")
library(leaps)
#########################
usethis::edit_r_environ()
#Sys.setenv(CFBD_API_KEY = "OJOCuvsvPW4SIhLZPbr3/Pw8vaoASLKFX5yKTD28+Su7YvPZAPfHQl55pB3CADjm")
##########################
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
      read.xlsx("./Output/Modeling/Week_12_year_2021_Data.xlsx")
      # read.xlsx("./Output/Modeling/Week_11_year_2021_Data.xlsx")
      )
### Initiate team info fo all teams ###
team_info.dat<-cfbd_team_info(year = 2023)%>%
  select(team_id, school, abbreviation, conference)

## Data Cleaning ##
names(raw.dat.full)
raw.dat.full$home_cover<-NULL
raw.dat.full<-raw.dat.full[,c(1:3,5,43,8:41,46:ncol(raw.dat.full))]

# Filter out teams not in FBS
raw.dat.full$school_home[!raw.dat.full$school_home %in%team_info.dat$school]
raw.dat.full$school_away[!raw.dat.full$school_away %in%team_info.dat$school]
raw.dat.full<-raw.dat.full[-which(!raw.dat.full$school_home %in% team_info.dat$school),]
raw.dat.full<-raw.dat.full[-which(!raw.dat.full$school_away %in%team_info.dat$school),]

## Check NA's
colSums(is.na(raw.dat.full))

# Save down initial cleaned data
full.dat<-raw.dat.full

############## Data Manipulations #################
#############

## Impute NA's? - Possible in future (Talent, off_havoc, def_havoc, off_explosiveness, def_explosiveness, off_total_ppa, def_total_ppa)

#############

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
              "talent_home", "talent_away",
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
              "spread_home"#, "spread_away"#,#Only one spread needed
              #"OFEI_home", "OFEI_away",
              #"DFEI_home","DFEI_away"
))

full.dat<-full.dat[,c(1:5,IV_idx,76,78)] 
for (i in 6:ncol(full.dat)) {
 full.dat[,i]<- as.numeric(full.dat[,i])
}
full.dat<-na.omit(full.dat)

## Check Correlations for Multicollinearity
cor.matrix<-cor(full.dat[,-c(1:5)])

# To Remove due to high MC:
 #win_prob_home, win_prob_away (re-run and removed above)
 #ofei and dfei (re-run and removed above)
names(full.dat)


### Parse down potentially unimportant variables based on correlation with difference (and to a lesser extent home_win)
## Select IVs
IV_idx_2<-which(colnames(raw.dat.full) %in% c("Win_pct_home", "Win_pct_away",
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
                                        "spread_home"
                                        #"OFEI_home", "OFEI_away",
                                        #"DFEI_home","DFEI_away"
))

mod.dat<-raw.dat.full[,c(1:5,IV_idx_2,76,78)] 

for (i in 6:ncol(mod.dat)) {
  mod.dat[,i]<- as.numeric(mod.dat[,i])
}
mod.dat<-na.omit(mod.dat)

names(mod.dat)
(cor.matrix.win<-t(cor(mod.dat$home_win, mod.dat[,-c(1:5)])))
cor.matrix.win[order(cor.matrix.win, decreasing=T),]
## Most correlated with Wining (>+/-0.25)
# Positive
  #elo_home, FEI_home, fpi_home, sp_plus_home, SRS_home, win_pct_home, postgame_wp_home
# Negative
  #spread_home, elo_away, Predictive_PwrRk_home, Win_pct_away, Last5_PwrRk_home,sp_plus_away,SRS_away,
  #Home_PwrRk_home, fpi_away, FEI_away, postgame_wp_away
### Data Manipulations and plotting relationships ###

# ## Create Indicator Variables for Interaction between AP Ranked Teams
# mod.dat$AP_at_AP<-as.factor(ifelse(mod.dat$AP_rank_home==1 & mod.dat$AP_rank_away==1, 1, 0))
# mod.dat$AP_at_non<-as.factor(ifelse(mod.dat$AP_rank_home==0 & mod.dat$AP_rank_away==1, 1, 0))
# mod.dat$non_at_AP<-as.factor(ifelse(mod.dat$AP_rank_home==1 & mod.dat$AP_rank_away==0, 1, 0))
# mod.dat$non_at_non<-as.factor(ifelse(mod.dat$AP_rank_home==0 & mod.dat$AP_rank_away==0, 1, 0))
# mod.dat$AP_rank_home<-NULL
# mod.dat$AP_rank_away<-NULL

## Data Investigation & Variable Selection ##
#Take win percentage difference
mod.dat<-mod.dat%>%
  mutate(win_pct_diff = Win_pct_home-Win_pct_away)%>%
  select(-c(Win_pct_home, Win_pct_away)) #Creat Win_Pct_diff
cor(mod.dat$home_win, mod.dat$win_pct_diff) #0.53

# Consider exponentiating wp for more linear relationship
mod.dat$Win_pct_diff_exp<-exp(mod.dat$win_pct_diff)
cor(mod.dat$home_win,mod.dat$Win_pct_diff_exp)#No use
mod.dat$Win_pct_diff_exp<-NULL

## Elo Difference
mod.dat$elo_diff<-mod.dat$elo_home-mod.dat$elo_away
cor(mod.dat$home_win, mod.dat$elo_diff)#0.62
mod.dat$elo_home<-mod.dat$elo_away<-NULL

## SRS Difference
mod.dat$SRS_diff<-mod.dat$SRS_home-mod.dat$SRS_away
cor(mod.dat$home_win, mod.dat$SRS_diff)#0.57
mod.dat$SRS_home<-mod.dat$SRS_away<-NULL

## FPI Difference
mod.dat$fpi_diff<-mod.dat$fpi_home-mod.dat$fpi_away
cor(mod.dat$home_win, mod.dat$fpi_diff)#0.62
mod.dat$fpi_home<-mod.dat$fpi_away<-NULL

## FEI Difference
mod.dat$fei_diff<-mod.dat$FEI_home-mod.dat$FEI_away
cor(mod.dat$home_win, mod.dat$fei_diff)#0.63
mod.dat$FEI_home<-mod.dat$FEI_away<-NULL

## SP+ Difference
mod.dat$sp_plus_diff<-mod.dat$sp_plus_home-mod.dat$sp_plus_away
cor(mod.dat$home_win, mod.dat$sp_plus_diff)#0.62
mod.dat$sp_plus_home<-mod.dat$sp_plus_away<-NULL

## postgame WP Difference
mod.dat$postgame_wp_diff<-mod.dat$postgame_wp_home-mod.dat$postgame_wp_away
cor(mod.dat$home_win, mod.dat$postgame_wp_diff)#0.442 (Better but not great)
mod.dat$postgame_wp_home<-mod.dat$postgame_wp_away<-NULL

## Offensive PPA Difference (this was not highly correlated but looking at diiference)
mod.dat$off_total_ppa_diff<-mod.dat$off_total_ppa_home-mod.dat$off_total_ppa_away
cor(mod.dat$home_win, mod.dat$off_total_ppa_diff)#Nope
# mod.dat$off_total_ppa_home<-mod.dat$off_total_ppa_away<-NULL
# mod.dat$def_total_ppa_home<-mod.dat$def_total_ppa_away<-NULL
# mod.dat$total_ppa_diff<-NULL

## Offensive vs. Defensive PPA from opposing teams
mod.dat$ppa_interaction<-(mod.dat$off_total_ppa_home*mod.dat$def_total_ppa_away)
cor(mod.dat$home_win, mod.dat$ppa_interaction)#Nope
mod.dat$off_total_ppa_home<-mod.dat$off_total_ppa_away<-NULL
mod.dat$def_total_ppa_home<-mod.dat$def_total_ppa_away<-NULL
mod.dat$off_total_ppa_diff<-NULL
mod.dat$total_ppa_diff<-mod.dat$ppa_interaction<-NULL



## AP vs. Non-AP difference
mod.dat$AP_both<-ifelse(mod.dat$AP_rank_home==1 & mod.dat$AP_rank_away==1,1,0)
mod.dat$AP_home<-ifelse(mod.dat$AP_rank_home==1 & mod.dat$AP_rank_away==0,1,0)
mod.dat$AP_away<-ifelse(mod.dat$AP_rank_home==0 & mod.dat$AP_rank_away==1,1,0)
mod.dat$AP_none<-ifelse(mod.dat$AP_rank_home==0 & mod.dat$AP_rank_away==0,1,0)
mod.dat$AP_rank_home<-NULL
mod.dat$AP_rank_away<-NULL
cor(mod.dat$home_win, mod.dat[,(ncol(mod.dat)-3):ncol(mod.dat)])#Nope
mod.dat$AP_both<-NULL
mod.dat$AP_none<-NULL
# Create returning ppa home_win to see if that helps in linearity
mod.dat<-mod.dat%>%
  mutate(returning_ppa_diff = returning_ppa_home-returning_ppa_away)%>%
  select(-c(returning_ppa_home, returning_ppa_away, pct_returning_ppa_home, pct_returning_ppa_away))
cor(mod.dat$home_win, mod.dat$returning_ppa_diff)
# no
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
#Average is better ut not fantastic, keep for now
mod.dat<-mod.dat%>%
  select(-c(Predictive_PwrRk_home, Home_PwrRk_home, SOS_PwrRk_home, Last5_PwrRk_home,
            Predictive_PwrRk_away, Away_PwrRk_away, SOS_PwrRk_away, Last5_PwrRk_away))
(cor.matrix<-cor(mod.dat[,-c(1:5)]))


calculate_z_score <- function(x) {
  z_score <- (x - mean(x)) / sd(x)
  return(z_score)
}
mod.dat$portal_score_home<-calculate_z_score(mod.dat$portal_score_home)
mod.dat$portal_score_away<-calculate_z_score(mod.dat$portal_score_away)
mod.dat$recruiting_score_home<-calculate_z_score(mod.dat$recruiting_score_home)
mod.dat$recruiting_score_away<-calculate_z_score(mod.dat$recruiting_score_away)
for (i in 1:nrow(mod.dat)) {
mod.dat$recruit_and_portal_home[i]<-mean(mod.dat$recruiting_score_home[i], mod.dat$portal_score_home[i])
}
for (i in 1:nrow(mod.dat)) {
  mod.dat$recruit_and_portal_away[i]<-mean(mod.dat$recruiting_score_away[i], mod.dat$portal_score_away[i])
}
cor(mod.dat$home_win,mod.dat$recruit_and_portal_home)
cor(mod.dat$home_win,mod.dat$recruit_and_portal_away) # not good
mod.dat$recruit_and_portal_away<-mod.dat$recruit_and_portal_home<-mod.dat$recruiting_score_home<-mod.dat$recruiting_score_away<-mod.dat$portal_score_away<-mod.dat$portal_score_home<-NULL


# plot(mod.dat$home_win, mod.dat$talent_home) #to remove
# plot(mod.dat$home_win, mod.dat$talent_away) #to remove
## Removed talent (performed above and re-run) ##


# # Making spread positive to correspond with backwards logic of spreads vs home_wins
# mod.dat$spread_home<-(-1*mod.dat$spread_home)
 
## Remove non-PPA advanced stats
names(mod.dat)
mod.dat<-mod.dat%>%
  select(-c(off_havoc_total_home, off_explosiveness_home,
            off_havoc_total_away, off_explosiveness_away,
            def_havoc_total_home, def_havoc_total_away,
            def_explosiveness_home, def_explosiveness_away,
            turnovers_home, turnovers_away,
            penalty_yds_home, penalty_yds_away,
            third_down_convs_home, third_down_convs_away))

#Re-check correlations
names(mod.dat)
cor.matrix.win<-t(cor(mod.dat$home_win, mod.dat[,-c(1:5)]))
cor.matrix.win[order(cor.matrix.win[,1]),]

# Order Variables for modelling
names(mod.dat)
mod.dat$difference<-NULL #do not need difference, only binary W/L

mod.dat<-mod.dat[,c(1:6,8:18,7)] #Grabbing just win_pct_diff (not _exp)
names(mod.dat)
mod.dat<-na.omit(mod.dat)

###### MODEL 1: SVM ######
set.seed(109)
train<-sample(1:nrow(mod.dat), nrow(mod.dat)*.667, replace = F)
test<-which(!c(1:nrow(mod.dat)) %in% train)

str(mod.dat)
# Encode variables as factor where necessary
mod.dat$home_win<-as.factor(mod.dat$home_win)
mod.dat$AP_home<-as.factor(mod.dat$AP_home)
mod.dat$AP_away<-as.factor(mod.dat$AP_away)

model1.dat<-mod.dat[train,]

## Fit Support Vector Classifier
svmfit<-svm(home_win~.,data=model1.dat[,-c(1:5)], kernel='linear',
            cost=10, scale=FALSE) #not scaling each feature, but may want to based on scenario
plot(svmfit, model1.dat) #Support Vectors plotted as x's, remaining obs as o
svmfit$index
summary(svmfit)
# Look at prediction
mod1.test.dat<-mod.dat[test,]
ypred<-predict(svmfit,mod1.test.dat)
table(predict=ypred,truth=mod1.test.dat$home_win) #3 
(42+55)/nrow(mod1.test.dat) #80.8% correct

## Model 2: Cross-Validation
# tune() function performs 10-fold cross-validation
set.seed(8)
tune.out<-tune(svm,home_win~.,data=model1.dat[,-c(1:5)], kernel="linear",scale = F,
               ranges = list(cost=c(0.001, #Use a range of cost parameter values
                                    0.01,
                                    0.1,
                                    0.5,
                                    0.75,
                                    1,
                                    1.5,
                                    2,
                                    5,
                                    10,
                                    100)))
summary(tune.out)
# cost=1.5 yeilds lowest CV error rate
# tune() function stores the best model obtained
bestmodel<-tune.out$best.model
summary(bestmodel)
# Predict using best model
ypred<-predict(bestmodel,mod1.test.dat)
table(predict=ypred,truth=mod1.test.dat$home_win) #3 
(40+59)/nrow(mod1.test.dat) #82.5% correct

svm.best<-svm(home_win~.,data=model1.dat[,-c(1:5)], kernel='linear',
    cost=0.01, scale=F) #not scaling each feature, but may want to based on scenario
summary(svm.best)
# Predict using best model
ypred<-predict(svm.best,mod1.test.dat)
table(predict=ypred,truth=mod1.test.dat$home_win) #3 
(40+59)/nrow(mod1.test.dat) #81.7% correct

plot(svm.best, data=model1.dat)

# Extract Coefficients
svm.best.coefs<-data.frame(coefficients = (t(svm.best$SV) %*% svm.best$coefs))
svm.best.coef<-rbind(intercept=svm.best$rho,svm.best.coefs)

write.csv(svm.best.coef, "./coefs.csv")
write.csv(model1.dat, "./mod1traindat.csv")
model1.dat$AP_home<-as.numeric(model1.dat$AP_home)
model1.dat$AP_away<-as.numeric(model1.dat$AP_away)
beta<-drop(t(svm.best$coefs)%*%model1.dat[svm.best$index,-c(1:5)])

###### MODEL 2: Best Subsets ######
model2.dat<-mod.dat
model2.dat$non_at_non<-NULL
regfit.full.BSS <-regsubsets(difference~., model2.dat[,-c(1:5)], nvmax=18)
(reg.summary.BSS<-summary(regfit.full.BSS))
names(reg.summary.BSS)

which.min(reg.summary.BSS$cp) #Best-12
(mod2.best.cp<-names(which(reg.summary.BSS$which[12,]==TRUE)))
min(reg.summary.BSS$cp) #7.89
plot(reg.summary.BSS$cp, xlab = "Number of Variables",
     ylab = "Mallow's cp", type = "l")

which.min(reg.summary.BSS$bic) #Best-7
min(reg.summary.BSS$bic) #-419.1
(mod2.best.bic<-names(which(reg.summary.BSS$which[7,]==TRUE)))
plot(reg.summary.BSS$bic, xlab = "Number of Variables",
     ylab = "Bayesian Information Criterion (BIC)", type = "l")

which.max(reg.summary.BSS$adjr2) #Best-17
(mod2.best.adjr2<-names(which(reg.summary.BSS$which[17,]==TRUE)))
max(reg.summary.BSS$adjr2) #0.733
plot(reg.summary.BSS$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")

### Examine what variables are best ###
mod2.best.cp
mod2.best.bic
mod2.best.adjr2
## Vars in all
  #elo_home, elo_away
  #fpi_home, fpi_away
  #spread_home, 
  #Pwr_Rk_home, Pw_Rk_away

## vars in 2
  #sp_plus_home, sp_plus_away
  #postgame_wp_home *Add
  #off_total_ppa_away

## Vars in 1
  #recruiting_score_away
  #AP_at_AP
  #non_at_AP

# Next model is all variables in all or in 2
new_var_list_1<-c("elo_home", "elo_away", "fpi_home", "fpi_away", "Pwr_Rk_home", "Pwr_Rk_away","spread_home",
                  "postgame_wp_home","sp_plus_home", "sp_plus_away","off_total_ppa_away"
                  ) 

## Model 3: Model on new full Variable list
model3.dat<-mod.dat[train,]%>%
  select(game, year, week, school_home, school_away,all_of(new_var_list_1), difference)
mod3.test.dat<-mod.dat[test,]%>%
  select(game, year, week, school_home, school_away,all_of(new_var_list_1), difference)

fit.3 <- lm(difference ~., data = model3.dat[,-c(1:5)])
summary(fit.3) #rsq = 0.739
mod3.preds<-round(predict(fit.3, mod3.test.dat[, -c(1:5)] ), 1)
mod3.test.dat$predicted<-mod3.preds
# Total Absolute Difference
sum(abs(mod3.test.dat$predicted-mod3.test.dat$difference)) #1080.1
# Mean Absolute Difference
mean(abs(mod3.test.dat$predicted-mod3.test.dat$difference)) #9.0
plot(mod3.test.dat$predicted,mod3.test.dat$difference)
# Correct side
mod3.test.dat$side<-ifelse((mod3.test.dat$predicted<0 & mod3.test.dat$difference<0) | (mod3.test.dat$predicted>0 & mod3.test.dat$difference>0),1,0 )
sum(mod3.test.dat$side)/nrow(mod3.test.dat) #82.5%
# Cover Spread
mod3.test.dat$spread<-(-1*mod3.test.dat$spread_home)
mod3.test.dat$cover<-ifelse(mod3.test.dat$difference>=mod3.test.dat$spread,1,0)
mod3.test.dat$pred_cover<-ifelse(mod3.test.dat$predicted>=mod3.test.dat$spread,1,0)
mod3.test.dat$pred_cover_correct<-ifelse(mod3.test.dat$cover==mod3.test.dat$pred_cover,1,0)
sum(mod3.test.dat$pred_cover_correct)/nrow(mod3.test.dat) #88.3%
xtabs(~pred_cover+cover, mod3.test.dat) # Most common error is predicting no cover when actually does cover

## Model 4: Model on new Variable list reduced based on previous model
new_var_list_2<-c("elo_home", "elo_away", "fpi_home", "fpi_away", "Pwr_Rk_home", "Pwr_Rk_away","spread_home",
                  "postgame_wp_home"
)
model4.dat<-mod.dat[train,]%>%
  select(game, year, week, school_home, school_away,all_of(new_var_list_2), difference)
mod4.test.dat<-mod.dat[test,]%>%
  select(game, year, week, school_home, school_away,all_of(new_var_list_2), difference)

fit.4 <- lm(difference ~., data = model4.dat[,-c(1:5)])
summary(fit.4) #rsq=0.729
mod4.preds<-round(predict(fit.4, mod4.test.dat[, -c(1:5)] ), 1)

mod4.test.dat$predicted<-mod4.preds

# Total Absolute Difference
sum(abs(mod4.test.dat$predicted-mod4.test.dat$difference)) #1090.5
# Mean Absolute Difference
mean(abs(mod4.test.dat$predicted-mod4.test.dat$difference)) #9.09
plot(mod4.test.dat$predicted,mod4.test.dat$difference)
# Correct side
mod4.test.dat$side<-ifelse((mod4.test.dat$predicted<0 & mod4.test.dat$difference<0) | (mod4.test.dat$predicted>0 & mod4.test.dat$difference>0),1,0 )
sum(mod4.test.dat$side)/nrow(mod4.test.dat) #77.5
# Cover Spread
mod4.test.dat$spread<-(-1*mod4.test.dat$spread_home)
mod4.test.dat$cover<-ifelse(mod4.test.dat$difference>=mod4.test.dat$spread,1,0)
mod4.test.dat$pred_cover<-ifelse(mod4.test.dat$predicted>=mod4.test.dat$spread,1,0)
mod4.test.dat$pred_cover_correct<-ifelse(mod4.test.dat$cover==mod4.test.dat$pred_cover,1,0)
sum(mod4.test.dat$pred_cover_correct)/nrow(mod4.test.dat)
xtabs(~pred_cover+cover, mod4.test.dat) #90%

## Model 5: Model on new Variable list expanded further parsed
new_var_list_3<-c("elo_home", "elo_away", "fpi_home", "fpi_away", "Pwr_Rk_home", "Pwr_Rk_away","spread_home"
)
model5.dat<-mod.dat[train,]%>%
  select(game, year, week, school_home, school_away,all_of(new_var_list_3), difference)
mod5.test.dat<-mod.dat[test,]%>%
  select(game, year, week, school_home, school_away,all_of(new_var_list_3), difference)

fit.5 <- lm(difference ~., data = model5.dat[,-c(1:5)])
summary(fit.5) #rsq=0.727
mod5.preds<-round(predict(fit.5, mod5.test.dat[, -c(1:5)] ), 1)

mod5.test.dat$predicted<-mod5.preds

# Total Absolute Difference
sum(abs(mod5.test.dat$predicted-mod5.test.dat$difference)) #1088.5

# Mean Absolute Difference
mean(abs(mod5.test.dat$predicted-mod5.test.dat$difference)) #9.07
plot(mod5.test.dat$predicted,mod5.test.dat$difference)

# Correct side
mod5.test.dat$side<-ifelse((mod5.test.dat$predicted<0 & mod5.test.dat$difference<0) | (mod5.test.dat$predicted>0 & mod5.test.dat$difference>0),1,0 )
sum(mod5.test.dat$side)/nrow(mod5.test.dat) #79.2
# Cover Spread
mod5.test.dat$spread<-(-1*mod5.test.dat$spread_home)
mod5.test.dat$cover<-ifelse(mod5.test.dat$difference>=mod5.test.dat$spread,1,0)
mod5.test.dat$pred_cover<-ifelse(mod5.test.dat$predicted>=mod5.test.dat$spread,1,0)
mod5.test.dat$pred_cover_correct<-ifelse(mod5.test.dat$cover==mod5.test.dat$pred_cover,1,0)
sum(mod5.test.dat$pred_cover_correct)/nrow(mod5.test.dat)#88.3%
xtabs(~pred_cover+cover, mod5.test.dat)

### So far model 3 is superior ###

## Model 6: Model on new Variable list expanded based on model 3 output (for investigation)
new_var_list_3<-c("elo_home", "elo_away", "fpi_home", "fpi_away", "Pwr_Rk_home", "Pwr_Rk_away","spread_home",
                   "sp_plus_away"
)
model6.dat<-mod.dat[train,]%>%
  select(game, year, week, school_home, school_away,all_of(new_var_list_3), difference)
mod6.test.dat<-mod.dat[test,]%>%
  select(game, year, week, school_home, school_away,all_of(new_var_list_3), difference)

fit.6 <- lm(difference ~., data = model6.dat[,-c(1:5)])
summary(fit.6) #rsq=0.736
mod6.preds<-round(predict(fit.6, mod6.test.dat[, -c(1:5)] ), 1)
mod6.test.dat$predicted<-mod6.preds

# Total Absolute Difference
sum(abs(mod6.test.dat$predicted-mod6.test.dat$difference)) #1097.7
# Mean Absolute Difference
mean(abs(mod6.test.dat$predicted-mod6.test.dat$difference)) #9.15
plot(mod6.test.dat$predicted,mod6.test.dat$difference)
# Correct side
mod6.test.dat$side<-ifelse((mod6.test.dat$predicted<0 & mod6.test.dat$difference<0) | (mod6.test.dat$predicted>0 & mod6.test.dat$difference>0),1,0 )
sum(mod6.test.dat$side)/nrow(mod6.test.dat) #82.5
# Cover Spread
mod6.test.dat$spread<-(-1*mod6.test.dat$spread_home)
mod6.test.dat$cover<-ifelse(mod6.test.dat$difference>=mod6.test.dat$spread,1,0)
mod6.test.dat$pred_cover<-ifelse(mod6.test.dat$predicted>=mod6.test.dat$spread,1,0)
mod6.test.dat$pred_cover_correct<-ifelse(mod6.test.dat$cover==mod6.test.dat$pred_cover,1,0)
sum(mod6.test.dat$pred_cover_correct)/nrow(mod6.test.dat) #87.5%
xtabs(~pred_cover+cover, mod6.test.dat)


# ##### MODEL 3 is BEST !!! ##########

## Predict with Confidence Interval
summary(fit.3)
mod3.preds.v2<-round(predict(fit.3, mod3.test.dat[, -c(1:5)], interval = "prediction", level = .2 ), 1)
mod3.test.dat<-cbind(mod3.test.dat, mod3.preds.v2)
# Calculate if difference is within interval
ifelse(mod3.test.dat$difference<=mod3.test.dat$upr & mod3.test.dat$difference>=mod3.test.dat$lwr, 1, 0)

