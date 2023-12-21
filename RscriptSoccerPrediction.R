rm(list=ls())

############################### Load all required packages #################################
library(RSQLite)
library(DBI)
library(dplyr)
library(stringr)
library(ggplot2)
library(car)
library(nortest)
library(caret)
library(shiny)
library(data.table)
library(rpart)
library(rpart.plot)
library(factoextra)
library(tidyverse)
library(MASS)
library(leaps)
library(e1071)
library(naivebayes)
library(base)
library(animation)
library(arules)
library(arulesViz)
library(olsrr)
library(lmtest)
library(forecast)
library(quantmod)
library(sandwich)
library(tseries)
library(polycor)
library(eeptools)
############################################################################################


################################### Import the data ########################################
Data=dbConnect(SQLite(),"D:/M.S - Central Michigan/STA591/Project - Team 6/database.sqlite")
Match=dbReadTable(Data,"Match")
Country=dbReadTable(Data,"Country")
League=dbReadTable(Data,"League")
Player=dbReadTable(Data,"Player")
Team=dbReadTable(Data,"Team")
Player_Attributes=dbReadTable(Data,"Player_Attributes")
Team_Attributes=dbReadTable(Data,"Team_Attributes")
############################################################################################


############################# Prepare the Match table ######################################
Match2=left_join(Match,Country,by=c("country_id"="id"))
Match3=left_join(Match2,League,by=c("league_id"="id"))
Match4=left_join(Match3,Team,by=c("home_team_api_id"="team_api_id"))
Match4$home_team_LN=Match4$team_long_name
Match4$home_team_SN=Match4$team_short_name
Match5=left_join(Match4,Team,by=c("away_team_api_id"="team_api_id"))
Match5$away_team_LN=Match5$team_long_name.y
Match5$away_team_SN=Match5$team_short_name.y
Match5$match_result=ifelse(Match5$home_team_goal==Match5$away_team_goal,"Draw",
                           ifelse(Match5$home_team_goal>Match5$away_team_goal,
                                  str_c(Match5$home_team_LN," ","(",Match5$home_team_SN,")"," won"),
                                  str_c(Match5$away_team_LN," ","(",Match5$away_team_SN,")"," won")))
Match5$match_result_who=ifelse(Match5$home_team_goal==Match5$away_team_goal,"Draw",
                               ifelse(Match5$home_team_goal>Match5$away_team_goal,"Home team",
                                      "Away team"))
names(Match5)[names(Match5)=="name.x"]="country_name"
names(Match5)[names(Match5)=="name.y"]="league_name"
############################################################################################


################################ Data visualization ########################################
par(mar=c(6,5,5,5))
x=barplot(table(Match5$country_name),las=2,angle=90,ylim=c(0,3500),
     col=c("red","blue","yellow","green","cyan","violet","orange","magenta","maroon1",
           "mediumaquamarine","mediumturquoise"),main="Matches played by Country")
y=as.data.frame(count(Match5,Match5$country_name))
colnames(y)=c("Country","Number_of_matches")
text(x,y$Number_of_matches+100,labels=y$Number_of_matches)
############################################################################################


############################# Some basic analysis ##########################################
# It is said that the home teams always have advantage and is more likely to win a match,
  # so this plot is to see if that "rumor" is true
(summary(Match5$home_team_goal))
(summary(Match5$away_team_goal))
x2=barplot(table(Match5$match_result_who),ylab="Win count",xlab="Result",main="Match results",
        col=c("red","blue","violet"),ylim=c(0,15000))
y2=as.data.frame(count(Match5,Match5$match_result_who))
colnames(y2)=c("who_won","matches_count")
text(x2,y2$matches_count+300,labels=y2$matches_count)
# --> The plot shows the number of win matches looks to be higher for the home teams, so
  # it may be true to say that home teams is more likely to win a match. Let's check it
  # statistically using a paired t-test
# Create the datasets used for testing
A1=as.numeric(nrow(subset(Match5,Match5$league_name=="Belgium Jupiler League"&
                 Match5$match_result_who=="Home team")))
A2=as.numeric(nrow(subset(Match5,Match5$league_name=="England Premier League"&
                            Match5$match_result_who=="Home team")))
A3=as.numeric(nrow(subset(Match5,Match5$league_name=="France Ligue 1"&
                            Match5$match_result_who=="Home team")))
A4=as.numeric(nrow(subset(Match5,Match5$league_name=="Germany 1. Bundesliga"&
                            Match5$match_result_who=="Home team")))
A5=as.numeric(nrow(subset(Match5,Match5$league_name=="Italy Serie A"&
                            Match5$match_result_who=="Home team")))
A6=as.numeric(nrow(subset(Match5,Match5$league_name=="Netherlands Eredivisie"&
                            Match5$match_result_who=="Home team")))
A7=as.numeric(nrow(subset(Match5,Match5$league_name=="Poland Ekstraklasa"&
                            Match5$match_result_who=="Home team")))
A8=as.numeric(nrow(subset(Match5,Match5$league_name=="Portugal Liga ZON Sagres"&
                            Match5$match_result_who=="Home team")))
A9=as.numeric(nrow(subset(Match5,Match5$league_name=="Scotland Premier League"&
                            Match5$match_result_who=="Home team")))
A10=as.numeric(nrow(subset(Match5,Match5$league_name=="Spain LIGA BBVA"&
                            Match5$match_result_who=="Home team")))
A11=as.numeric(nrow(subset(Match5,Match5$league_name=="Switzerland Super League"&
                            Match5$match_result_who=="Home team")))
B1=as.numeric(nrow(subset(Match5,Match5$league_name=="Belgium Jupiler League"&
                            Match5$match_result_who=="Away team")))
B2=as.numeric(nrow(subset(Match5,Match5$league_name=="England Premier League"&
                            Match5$match_result_who=="Away team")))
B3=as.numeric(nrow(subset(Match5,Match5$league_name=="France Ligue 1"&
                            Match5$match_result_who=="Away team")))
B4=as.numeric(nrow(subset(Match5,Match5$league_name=="Germany 1. Bundesliga"&
                            Match5$match_result_who=="Away team")))
B5=as.numeric(nrow(subset(Match5,Match5$league_name=="Italy Serie A"&
                            Match5$match_result_who=="Away team")))
B6=as.numeric(nrow(subset(Match5,Match5$league_name=="Netherlands Eredivisie"&
                            Match5$match_result_who=="Away team")))
B7=as.numeric(nrow(subset(Match5,Match5$league_name=="Poland Ekstraklasa"&
                            Match5$match_result_who=="Away team")))
B8=as.numeric(nrow(subset(Match5,Match5$league_name=="Portugal Liga ZON Sagres"&
                            Match5$match_result_who=="Away team")))
B9=as.numeric(nrow(subset(Match5,Match5$league_name=="Scotland Premier League"&
                            Match5$match_result_who=="Away team")))
B10=as.numeric(nrow(subset(Match5,Match5$league_name=="Spain LIGA BBVA"&
                             Match5$match_result_who=="Away team")))
B11=as.numeric(nrow(subset(Match5,Match5$league_name=="Switzerland Super League"&
                             Match5$match_result_who=="Away team")))
HomeWin=c(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)
AwayWin=c(B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11)
# Ho : The mean number of matches that home teams win and away teams win are the same
# Ha : The mean number of matches that home teams win is more than away teams win
leveneTest(c(HomeWin,AwayWin),c(rep(1,11),rep(2,11))) 
# --> Check for equal variances result in variances are not equal
ad.test(HomeWin) # --> Check for normality, HomeWin data is normal
ad.test(AwayWin) # --> Check for normality, AwayWin data is normal
t.test(HomeWin,AwayWin,alternative="greater",paired=TRUE,var.equal=FALSE)
# --> p-value is 3.506e-06 < 0.05, so it's true that home teams are more likely to win
  # a match
  
# It is also said that England Premier Leagues usually has more draw matches than the 
  # other leagues where there is one or two "dominating" team, so this plot is to check 
  # that
LeaguesData=as.data.frame(cbind(Match5$league_name,Match5$match_result_who))
colnames(LeaguesData)=c("League_name","Match_results")
LeaguesData$MatchWLD=ifelse(LeaguesData$Match_results=="Draw","Draw","One team won")
LeaguesDataTable=data.frame(table(LeaguesData$League_name,LeaguesData$MatchWLD))
colnames(LeaguesDataTable)=c("Leagues","Result","Count")
ggplot(data=LeaguesDataTable,aes(x=Leagues,y=Count,fill=Result))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90))
#--> The proportion between the number of matches that ends up in a draw or one team win
  # look the same between the leagues, so that conclusion is not likely to be true. Again,
  # let's check it statistically using a 2-way ANOVA test
LeaguesDataTable=as.data.frame((LeaguesDataTable))
Leagues=as.factor(LeaguesDataTable$Leagues)
Result=as.factor(LeaguesDataTable$Result)
EPL_Check=aov(LeaguesDataTable$Count~Result+Leagues)
shapiro.test(EPL_Check$residuals) # Check for normality, the data is normal
summary(EPL_Check)
(Tukey_result=TukeyHSD(EPL_Check))
plot(Tukey_result$Leagues[ ,4],type="l",xlab="Comparisons",ylab="p-values",col="red",
     main="p-values of the Tukey comparisons between leagues",lwd=1.5,ylim=c(0.01,1))
abline(h=0.05,col="blue",lwd=1.5)
Tukey_result_pval=as.data.frame(Tukey_result$Leagues[ ,4])
colnames(Tukey_result_pval)="p_value"
Significant_EPL=subset(Tukey_result_pval,Tukey_result_pval$p_value<0.05)
# Based on the ANOVA result and the Tukey comparisons, it does have a significant 
  # difference between the number of matches that ends up in a draw and those that have a
  # winner; but there is no difference between the leagues, hence, England Premier League
  # doesn't have more draw matches than the other European leagues
# NOTE : There is an issue that the ANOVA test shows that there is a difference between the
  # leagues but none of the Tukey pairwise comparisons indicate a difference. This is
  # due to the F-value and p-value for the overall test and the pairwise comparisons have
  # some differences.

# When watching soccer, people usually prefer matches that have goals. This test is to
  # check if there is a significant difference in the number of goals of each league, if
  # yes, a pairwise comparison is performed to find out the significant different pairs
Match5$MatchGoals=Match5$home_team_goal+Match5$away_team_goal
BestLeagueData=as.data.frame(cbind(Match5$league_name,Match5$MatchGoals))
colnames(BestLeagueData)=c("Leagues","Total_goals")
BestLeagueData$Total_goals=as.numeric(BestLeagueData$Total_goals)
Goal_by_League=as.data.frame(tapply(BestLeagueData$Total_goals,BestLeagueData$Leagues,sum))
colnames(Goal_by_League)="Goals"
Goal_by_League$Leagues=League$name
par(mar=c(11,5,5,5))
x3=barplot(Goal_by_League$Goals~Goal_by_League$Leagues,las=2,ylab="Total goals",xlab="",
        main="Total number of goals by League",col=c("red","blue","green",
                                                     "yellow","violet","cyan",
                                                     "orange","mediumturquoise",
                                                     "mediumaquamarine","magenta",
                                                     "maroon1"),ylim=c(0,9000))
text(x3,Goal_by_League$Goals+200,labels=Goal_by_League$Goals)
BestLeagues=as.factor(BestLeagueData$Leagues)
BestLeagueModel=aov(BestLeagueData$Total_goals~BestLeagues,data=BestLeagueData)
summary(BestLeagueModel)
BestLeagueTukey=TukeyHSD(BestLeagueModel)
LeaguesCompare=as.data.frame(BestLeagueTukey$BestLeagues[ ,c(1,4)])
colnames(LeaguesCompare)=c("Difference","p_value")
(MostGoalsLeague=subset(LeaguesCompare,LeaguesCompare$p_value>=0.05))
sort(tapply(BestLeagueData$Total_goals,BestLeagueData$Leagues,mean),decreasing=TRUE)
############################################################################################


############################# Data cleaning & Transformation ###############################
# Removing unused columns
Full_Match=subset(Match5,select=-c(id.x,country_id.x,league_id,date,home_team_api_id,
                                   away_team_api_id,country_id.y,id.y,team_fifa_api_id.x,
                                   team_long_name.x,team_short_name.x,id,team_fifa_api_id.y,
                                   team_long_name.y,team_short_name.y))
                                   
# Removing the "Not available" data
Clean_Match=na.omit(Full_Match)

# Check if the data has been shuffled
head(Clean_Match)
tail(Clean_Match)
# --> It hasn't been shuffled, which will lead to poor prediction when we build models

# Shuffle the data
ShuffleIndex=sample(1:nrow(Clean_Match))
Clean_Match=Clean_Match[ShuffleIndex, ]
head(Clean_Match)
tail(Clean_Match)
############################################################################################


############################## Predictions on players ######################################
# Prepare the table
Player_Data=left_join(Player,Player_Attributes,by="player_fifa_api_id")
Player_Data$age=round(age_calc(as.Date(Player_Data$birthday),as.Date(Player_Data$date),
                         units="years"),1)
Player_Data_clean=na.omit(subset(Player_Data,select=-c(id.x,player_api_id.x,birthday,
                                                       player_fifa_api_id,id.y,date,
                                                       player_api_id.y,player_name)))
# Create dummy variables & remove unused columns
Player_Data_clean$x1=ifelse(Player_Data_clean$preferred_foot=="right",1,0)
Player_Data_clean$x2=ifelse(Player_Data_clean$attacking_work_rate=="high",1,0)
Player_Data_clean$x3=ifelse(Player_Data_clean$attacking_work_rate=="medium",1,0)
Player_Data_clean$x4=ifelse(Player_Data_clean$defensive_work_rate=="high",1,0)
Player_Data_clean$x5=ifelse(Player_Data_clean$defensive_work_rate=="medium",1,0)
Player_model_data=subset(Player_Data_clean,select=-c(preferred_foot,attacking_work_rate,
                                                     defensive_work_rate))
# Create a model for goal keeper and decide which variable is important
GK_model=lm(overall_rating~reactions+gk_diving+gk_handling+gk_kicking+gk_positioning+
              gk_reflexes+age,data=Player_model_data)
summary(GK_model)
#(ols_step_both_aic(GK_model,details=FALSE))
# Line above take long time to run so comment out when save

# Create a model for defender and decide which variable is important
DF_model=lm(overall_rating~reactions+x4+x5+agility+interceptions+acceleration+
              standing_tackle+sliding_tackle+marking+age,data=Player_model_data)
summary(DF_model)
#(ols_step_both_aic(DF_model,details=FALSE))
# Line above take long time to run so comment out when save

# Create a model for Midfielder and decide which variable is important
MF_model=lm(overall_rating~reactions+x4+x5+x2+x3+crossing+short_passing+long_passing+
              ball_control+positioning+vision+age,data=Player_model_data)
summary(MF_model)
#(ols_step_both_aic(MF_model,details=FALSE))
# Line above take long time to run so comment out when save

# Create a model for Forwarder and decide which variable is important
FW_model=lm(overall_rating~reactions+x1+x2+x3+volleys+dribbling+curve+acceleration+
              shot_power+jumping+long_shots+penalties+age,data=Player_model_data)
summary(FW_model)
#(ols_step_both_aic(FW_model,details=FALSE))
# Line above take long time to run so comment out when save
###########################################################################################


#################################### Match predictions ####################################
Match_Predict_Data=cbind(Clean_Match[ ,80:109],Clean_Match$match_result_who)
names(Match_Predict_Data)[names(Match_Predict_Data)=="Clean_Match$match_result_who"]="Result"
Match_Predict_Data$Result=ifelse(Match_Predict_Data$Result=="Home team","Home",
                                 ifelse(Match_Predict_Data$Result=="Away team","Away","Draw"))
# Determine "levels" for betting odds
# B365
range(Match_Predict_Data$B365H)
Match_Predict_Data$B365H=ifelse(Match_Predict_Data$B365H<=2,"Low",
                                ifelse(Match_Predict_Data$B365H>=3.5,"High","Medium"))
range(Match_Predict_Data$B365D)
Match_Predict_Data$B365D=ifelse(Match_Predict_Data$B365D<=3.5,"Low",
                                ifelse(Match_Predict_Data$B365D>=5,"High","Medium"))
range(Match_Predict_Data$B365A)
Match_Predict_Data$B365A=ifelse(Match_Predict_Data$B365A<=3,"Low",
                                ifelse(Match_Predict_Data$B365A>=8,"High","Medium"))
# BW
range(Match_Predict_Data$BWH)
Match_Predict_Data$BWH=ifelse(Match_Predict_Data$BWH<=2.5,"Low",
                                ifelse(Match_Predict_Data$BWH>=4.5,"High","Medium"))
range(Match_Predict_Data$BWD)
Match_Predict_Data$BWD=ifelse(Match_Predict_Data$BWD<=3.5,"Low",
                                ifelse(Match_Predict_Data$BWD>=6,"High","Medium"))
range(Match_Predict_Data$BWA)
Match_Predict_Data$BWA=ifelse(Match_Predict_Data$BWA<=2.5,"Low",
                                ifelse(Match_Predict_Data$BWA>=5,"High","Medium"))
# IW
range(Match_Predict_Data$IWH)
Match_Predict_Data$IWH=ifelse(Match_Predict_Data$IWH<=2,"Low",
                                ifelse(Match_Predict_Data$IWH>=4,"High","Medium"))
range(Match_Predict_Data$IWD)
Match_Predict_Data$IWD=ifelse(Match_Predict_Data$IWD<=3.2,"Low",
                                ifelse(Match_Predict_Data$IWD>=4,"High","Medium"))
range(Match_Predict_Data$IWA)
Match_Predict_Data$IWA=ifelse(Match_Predict_Data$IWA<=3,"Low",
                                ifelse(Match_Predict_Data$IWA>=4.5,"High","Medium"))
# LB
range(Match_Predict_Data$LBH)
Match_Predict_Data$LBH=ifelse(Match_Predict_Data$LBH<=2,"Low",
                                ifelse(Match_Predict_Data$LBH>=4,"High","Medium"))
range(Match_Predict_Data$LBD)
Match_Predict_Data$LBD=ifelse(Match_Predict_Data$LBD<=3.5,"Low",
                                ifelse(Match_Predict_Data$LBD>=5,"High","Medium"))
range(Match_Predict_Data$LBA)
Match_Predict_Data$LBA=ifelse(Match_Predict_Data$LBA<=2,"Low",
                                ifelse(Match_Predict_Data$LBA>=4.5,"High","Medium"))
# PS
range(Match_Predict_Data$PSH)
Match_Predict_Data$PSH=ifelse(Match_Predict_Data$PSH<=2,"Low",
                              ifelse(Match_Predict_Data$PSH>=4.5,"High","Medium"))
range(Match_Predict_Data$PSD)
Match_Predict_Data$PSD=ifelse(Match_Predict_Data$PSD<=3.5,"Low",
                              ifelse(Match_Predict_Data$PSD>=5,"High","Medium"))
range(Match_Predict_Data$PSA)
Match_Predict_Data$PSA=ifelse(Match_Predict_Data$PSA<=2.5,"Low",
                              ifelse(Match_Predict_Data$PSA>=5,"High","Medium"))
# WH
range(Match_Predict_Data$WHH)
Match_Predict_Data$WHH=ifelse(Match_Predict_Data$WHH<=2,"Low",
                              ifelse(Match_Predict_Data$WHH>=4,"High","Medium"))
range(Match_Predict_Data$WHD)
Match_Predict_Data$WHD=ifelse(Match_Predict_Data$WHD<=3.4,"Low",
                              ifelse(Match_Predict_Data$WHD>=5,"High","Medium"))
range(Match_Predict_Data$WHA)
Match_Predict_Data$WHA=ifelse(Match_Predict_Data$WHA<=2.5,"Low",
                              ifelse(Match_Predict_Data$WHA>=6,"High","Medium"))
# SJ
range(Match_Predict_Data$SJH)
Match_Predict_Data$SJH=ifelse(Match_Predict_Data$SJH<=2,"Low",
                              ifelse(Match_Predict_Data$SJH>=3.5,"High","Medium"))
range(Match_Predict_Data$SJD)
Match_Predict_Data$SJD=ifelse(Match_Predict_Data$SJD<=3.5,"Low",
                              ifelse(Match_Predict_Data$SJD>=4.5,"High","Medium"))
range(Match_Predict_Data$SJA)
Match_Predict_Data$SJA=ifelse(Match_Predict_Data$SJA<=2.5,"Low",
                              ifelse(Match_Predict_Data$SJA>=4.5,"High","Medium"))
# VC
range(Match_Predict_Data$VCH)
Match_Predict_Data$VCH=ifelse(Match_Predict_Data$VCH<=2,"Low",
                              ifelse(Match_Predict_Data$VCH>=4,"High","Medium"))
range(Match_Predict_Data$VCD)
Match_Predict_Data$VCD=ifelse(Match_Predict_Data$VCD<=3.5,"Low",
                              ifelse(Match_Predict_Data$VCD>=4.5,"High","Medium"))
range(Match_Predict_Data$VCA)
Match_Predict_Data$VCA=ifelse(Match_Predict_Data$VCA<=2.5,"Low",
                              ifelse(Match_Predict_Data$VCA>=4.5,"High","Medium"))
# GB
range(Match_Predict_Data$GBH)
Match_Predict_Data$GBH=ifelse(Match_Predict_Data$GBH<=2,"Low",
                              ifelse(Match_Predict_Data$GBH>=4.5,"High","Medium"))
range(Match_Predict_Data$GBD)
Match_Predict_Data$GBD=ifelse(Match_Predict_Data$GBD<=3.5,"Low",
                              ifelse(Match_Predict_Data$GBD>=4.5,"High","Medium"))
range(Match_Predict_Data$GBA)
Match_Predict_Data$GBA=ifelse(Match_Predict_Data$GBA<=2.5,"Low",
                              ifelse(Match_Predict_Data$GBA>=4.5,"High","Medium"))
# BS
range(Match_Predict_Data$BSH)
Match_Predict_Data$BSH=ifelse(Match_Predict_Data$BSH<=2,"Low",
                              ifelse(Match_Predict_Data$BSH>=4,"High","Medium"))
range(Match_Predict_Data$BSD)
Match_Predict_Data$BSD=ifelse(Match_Predict_Data$BSD<=3.5,"Low",
                              ifelse(Match_Predict_Data$BSD>=4.5,"High","Medium"))
range(Match_Predict_Data$BSA)
Match_Predict_Data$BSA=ifelse(Match_Predict_Data$BSA<=2.5,"Low",
                              ifelse(Match_Predict_Data$BSA>=5,"High","Medium"))
# Create Train and Test sets
Match_Predict_Train=Match_Predict_Data[1:ceiling(0.8*nrow(Match_Predict_Data)), ]
Match_Predict_Test=Match_Predict_Data[(ceiling(0.8*nrow(Match_Predict_Data))+1):nrow(Match_Predict_Data), ]

# Write a function to perform match result prediction for all betting providers (provide
  # decision tree and accuracy)
B365Train=as.data.frame(Match_Predict_Train[,c(1:3,31)])
B365Test=as.data.frame(Match_Predict_Test[,c(1:3,31)])
BWTrain=as.data.frame(Match_Predict_Train[,c(4:6,31)])
BWTest=as.data.frame(Match_Predict_Test[,c(4:6,31)])
IWTrain=as.data.frame(Match_Predict_Train[,c(7:9,31)])
IWTest=as.data.frame(Match_Predict_Test[,c(7:9,31)])
LBTrain=as.data.frame(Match_Predict_Train[,c(10:12,31)])
LBTest=as.data.frame(Match_Predict_Test[,c(10:12,31)])
PSTrain=as.data.frame(Match_Predict_Train[,c(13:15,31)])
PSTest=as.data.frame(Match_Predict_Test[,c(13:15,31)])
WHTrain=as.data.frame(Match_Predict_Train[,c(16:18,31)])
WHTest=as.data.frame(Match_Predict_Test[,c(16:18,31)])
SJTrain=as.data.frame(Match_Predict_Train[,c(19:21,31)])
SJTest=as.data.frame(Match_Predict_Test[,c(19:21,31)])
VCTrain=as.data.frame(Match_Predict_Train[,c(22:24,31)])
VCTest=as.data.frame(Match_Predict_Test[,c(22:24,31)])
GBTrain=as.data.frame(Match_Predict_Train[,c(25:27,31)])
GBTest=as.data.frame(Match_Predict_Test[,c(25:27,31)])
BSTrain=as.data.frame(Match_Predict_Train[,28:31])
BSTest=as.data.frame(Match_Predict_Test[,28:31])

# Decision tree
SoccerBet=function(bet_provider){
  Provider=toupper(bet_provider)
  BetDataTrain=case_when(Provider=="B365"~B365Train,Provider=="BW"~BWTrain,
                         Provider=="IW"~IWTrain,Provider=="LB"~LBTrain,
                         Provider=="PS"~PSTrain,Provider=="WH"~WHTrain,
                         Provider=="SJ"~SJTrain,Provider=="VC"~VCTrain,
                         Provider=="GB"~GBTrain,Provider=="BS"~BSTrain)
  BetDataTest=case_when(Provider=="B365"~B365Test,Provider=="BW"~BWTest,
                         Provider=="IW"~IWTest,Provider=="LB"~LBTest,
                         Provider=="PS"~PSTest,Provider=="WH"~WHTest,
                         Provider=="SJ"~SJTest,Provider=="VC"~VCTest,
                         Provider=="GB"~GBTest,Provider=="BS"~BSTest)
  BetModel=rpart(Result~.,data=BetDataTrain,method='class')
  DecisionTree=rpart.plot(BetModel,extra="auto",cex=0.6)
  ResultPredict=predict(BetModel,BetDataTest,type='class')
  Result_confMatrix=table(BetDataTest$Result,ResultPredict)
  ProviderAccuracy=sum(diag(Result_confMatrix))/sum(Result_confMatrix)
  return(paste0("The accuracy of ",Provider," provider is ",100*round(ProviderAccuracy,4),"%"))
}
SoccerBet("B365")
SoccerBet("bW")
SoccerBet("iw")
SoccerBet("Lb")
SoccerBet("PS")
SoccerBet("wh")
SoccerBet("sJ")
SoccerBet("Vc")
SoccerBet("gb")
SoccerBet("bS")

# Naive Bayes classification
SoccerBet_NB=function(bet_provider){
  Provider=toupper(bet_provider)
  BetDataTrain=case_when(Provider=="B365"~B365Train,Provider=="BW"~BWTrain,
                         Provider=="IW"~IWTrain,Provider=="LB"~LBTrain,
                         Provider=="PS"~PSTrain,Provider=="WH"~WHTrain,
                         Provider=="SJ"~SJTrain,Provider=="VC"~VCTrain,
                         Provider=="GB"~GBTrain,Provider=="BS"~BSTrain)
  BetDataTest=case_when(Provider=="B365"~B365Test,Provider=="BW"~BWTest,
                        Provider=="IW"~IWTest,Provider=="LB"~LBTest,
                        Provider=="PS"~PSTest,Provider=="WH"~WHTest,
                        Provider=="SJ"~SJTest,Provider=="VC"~VCTest,
                        Provider=="GB"~GBTest,Provider=="BS"~BSTest)
  BetModel=naiveBayes(Result~.,data=BetDataTrain)
  ResultPredict=predict(BetModel,BetDataTest,type='class')
  Result_confMatrix=table(BetDataTest$Result,ResultPredict)
  ProviderAccuracy=sum(diag(Result_confMatrix))/sum(Result_confMatrix)
  return(paste0("The accuracy of ",Provider," provider is ",100*round(ProviderAccuracy,4),"%"))
}
SoccerBet_NB("B365")
SoccerBet_NB("bW")
SoccerBet_NB("iw")
SoccerBet_NB("Lb")
SoccerBet_NB("PS")
SoccerBet_NB("wh")
SoccerBet_NB("sJ")
SoccerBet_NB("Vc")
SoccerBet_NB("gb")
SoccerBet_NB("bS")

# The accuracy, although not so high, but they're appropriate given that we are predicting
  # using betting odds. Betting is a type of gambling, so we cannot get a high accuracy
  # model

############################################################################################


######################### Clustering on Teams' Attributes ################################
Team_Attributes2=left_join(Team_Attributes,Team,by="team_fifa_api_id",relationship="many-to-many")
Team_Attributes3=subset(Team_Attributes2,select=c(buildUpPlaySpeed,buildUpPlayPassing,
                                                  chanceCreationPassing,chanceCreationCrossing,
                                                  chanceCreationShooting,defencePressure,
                                                  defenceAggression,defenceTeamWidth))
# Standardize the variable
Team_Attribute_clean=na.omit(Team_Attributes3)
Team_Attribute_clean$buildUpPlaySpeed=scale(Team_Attribute_clean$buildUpPlaySpeed)
Team_Attribute_clean$buildUpPlayPassing=scale(Team_Attribute_clean$buildUpPlayPassing)
Team_Attribute_clean$chanceCreationPassing=scale(Team_Attribute_clean$chanceCreationPassing)
Team_Attribute_clean$chanceCreationCrossing=scale(Team_Attribute_clean$chanceCreationCrossing)
Team_Attribute_clean$chanceCreationShooting=scale(Team_Attribute_clean$chanceCreationShooting)
Team_Attribute_clean$defencePressure=scale(Team_Attribute_clean$defencePressure)
Team_Attribute_clean$defenceAggression=scale(Team_Attribute_clean$defenceAggression)
Team_Attribute_clean$defenceTeamWidth=scale(Team_Attribute_clean$defenceTeamWidth)
ClusterResult=kmeans(Team_Attribute_clean,3)
ClusterResult$size
ClusterResult$centers
ClusterResult$cluster
#(kmeans.ani(Team_Attribute_clean[2:3],pch=c(20,20,20),col=c("red","blue","violet")))
# *** Takes long time to run the animation process so comment out when saving *** #