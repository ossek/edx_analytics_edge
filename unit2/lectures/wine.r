prwine = read.csv("./wine.csv")
str(wine)
summary(wine)

model1 = lm(wine$Price ~ wine$AGST)
summary(model1)
str(model1)
plot(model1)
model1$residuals

model1$residuals
model1$residuals^2
SSE = sum(model1$residuals^2)
SSE

#model2 = lm(wine$Price ~ wine$AGST + wine$HarvestRain)
#or save some typing:
model2 = lm(Price ~ AGST + HarvestRain,data = wine)
summary(model2)
SSE2 = sum(model2$residuals^2)
SSE2

model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop,data = wine)
summary(model3)
SSE3 = sum(model3$residuals^2)
SSE3

#Video 4 Quick questions
vid4 = lm(wine$Price ~ wine$HarvestRain + wine$WinterRain)
# recall that estimate column are the Beta values for our model
summary(vid4)

#video 5
# the star coding scheme from summary function called on a model using all variables told us
# that FrancePop and Age were insignificant.  Try without FrancePop:
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop,data = wine)
summary(model3)
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age,data = wine)
summary(model4)
#we see in this summary that the signif code wen up for Age -- this is due to multicollinearity.
#  Age and FrancePop are highly correlated

# quick question
#what is correlation for HarvestRain and WinterRain?
cor(wine$HarvestRain,wine$WinterRain)

#vide 7
wineTest = read.csv("wine_test.csv")
str(wineTest)
predictTest = predict(model4,newdata = wineTest)
predictTest
SSE = sum((wineTest$Price - predictTest)^2)
#difference between our test datapoint actual values and the values of the 'baseline' model
SST = sum((wineTest$Price - mean(wine$Price))^2)
testSetRSquared = 1 - SSE/SST
testSetRSquared
  
####lecture sequence 2
baseball= read.csv("baseball.csv")
str(baseball)
#RS is runs scored RA is runs allowed
moneyball = subset(baseball,baseball$Year < 2002)
str(moneyball)
moneyball$RD = moneyball$RS - moneyball$RA
#visual check for relation between RD and Wins, on x and y respectively
plot(moneyball$RD,moneyball$W)

#make a model
winsReg = lm(moneyball$W ~ moneyball$RD)
summary(winsReg)
#the coefficients table tells us our equation is
#W = 80.8814 + 0.10576(RD)
#our goal is to solve for RD so W >= 95
#80.8814 + 0.10576(RD) >= 95
#solving for RD:
(95-80.8814)/.10576

#quick question 1
#if team scores 713 runs and allows 614, how many games do we expect them to win?
80.8814 + 0.10576*(713-614)

#lookee here, OBP is 'on-base-percent' (like batting average, but includes walks), 
#and SLG is slugging percentage (how far a player gets around bases on his turn)
RunsRegOrig = lm(moneyball$RS ~ moneyball$OBP + moneyball$SLG + moneyball$BA)
summary(RunsRegOrig)
#we see that the BA coefficient is negative, which would imply that teams with lower BA
#  score more runs (that's counterintutive).  Multicollinearity of our variables makes the 
#  coefficients hard to interpret.  Try to remove BA
RunsReg = lm(moneyball$RS ~ moneyball$OBP + moneyball$SLG)
summary(RunsReg)

#can use these to predict runs allowed:
#OOBP opponent's on base percentage
#OSLG opponent's slugging percentage
ORunsReg = lm(moneyball$RA ~ moneyball$OOBP + moneyball$OSLG)
summary(ORunsReg)

#Quick Question
# If a baseball team's OBP is 0.311 and SLG is 0.405, how many runs do we expect the team to score?
# From RunsReg:
#RS = -804.65 + 2737.77(obp) + 1584.91(slg)
-804.65 + 2737.77*0.311 + 1584.91*0.405

#If a baseball team's opponents OBP (OOBP) is 0.297 and oppenents SLG (OSLG) is 0.370, 
#how many runs do we expect the team to allow?
#Using the linear regression model discussed during the 
#lecture (the one on the last slide of the previous video), enter the number of runs we expect the team to allow:
# From ORunsReg:
#RA = -837.38 + 2913.60*oobp + 1514.29*oslg
-837.38 + 2913.60*0.297 + 1514.29*0.370

####Video 4 
#We want to predict present / future player performance based on past player performance.
#  This approach makes some assumptions about those being related.
# using 2001 stats and our RunsReg model, we can estimate how many runs they scored
######I think these should be the figures from the dataset, but lecture gives different ones
# oak2001OBP = moneyball[moneyball$Year == 2001 & moneyball$Team == 'OAK',c('OBP')]
# oak2001OSLG = moneyball[moneyball$Year == 2001 & moneyball$Team == 'OAK',c('SLG')]
######  This may have to do with the lecture figures being 'etimates based on 2001 player data'
######  Whereas we have team data (??)
oak2001OBP = .339
oak2001OSLG = .430
estRunsScored = -804.65 + 2737.77*oak2001OBP + 1584.91*oak2001OSLG

# Same thing for opponents
OOPB2001 = .307
OSLG2001 = .373 
estRunsAllowed = -837.38 + 2913.60*OOPB2001 + 1514.29*OSLG2001

#We have a prediction for RunsScored, and for RunsAllowed; we can use these to predict Wins
#  From our model
estimatedWins = 80.8814 + 0.10576*(estRunsScored-estRunsAllowed)

#Quick Question:
#Suppose you are the General Manager of a baseball team, and you are selecting TWO players for your team. 
#You have a budget of $1,500,000, and you have the choice between the following players:

estimateRunsForOakPlayer <- function(OBP,SLG)
{
  -804.65 + 2737.77*OBP + 1584.91*SLG
}
#Player Name  OBP	SLG	Salary
#Eric Chavez	0.338	0.540	$1,400,000
#Jeremy Giambi	0.391	0.450	$1,065,000
#Frank Menechino	0.369	0.374	$295,000
#Greg Myers	0.313	0.447	$800,000
#Carlos Pena	0.361	0.500	$300,000
Chavez   = estimateRunsForOakPlayer(0.338	,0.540)
Giambi	 = estimateRunsForOakPlayer(0.391	,0.450)
Menechino  = estimateRunsForOakPlayer(0.369,0.374)
Myers	 = estimateRunsForOakPlayer(0.313	,0.447)
Pena	 = estimateRunsForOakPlayer(0.361	,0.500)
# show em
Chavez
Giambi
Menechino
Myers
Pena  

#Video 5 Question 

teamRank = c(1,2,3,3,4,4,4,4,5,5)
#rank 1
GiantsWins = 94
#rank 2
TigersWins = 88
#rank 3
YankeesWins = 95
CardinalsWins = 88
# Rank 4: 
OriolesWins = 93
AsWins = 94
NationalsWins = 98
RedsWins = 97
#Rank 5: 
RangersWins = 93
BravesWins = 94
wins2012 = c(GiantsWins,TigersWins,YankeesWins,CardinalsWins,OriolesWins,AsWins,NationalsWins,RedsWins,RangersWins,BravesWins)

#Rank 1
RedSoxWins2103 = 97
#Rank 2: 
CardinalsWins2013 = 97
#Rank 3: 
DodgersWins2103 = 92
TigersWins2103 = 93
#Rank 4: 
RaysWins2013 = 92 
AsWins2013 = 96
PiratesWins2013 = 94
BravesWins2013 = 96
#Rank 5: 
IndiansWins2013 = 92
RedsWins2013 = 90 
wins2013 = c(RedSoxWins2103,CardinalsWins2013,DodgersWins2103,TigersWins2103,
             RaysWins2013,AsWins2013,PiratesWins2013,BravesWins2013,IndiansWins2013,RedSoxWins2103)

cor(teamRank,wins2012)
cor(teamRank,wins2013)
