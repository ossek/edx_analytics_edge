nba_test = read.csv("./NBA_test.csv")
nba_train = read.csv("./NBA_train.csv")

str(nba_train)

#how many games does a team need to make it to the playoffs?
playoffMakers = subset(nba_train, nba_train$Playoffs == TRUE)
plot(nba_train$W, nba_train$Playoffs)
plot(playoffMakers$W, playoffMakers$FG)
plot(playoffMakers$W,playoffMakers$FT)
plot(playoffMakers$W,playoffMakers$PTS)

table(nba_train$W,nba_train$Playoffs)
#TA says, based on this data it looks like about 42 games is a good threshold.
#  at 42 games won, 8 didn't go to playoffs and 29 did

#can we use difference betweeen points scored and points allowed to predict 
# a playoffs trip?
nba_train$PointsMargin = nba_train$PTS - nba_train$oppPTS
plot(nba_train$PointsMargin,nba_train$W)

#makin' a model makin' a model
winsReg = lm(W ~ PointsMargin,data = nba_train)
summary(winsReg)

#let's write down the regression equation this gives us:
# W = 41 + 3.259e-02*PointsMargin
# solving for W >= 42
#  PointsMargin >=  (42 - 41) / 3.259e-02
#  PointsMargin >= 30.68426

#let's try a more elaborate model for points scored
pointsReg = lm(PTS ~ X2PA + X3PA + FTA + ORB + 
                 DRB + AST + STL + BLK + TOV, data = nba_train) 
summary(pointsReg)

#let's find SSE
SSE = sum(pointsReg$residuals^2)
SST = sum((nba_train$PTS -  mean(nba_train$PTS))^2)
RSQ = 1 - SSE/SST

# root mean squared error
RMSE = sqrt(SSE/nrow(nba_train))
#On average, we make an error of <RMSE> points