gerber = read.csv("./gerber.csv")
str(gerber)
nrow(gerber[gerber$voting == TRUE,])/nrow(gerber)

nrow(gerber[gerber$civicduty == TRUE & gerber$voting == TRUE,])/nrow(gerber)
nrow(gerber[gerber$hawthorne == TRUE & gerber$voting == TRUE,])/nrow(gerber)
nrow(gerber[gerber$self == TRUE & gerber$voting == TRUE,])/nrow(gerber)
nrow(gerber[gerber$neighbors == TRUE & gerber$voting == TRUE,])/nrow(gerber)

#1.3
logrModel = glm(family = "binomial", data = gerber,voting ~ hawthorne + civicduty + neighbors + self)
summary(logrModel)

logrPredictions = predict(logrModel,type = "response")
logrConfusion = as.matrix(table(gerber$voting,logrPredictions > 0.3))
r = logrConfusion
r

#accuracy (TP + TN) / ALL
logrAccuracy = (r[1,1] + r[2,2]) / (r[1,1] + r[1,2] + r[2,1] + r[2,2])
logrAccuracy

#1.5
logrConfusion = as.matrix(table(gerber$voting,logrPredictions > 0.5))
r = logrConfusion
r

#accuracy (TN) / ALL  -- since this case only guesses false, just 
logrAccuracy = (r[1,1]) / (r[1,1] + r[2,1])
logrAccuracy

#1.6
# compare the last 2 accuracies to baseline accuracy (percentage of people who did not vote)
#  and compute model's AUC.  what's happening?
#baseline (guessing the most common all the time)
nrow(gerber[gerber$voting == FALSE,])/nrow(gerber)

#AUC
library(ROCR)
ROCpred = prediction(logrPredictions,gerber$voting)
auc = as.numeric(performance(ROCpred,"auc")@y.values)
auc
## acc with t 0.3   0.5419578
## acc with t 0.5   0.6841004
## acc with baseline  0.6841004
## auc 0.5308461
## weak predictive model

####TREES
# 2.1
## don't use class because we are going to create a regression tree
cartGerber = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(cartGerber)
#### we see with this none of the variables matter enough to split on

cartGerber2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp = 0.0)
prp(cartGerber2)

#2.3
#a boolean var v can only split into 2 classes for each, < 0.5 (v == false), and >= 0.5 (v == true)

#2.4
cartGerber3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp = 0.0)
prp(cartGerber3)
# sex is 0 for male (< 0.5) and 1 for female (>= 0.5)

#3.1
cartControlOnly = rpart(voting ~ control, data=gerber, cp = 0.0)
prp(cartControlOnly,digits = 6)
abs(.296638 - .34)

#3.2
cartSex = rpart(voting ~ control + sex , data=gerber, cp = 0.0)
prp(cartSex,digits = 6)
#men diff
abs(.334176 - .290456)
#women diff
abs(.302795 - .345818)
#who is affected more by the diff ?
abs(.334176 - .290456) - abs(.302795 - .345818)

#3.3
logrSex = glm(family = binomial, data = gerber, voting ~ control + sex)
summary(logrSex)

#3.4
controlSexPossibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
controlSexPossibilities
predict(logrSex, newdata=controlSexPossibilities, type="response")
#1 2 3 4 correspond respectively to 
# (m, not control), (m, control), (f, not control), (f,control)


abs(.290456 - 0.2908065)

#3.5
## add a new variable to gerber representing bitwise and of sex and control (i think?)
logrSex2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(logrSex2)

#3. 6
predict(logrSex2, newdata=controlSexPossibilities, type="response")
#diff again for tree and supplemented log model case for (f, control)
abs(.290456 - 0.2904558)

#3.7
## don't always use all combination variables for log regression models due to possible overfitting
