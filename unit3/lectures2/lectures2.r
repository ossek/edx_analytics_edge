#install.packages("ROCR")
#install.packages("caTools")
library(ROCR)
library(caTools)

framingham = read.csv("./framingham.csv")
str(framingham)

set.seed(1000)
splitty = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
framingTrain = subset(framingham,splitty == TRUE)
framingTest = subset(framingham,splitty == FALSE)

# "." is shorthand for all other variables.  Be carefult with this for cases 
#  where eg there's an identifier such as name as one of the variables
framingLog = glm(TenYearCHD ~ .,data= framingTrain,family = binomial)
summary(framingLog)

predictions = predict(framingLog,type = "response",newdata = framingTest)

threshold = 0.5
table(framingTest$TenYearCHD,predictions > threshold)
#accuracy: (TP+TN)/(TP+TN+FP+FN)
(1069+11)/(1069+11+187+6)

#compare to baseline accuracy.  0 is most frequent here, so
#  sum of most frequent outcomes/all obs.
(1069+6)/(1069+11+187+6)

#we can see if varying the threshold can still give us a more valuable model
#  by computing AUC for the ROC curve
library(ROCR)
ROCpred = prediction(predictions,framingTest$TenYearCHD)
aucOnTestSet = as.numeric(performance(ROCpred,"auc")@y.values)
aucOnTestSet
#this auc (.742) is decent (certainly better than .5 which is the minimum auc from just guessing)

#sensitivity
# TP/(TP+FN)
11/(187+11)

#specificity
1069/(1069+6)

