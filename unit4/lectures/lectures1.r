stevens = read.csv("./stevens.csv")
str(stevens)
# 1 for reverse, 0 for affirm
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse,SplitRatio = .7)
spl
stevensTrain = subset(stevens,spl == TRUE)
stevensTest = subset(stevens,spl == FALSE)

install.packages("rpart")
library(rpart)

install.packages("rpart.plot")
library(rpart.plot)

#method class means classification tree (as opposed to regression tree)
stevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent 
                    + LowerCourt + Unconst, data = stevensTrain,method = "class",
                    minbucket = 25)
prp(stevensTree)

#using type class is like using a threshold of .5 . It uses the 'majority class'.
predictCart = predict(stevensTree, newdata = stevensTest,type = "class")
predictCart

confusionMatrix = as.matrix(table(stevensTest$Reverse, predictCart))
confusionMatrix
r = confusionMatrix

#accuracy
(r[1,1] + r[2,2]) / (r[1,1] + r[1,2] + r[2,1] + r[2,2])

#ROC curve and AUC
library(ROCR)
predictROC = predict(stevensTree,newdata = stevensTest)


# this will show us a list of observations, and the percent of outcome 0 and 1 in the subset(ie bucket) that 
#  that observation was in.
#  The second column (percentage of data in the subset that was observation 1), 
#  is what we're interested in for generating ROC curve.
predictROC

pred = prediction(predictROC[,2],stevensTest$Reverse)
pred
# true postive rate and false positive rate
perf = performance(pred,"tpr","fpr")
plot(perf)
auc = as.numeric(performance(pred, "auc")@y.values)

#QQ 4

stevensTreeBucket5 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent 
                    + LowerCourt + Unconst, data = stevensTrain,method = "class",
                    minbucket = 5)
summary(stevensTreeBucket5)
str(stevensTreeBucket5)

#splits = minsplit + 1? #nope
stevensTreeBucket5$control$minsplit

stevensTreeBucket5$variable.importance

prp(stevensTreeBucket5)

stevensTreeBucket100 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent 
                           + LowerCourt + Unconst, data = stevensTrain,method = "class",
                           minbucket = 100)
prp(stevensTreeBucket100)
stevensTreeBucket100$control$minsplit

#Video 5
# Random Forests.
install.packages("randomForest")
library(randomForest)

#The randomForest function does not have a method argument (like rpart's method = "class").
#  So when we want to do a classification problem,
#  we need to make sure outcome (our dependent variable) is a factor.  We will get a warning if we don't.

stevensTrain$Reverse = as.factor(stevensTrain$Reverse)
stevensTest$Reverse = as.factor(stevensTest$Reverse)

stevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent 
                             + LowerCourt + Unconst, data = stevensTrain,nodesize = 25, ntree = 200)
summary(stevensForest)
predictForest = predict(stevensForest,newdata = stevensTest)
confusionMatrixForForest = as.matrix(table(stevensTest$Reverse,predictForest))
# note -- this is not the same as in the video.  Probably because the video did not include resetting the seed?
#   aaaaaand the end of the video says matrix may be different.  The QQ after this video implies the 
#   random forest method contains some random generator code that is tied to system time (maybe).
confusionMatrixForForest
r2 = confusionMatrixForForest
#accuracy
(r2[1,1] + r2[2,2]) / (r2[1,1] + r2[1,2] + r2[2,1] + r2[2,2])

##QQ for video 5
set.seed(200)
stevensForestQQ = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent 
                             + LowerCourt + Unconst, data = stevensTrain,nodesize = 25, ntree = 200)
predictForestQQ = predict(stevensForestQQ,newdata = stevensTest)
confusionMatrixForForestQQ = as.matrix(table(stevensTest$Reverse,predictForestQQ))
# note -- this is not the same as in the video.  Probably because the video did not include resetting the seed?
#   aaaaaand the end of the video says matrix may be different.  The QQ after this video implies the 
#   random forest method contains some random generator code that is tied to system time (maybe).
confusionMatrixForForestQQ
r2Q = confusionMatrixForForestQQ
#accuracy
(r2Q[1,1] + r2Q[2,2]) / (r2Q[1,1] + r2Q[1,2] + r2Q[2,1] + r2Q[2,2])


#video 6
install.packages("caret")
#may need these
#liblapack-dev
#liblapack3
#libopenblas-base
#libopenblas-dev

library(caret)
install.packages("e1071")
library(e1071)
# method cv for cross validation, number=10 for 10 folds
numFolds = trainControl(method="cv",number=10)
# define cp params to test from .01 to .5 in increments of .01
cpGrid = expand.grid(.cp=seq(0.01,0.50,0.01))

#use method rpart repeatedly
train(Reverse ~ Circuit + Issue + Petitioner + Respondent 
      + LowerCourt + Unconst, data = stevensTrain,method="rpart",trControl = numFolds,tuneGrid = cpGrid)

#the output of train includes a table that describes cross validation accuracy for different cp parameters,
#  and gives us a final value
#  we want to use the 'final value' in cart (in this case .17)

stevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent 
                      + LowerCourt + Unconst, data = stevensTrain,method = "class",cp = 0.17)
#now make some predictions
predictCV = predict(stevensTreeCV,newdata = stevensTest,type = "class")
confusionStevensPredictCV = as.matrix(table(stevensTest$Reverse,predictCV))
r3 = confusionStevensPredictCV
#accuracy
(r3[1,1] + r3[2,2]) / (r3[2,2] + r3[1,2] + r3[1,1] + r3[2,1])

prp(stevensTreeCV)
