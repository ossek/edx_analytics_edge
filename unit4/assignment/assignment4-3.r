census = read.csv("./census.csv")
set.seed(2000)
splitter = sample.split(census$over50k,SplitRatio = 0.6)
censusTrain = subset(census,splitter == TRUE)
censusTest = subset(census,splitter == FALSE)

logrModel = glm(family = binomial,data = censusTrain,over50k ~ .)
summary(logrModel)

#1.2
#accuracy of model on testing set
predictions = predict(logrModel,newdata = censusTest,type = "response")
predictionMatrix = as.matrix(table(censusTest$over50k,predictions > 0.5))
predictionMatrix

predictionacc = Reduce(function(summ,rowcolnum) summ + predictionMatrix[rowcolnum,rowcolnum],
                              seq(nrow(predictionMatrix)),0) / nrow(censusTest)
predictionacc

#baseline accuracy for testing set: (where baseline is just taking most common / total)
nrow(censusTest[censusTest$over50k == " <=50K",])/nrow(censusTest)
nrow(censusTest[censusTest$over50k == " >50K",])/nrow(censusTest)

nrow(censusTest[censusTest$over50k == " <=50K",])/nrow(censusTest)

#1.4
library(ROCR)
ROCpred = prediction(predictions,censusTest$over50k)
auc = as.numeric(performance(ROCpred,"auc")@y.values)
auc

#2.1
censusCart = rpart(method = "class",data = censusTrain,over50k ~ .)
prp(censusCart,digits = 6)
censusCartPredictions = predict(censusCart,newdata=censusTest,type = "class") #class arg is like using threshold .5 for cart
length(censusCartPredictions)
length(censusTest$over50k)
length(censusTrain$over50k)
censusCartPredictionsMatrix = as.matrix(table(censusTest$over50k,censusCartPredictions))
censusCartPredictionsMatrix

censusCartAcc = Reduce(function(summ,rowcolnum) summ + censusCartPredictionsMatrix[rowcolnum,rowcolnum],
                            seq(nrow(censusCartPredictionsMatrix)),0) / nrow(censusTest)
censusCartAcc

#Let us now consider the ROC curve and AUC for the CART model on the test set. You will need to get predicted probabilities 
#for the observations in the test set to build the ROC curve and compute the AUC. 
#Remember that you can do this by removing the type="class" argument when making predictions, and taking the second column of the resulting object.
censusCartPredictionProbs = predict(censusCart,newdata=censusTest)
censusCartPredictionProbs
ROCpred = prediction(censusCartPredictionProbs[,2],censusTest$over50k)
plot(ROCpred)
performance(ROCpred,"auc")
auc = as.numeric(performance(ROCpred,"auc")@y.values)
auc

#3.1
set.seed(1)
smallCensusTrain = censusTrain[sample(nrow(censusTrain), 2000), ]

set.seet(1)
censusForest = randomForest(data = smallCensusTrain,over50k ~ .)
#Remember that you don't need a "type" argument when making predictions with a random forest model if you want to use a threshold of 0.5
censusForestPredictions = 