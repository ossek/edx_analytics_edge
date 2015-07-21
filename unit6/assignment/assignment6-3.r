stocks = read.csv("./StocksCluster.csv")
#1.1
str(stocks)

#1.2
nrow(stocks[stocks$PositiveDec == 1,])/nrow(stocks)

#1.3
cor(stocks[,c("ReturnJan")],stocks$ReturnFeb)
colnames(stocks)


showCors <- function(df)
{
  cols <- colnames(df)
  inner <- function(col1)
  {
    correr <- function(col2)
    {
      #print(paste("col2",col2))
      c( col1, col2, cor( df[,c(col1)], df[,c(col2)] ))
    }
    lapply(cols,correr)
  }
  lapply(cols,inner)
}

cors = showCors(stocks)
corsFlat = unlist(cors,recursive = FALSE)
unique(corsFlat)

rbinded = unique(Reduce(rbind,corsFlat))
corFrame = as.data.frame(rbinded,row.names = seq(nrow(rbinded)),stringsAsFactors = FALSE)
colnames(corFrame) = c("month1","month2","correlation")
str(corFrame)
corFrame$correlation = as.numeric(corFrame$correlation)

#take out self correlations
corFrameNoSelf = corFrame[corFrame$correlation != 1,]

corFrameNoSelf[corFrameNoSelf$correlation == max(corFrameNoSelf$correlation),]

#1.4
max(colMeans(stocks[,seq(ncol(stocks) - 1)]))

min(colMeans(stocks[,seq(ncol(stocks) - 1)]))

#2.1
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
stocksLogModel = glm(family = binomial,data = stocksTrain,PositiveDec ~ .)

logPredictions = predict(stocksLogModel,type = "response")
confusionMatrix = as.matrix(table(stocksTrain$PositiveDec,logPredictions > 0.5))

predictionAccuracy <- function(confusionMatrix,nrowOrig)
{
  Reduce(function(summ,rowcolnum) summ + confusionMatrix[rowcolnum,rowcolnum],
         seq(nrow(confusionMatrix)),0) / nrowOrig  
}

predictionAccuracy(confusionMatrix,nrow(stocksTrain))

#2.2
logPredictionsTest = predict(stocksLogModel,type="response",newdata = stocksTest)
confusionMatrixTest = as.matrix(table(stocksTest$PositiveDec,logPredictionsTest > 0.5))
predictionAccuracy(confusionMatrixTest,nrow(stocksTest))

#2.3
#baseline, predicting most common outcome of positive
nrow(stocksTest[stocksTest$PositiveDec == 1,])/nrow(stocksTest)

#3.1
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#3.2
#In cases where we have a training and testing set, we'll want to normalize by the mean and standard deviation of the variables in the training set
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)
#. Since normTest was constructed by subtracting by the mean ReturnJan value from the training set, this explains why the mean value of ReturnJan is slightly negative in normTest.

#3.3
set.seed(144)
km = kmeans(normTrain,centers = 3)
stocksClusters2 = (lapply(seq(3),function(grpIdx) subset(normTrain,km[1]$cluster == grpIdx)))
lapply(seq(3),function(grpIdx) c(grpIdx,nrow(stocksClusters2[[grpIdx]])))

#3.5
install.packages("flexclust")
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

Reduce(function(last,cur) last + cur, lapply(clusterTest,function(clustNum) clustNum == 2))
#alt
table(clusterTest)

#4.1
#(be careful to take subsets of stocksTrain, not of normTrain).
stocksTrain1 = subset(stocksTrain,clusterTrain == 1)
stocksTrain2 = subset(stocksTrain,clusterTrain == 2)
stocksTrain3 = subset(stocksTrain,clusterTrain == 3)

stocksTest1 = subset(stocksTest,clusterTest == 1)
stocksTest2 = subset(stocksTest,clusterTest == 2)
stocksTest3 = subset(stocksTest,clusterTest == 3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

#4.2
stocksModel1 = glm(family = binomial,data = stocksTrain1,PositiveDec ~ .)
stocksModel2 = glm(family = binomial,data = stocksTrain2,PositiveDec ~ .)
stocksModel3 = glm(family = binomial,data = stocksTrain3,PositiveDec ~ .)

summary(stocksModel1)
summary(stocksModel2)
summary(stocksModel3)

#4.3
predictTest1 = predict(type="response",newdata = stocksTest1,stocksModel1)
predictTest2 = predict(type="response",newdata = stocksTest2,stocksModel2)
predictTest3 = predict(type="response",newdata = stocksTest3,stocksModel3)

confusion1 = as.matrix(table(stocksTest1$PositiveDec,predictTest1 > 0.5))
confusion2 = as.matrix(table(stocksTest2$PositiveDec,predictTest2 > 0.5))
confusion3 = as.matrix(table(stocksTest3$PositiveDec,predictTest3 > 0.5))

predictionAccuracy(confusion1,nrow(stocksTest1))
predictionAccuracy(confusion2,nrow(stocksTest2))
predictionAccuracy(confusion3,nrow(stocksTest3))

#4.4

