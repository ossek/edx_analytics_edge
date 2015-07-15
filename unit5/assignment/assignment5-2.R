clinical = read.csv("./clinical_trial.csv",fileEncoding="latin1",stringsAsFactors=FALSE )
str(clinical)

#1.1
max(nchar(clinical$abstract))

#1.2
nrow(clinical[nchar(clinical$abstract) == 0,])

#1.3
clinical[nchar(clinical$title) == min(nchar(clinical$title)),c("title")]

#2.1
corpusTitle = Corpus(VectorSource(clinical$title))

basicFiltrins <- function(corpus)
{
  transformed = tm_map(corpus,tolower)
  transformed = tm_map(transformed, PlainTextDocument)
  transformed = tm_map(transformed,removePunctuation)
  transformed = tm_map(transformed,removeWords,c(stopwords("english")))
  tm_map(transformed,stemDocument)
}
corpusTitle = basicFiltrins(corpusTitle)

corpusAbstract = Corpus(VectorSource(clinical$abstract))
corpusAbstract = basicFiltrins(corpusAbstract)
# a dtm is a matrix of document terms
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract =DocumentTermMatrix(corpusAbstract)

# only keep terms that appear in .05 of revisions
dtmAbstract = removeSparseTerms(dtmAbstract,0.95)
dtmTitle = removeSparseTerms(dtmTitle,0.95)

dtmAbstract = as.data.frame(as.matrix(dtmAbstract),row.names = FALSE)
dtmTitle = as.data.frame(as.matrix(dtmTitle),row.names = FALSE)

dtmTitle[1,1:10]
dtmAbstract[1,1:10]
ncol(dtmTitle)
ncol(dtmAbstract)

#2.2
sort(colSums(dtmAbstract))[ncol(dtmAbstract)]

#3.1
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

#3.2
#get rid of row names??
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = clinical$trial
ncol(dtm)

#3.3
#baseline (predict most frequent)
str(clinical)
nrow(dtm[dtm$trial == 1,])

#this is more frequent
nrow(dtm[dtm$trial == 0,])

library(caTools)
set.seed(144)
splitty = sample.split(SplitRatio = .7,dtm$trial)
dtmTrain = subset(dtm,splitty == TRUE)
dtmTest = subset(dtm,splitty == FALSE)

as.matrix(table(dtmTest$trial))[1,1]/nrow(dtmTest)

#3.4
trialCart = rpart(data = dtmTrain,method = "class",trial ~ .)
prp(trialCart)

#Obtain the training set predictions for the model (do not yet predict on the test set). 
#Extract the predicted probability of a result being a trial (recall that this involves not setting 
#a type argument, and keeping only the second column of the predict output). What is the maximum predicted probability for any result?
predictTrial = predict(trialCart)
max(predictTrial[,2])

#3.6
#Because the CART tree assigns the same predicted probability to each leaf node and there are a small 
#number of leaf nodes compared to data points, we expect exactly the same maximum predicted probability (as max from training predictions)

#3.7
confusionMatrixTrain = as.matrix(table(dtmTrain$trial,predictTrial[,2]  > 0.5)) 
cmtrain = confusionMatrixTrain
#sens is TP / (TP + FN)
sens = cmtrain[2,2] / (cmtrain[2,2] + cmtrain[2,1])

#specificity TN/(TN+FP)
spec = cmtrain[1,1] / (cmtrain[1,1] + cmtrain[1,2])
predictionAccuracy(cmtrain,nrow(dtmTrain))
sens
spec

#4.1
predictions = predict(trialCart,type="class",newdata = dtmTest)
confusionMatrix = table(predictions,dtmTest$trial)
confusionMatrix 
confusionMatrixM = as.matrix(confusionMatrix) 
c = confusionMatrixM

#sens (TP rate) ( positives guessed correctly / all positives )
# TP / TP + FP
sens = c[2,2] / (c[2,2] + c[1,2])

#spec (TN rate) (negatives guessed correctly / all negs)
# TN / TN + FN
spec = c[1,1] / (c[1,1] + c[2,1])

predictionAccuracy <- function(confusionMatrix,nrowOrig)
{
  Reduce(function(summ,rowcolnum) summ + confusionMatrix[rowcolnum,rowcolnum],
         seq(nrow(confusionMatrix)),0) / nrowOrig  
}

acc = predictionAccuracy(confusionMatrixM,nrow(dtmTest))
acc
sens
spec

#4.2
library(ROCR)
#since I predicted with type class earlier this won't work -- get some probabilities instead
predictionProbs = predict(trialCart,newdata = dtmTest)[,2]
ROCpred = prediction(predictionProbs,dtmTest$trial)
aucOnTestSet = as.numeric(performance(ROCpred,"auc")@y.values)
aucOnTestSet