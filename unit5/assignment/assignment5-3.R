emails = read.csv("./emails.csv",stringsAsFactors=FALSE)
str(emails)

#1.1
nrow(emails)

#1.2
nrow(emails[emails$spam == 1,])

#1.3

#1.4
#subject may appear multiple times (message contains a chain of emails, e.g.)

#1.5
max(nchar(emails$text))

#1.6
emails[nchar(emails$text) == min(nchar(emails$text)),]

#2.1
corpus = Corpus(VectorSource(emails$text))


basicFiltrins <- function(corpus)
{
  transformed = tm_map(corpus,tolower)
  transformed = tm_map(transformed, PlainTextDocument)
  transformed = tm_map(transformed,removePunctuation)
  transformed = tm_map(transformed,removeWords,c(stopwords("english")))
  tm_map(transformed,stemDocument)
}
corpus = basicFiltrins(corpus)

# a dtm is a matrix of document terms
dtm = DocumentTermMatrix(corpus)

#2.2
spdtm = removeSparseTerms(dtm,0.95)

#2.3
emailsSparse = as.data.frame(as.matrix(spdtm),row.names = FALSE)
colnames(emailsSparse) = make.names(colnames(emailsSparse))

sort(colSums(emailsSparse))[ncol(emailsSparse)]

#2.4
emailsSparse$spam = emails$spam
sort(colSums(emailsSparse))

colSumsFromEmailsSparse = colSums(emailsSparse[emailsSparse$spam == 0,])
length(colSumsFromEmailsSparse[colSumsFromEmailsSparse >= 5000])

#2.5
colSumsFromEmailsSparse = colSums(emailsSparse[emailsSparse$spam == 1,])

# this needs - 1 because it includes the word "spam" summed
length(colSumsFromEmailsSparse[colSumsFromEmailsSparse >= 1000])

#2.6

#2.7

#3.1
emailsSparse$spam = as.factor(emailsSparse$spam)

#now try each of the three models (logistic regression, CART, random forest), predicting on the TRAIN set.
# we want probabilities in this case from CART and Random Forest, so
# for CART, don't give "type" arg in rpart, and in random forest give arg 'type = "prob"'

set.seed(123)
splitter = sample.split(SplitRatio = .7,emailsSparse$spam)
emailSparseTrain = subset(emailsSparse,splitter == TRUE)
emailSparseTest = subset(emailsSparse,splitter == FALSE)

logrModel = glm(family = binomial,data = emailSparseTrain,spam ~ .)
cartModel = rpart(data = emailSparseTrain,spam ~ .)
library(randomForest)
set.seed(123)
randoModel = randomForest(data = emailSparseTrain,spam ~ .)

logrPred = predict(logrModel,type="response")
length(logrPred[logrPred < .00001])

length(logrPred[logrPred > 0.99999])

length(logrPred[logrPred < 0.99999 & logrPred > .00001 ])

#3.2
summary(logrModel)

#3.3
prp(cartModel)

#3.4
cmtLogr = as.matrix(table(emailSparseTrain$spam,logrPred > 0.5)) 
predictionAccuracy <- function(confusionMatrix,nrowOrig)
{
  Reduce(function(summ,rowcolnum) summ + confusionMatrix[rowcolnum,rowcolnum],
         seq(nrow(confusionMatrix)),0) / nrowOrig  
}
predictionAccuracy(cmtLogr,nrow(emailSparseTrain))

#3.5
library(ROCR)
#since I predicted with type class earlier this won't work -- get some probabilities instead
ROCpred = prediction(logrPred,emailSparseTrain$spam)
aucOnTestSet = as.numeric(performance(ROCpred,"auc")@y.values)
aucOnTestSet

#3.6
cartPred = predict(cartModel)
cmtCart = as.matrix(table(emailSparseTrain$spam,cartPred[,2] > 0.5)) 
predictionAccuracy(cmtCart,nrow(emailSparseTrain))

#3.7
ROCpred = prediction(cartPred[,2],emailSparseTrain$spam)
aucOnTestSet = as.numeric(performance(ROCpred,"auc")@y.values)
aucOnTestSet

#3.8
rfPred = predict(randoModel,type = "prob")
cmtRf = as.matrix(table(emailSparseTrain$spam,rfPred[,2] > 0.5)) 
predictionAccuracy(cmtRf,nrow(emailSparseTrain))

ROCpred = prediction(rfPred[,2],emailSparseTrain$spam)
aucOnTestSet = as.numeric(performance(ROCpred,"auc")@y.values)
aucOnTestSet

#3.9
#4.1
cmtLogr = as.matrix(table(emailSparseTrain$spam,logrPred > 0.5)) 