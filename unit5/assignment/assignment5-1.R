wiki = read.csv("./wiki.csv")
str(wiki)
wiki$Vandal = as.factor(wiki$Vandal)

#1.1
nrow(wiki[wiki$Vandal == 1,])

#1.2
#The text already is lowercase and stripped of punctuation. So to pre-process the data, just complete the following four steps:
#  
#  1) Create the corpus for the Added column, and call it "corpusAdded".
#
#2) Remove the English-language stopwords.
#
#3) Stem the words.
#
#4) Build the DocumentTermMatrix, and call it dtmAdded.
library(tm)
library(SnowballC)

# a corpus is a collection of documents
corpusAdded = Corpus(VectorSource(wiki$Added))
#corpusAdded = tm_map(corpus, PlainTextDocument)
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded,stemDocument)
# a dtm is a matrix of document terms
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

#1.3
# only keep terms that appear in .003 of revisions
sparseAdded = removeSparseTerms(dtmAdded,0.997)
sparseAdded

#1.4
wordsAdded = as.data.frame(as.matrix(sparseAdded))
wordsAdded[1:5,]
#prepend A to the colnames
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
wordsAdded[1:5,]

#Same thing we did for Added words for Removed (but prepend R instead of A)

corpusRemoved = Corpus(VectorSource(wiki$Removed))
#corpusRemoved = tm_map(corpus, PlainTextDocument)
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved,stemDocument)
# a dtm is a matrix of document terms
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

#1.3
# only keep terms that appear in .003 of revisions
sparseRemoved = removeSparseTerms(dtmRemoved,0.997)
sparseRemoved

#1.4
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
#prepend A to the colnames
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
ncol(wordsRemoved)

#1.5
wikiWords = cbind(wordsAdded, wordsRemoved)
# add Vandal column
wikiWords$Vandal = wiki$Vandal
library(caTools)
set.seed(123)
split = sample.split(SplitRatio = .7,wikiWords$Vandal)
wikiWordsTrain = subset(wikiWords,split == TRUE)
wikiWordsTest = subset(wikiWords,split == FALSE)

#accuracy of baseline (always predict most frequent outcome of not vandalism)
as.matrix(table(wikiWords$Vandal))[1,1]/nrow(wikiWords)

#1.6

library(rpart)
library(rpart.plot)

wikiCart = rpart(data = wikiWordsTrain,method = "class",Vandal ~ .)
prp(wikiCart)

predictCart = predict(wikiCart, newdata = wikiWordsTest, type = "class")
predictionMatrix = as.matrix(table(wikiWordsTest$Vandal,predictCart))
predictionAccuracy(predictionMatrix,nrow(wikiWordsTest))

predictionAccuracy <- function(confusionMatrix,nrowOrig)
{
  Reduce(function(summ,rowcolnum) summ + confusionMatrix[rowcolnum,rowcolnum],
                         seq(nrow(confusionMatrix)),0) / nrowOrig  
}  

#2.1
wikiWords2 = wikiWords
wikiWords2$http = ifelse(grepl("http",wiki$Added,fixed = TRUE),1,0)
nrow(wikiWords2[wikiWords2$http == 1,])

#2.2
wikiTrain2 = subset(wikiWords2,split == TRUE)
wikiTest2 = subset(wikiWords2,split == FALSE)
wikiCart2 = rpart(data = wikiTrain2,method = "class",Vandal ~ .)
prp(wikiCart2)

predictCart2 = predict(wikiCart2, newdata = wikiTest2, type = "class")
predictionMatrix2 = as.matrix(table(wikiTest2$Vandal,predictCart2))
predictionAccuracy(predictionMatrix2,nrow(wikiTest2))

#2.3
#noting that in matrix form we get a sort of table of presence / absence of terms
as.matrix(dtmAdded)[2,1:10]
as.matrix(dtmAdded)[3,1:10]
wikiWords2$numWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$numWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$numWordsAdded)
#2.4
wikiTrain3 = subset(wikiWords2,split == TRUE)
wikiTest3 = subset(wikiWords2,split == FALSE)
wikiCart3 = rpart(data = wikiTrain3,method = "class",Vandal ~ .)


cartPredictionAccuracy <- function(cart,test,depVariable)
{
  predictions = predict(cart,newdata = test,type= "class")
  predictionMatrix = as.matrix(table(test[,depVariable],predictions))
  predictionAccuracy(predictionMatrix,nrow(test))
}
cartPredictionAccuracy(wikiCart3,wikiTest3,"Vandal")

#3.1
wikiWords3 = wikiWords2
wikiWords3$minor = wiki$Minor
wikiWords3$loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3,split == TRUE)
wikiTest4 = subset(wikiWords3,split == FALSE)
wikiCart4 = rpart(data = wikiTrain4,method = "class",Vandal ~ .)
cartPredictionAccuracy(wikiCart4,wikiTest4,"Vandal")

#3.2
prp(wikiCart4)
