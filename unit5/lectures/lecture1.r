twit = read.csv("./tweets.csv",stringsAsFactors = FALSE)
str(twit)
twit$negative = as.factor(twit$Avg <= -1)

table(twit$negative)
#install.packages("tm")
library(tm)
#install.packages("SnowballC")
library(SnowballC)

# a corpus is a collection of documents
corpus = Corpus(VectorSource(twit$Tweet))
corpus
corpus[[1]]$content

#this somehow loses a property on the corpus[[1]] object,
# The PlainTextDocument conversion is essential in the most recent version of tm.
#corpus = tm_map(corpus, PlainTextDocument)
# and apparently we have to do the PlainTextDocument conversion here and nowhere else
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus
corpus[[1]]

corpus = tm_map(corpus,removePunctuation)
corpus[[1]]

length(stopwords("english")) 
corpus = tm_map(corpus,removeWords,c("apple",stopwords("english")))
corpus[[1]]

corpus = tm_map(corpus,stemDocument)
corpus[[1]]

#Video 6
# tm package provides a function DocumentTermMatrix
# rows are documents (in our case tweets) and columns correspond to words in those tweets


frequencies = DocumentTermMatrix(corpus)
frequencies
#video gives us cheap cheapen at 505:515??
#inspect(frequencies[1000:1005,570:590])
inspect(frequencies[1000:1005,570:590])

findFreqTerms(frequencies,lowfreq = 20)
findFreqTerms(frequencies,lowfreq = 100)

# only keep terms that appear in .005 of tweets
sparse = removeSparseTerms(frequencies,0.995)
sparse

twitSparse = as.data.frame(as.matrix(sparse))
#we probably will have some column names that would be numbers so let's make them ok for R
colnames(twitSparse) = make.names(colnames(twitSparse))
twitSparse$Negative = twit$negative
#install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(twitSparse$Negative,SplitRatio = 0.7)
trainSparse = subset(twitSparse, split == TRUE)
testSparse = subset(twitSparse, split == FALSE)

#Video 7
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

tweetCart = rpart(data = trainSparse,method = "class",Negative ~ .)
prp(tweetCart)

predictCart = predict(tweetCart, newdata = testSparse, type = "class")

predictionMatrix = as.matrix(table(testSparse$Negative,predictCart))
predictionMatrix
predictionacc = Reduce(function(summ,rowcolnum) summ + predictionMatrix[rowcolnum,rowcolnum],
                       seq(nrow(predictionMatrix)),0) / nrow(testSparse)
predictionacc

#compare to a simple baseline that always predicts nonnegative (the most frequent)
table(testSparse$Negative)
#300 correct nonneg predictions out of 355 total
baselineAcc = 300/355

# now let's try a random forest model
install.packages("randomForest")
library(randomForest)
set.seed(123)
# this will take a long time -- we have over 300 independent variables
twitRF = randomForest(data = trainSparse,Negative ~ .)
predictRF = predict(twitRF,newdata = testSparse)

predictionRFMatrix = as.matrix(table(testSparse$Negative,predictRF))
predictionRFMatrix
predictionRFacc = Reduce(function(summ,rowcolnum) summ + predictionRFMatrix[rowcolnum,rowcolnum],
                       seq(nrow(predictionRFMatrix)),0) / nrow(testSparse)
predictionRFacc

#QQ Video 7
logrModel = glm(family = binomial,data = trainSparse,Negative ~ .)
summary(logrModel)
predictionsLogr = predict(logrModel, newdata=testSparse, type="response")

predictionsLogrMatrix = as.matrix(table(testSparse$Negative,predictionsLogr > 0.5))
predictionsLogrMatrix
predictionsLogracc = Reduce(function(summ,rowcolnum) summ + predictionsLogrMatrix[rowcolnum,rowcolnum],
                         seq(nrow(predictionsLogrMatrix)),0) / nrow(testSparse)
predictionsLogracc