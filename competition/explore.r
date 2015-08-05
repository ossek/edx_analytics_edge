# Cory's good habit
#for auc
library(ROCR)
library(randomForest)
#normalizing and clustering
library(caret)
# for text mining
library(tm)
# for stemming
library(SnowballC)
# for cart
library(rpart)
library(rpart.plot)

#Data fields
#
#The dependent variable in this problem is the variable sold, which labels if an iPad listed on the eBay site was sold (equal to 1 if it did, and 0 if it did not). The dependent variable is provided in the training data set, but not the testing dataset. This is an important difference from what you are used to - you will not be able to see how well your model does on the test set until you make a submission on Kaggle.
#
#The independent variables consist of 9 pieces of product data available at the time the iPad listing is posted, and a unique identifier:
#
#description = The text description of the product provided by the seller.
#biddable = Whether this is an auction (biddable=1) or a sale with a fixed price (biddable=0).
#startprice = The start price (in US Dollars) for the auction (if biddable=1) or the sale price (if biddable=0).
#condition = The condition of the product (new, used, etc.)
#cellular = Whether the iPad has cellular connectivity (cellular=1) or not (cellular=0).
#carrier = The cellular carrier for which the iPad is equipped (if cellular=1); listed as "None" if cellular=0.
#color = The color of the iPad.
#storage = The iPad's storage capacity (in gigabytes).
#productline = The name of the product being sold.o


# how do we make a model that accounts for text analytics as well as the other independent variables?
# need to create a dtm to find most significant words then tack this dtm onto the existing dataframe.

#start out by doing some text pre processing steps (my suspicion is that, doing the typical preprocessing steps 
# we have been doing, this shouldn't influence the end result a ton.

#to start out with, we will make a dtm, and then generate predictive models with 
# logistic regression
  # here we will have room to set predictive threshold
# CART / decision tree
  # here we will have room to set predictive threshold also
# random forest
  # i can't remember if you can set predictive threshold with this one
# lastly try some clustering and apply models on clusters (??)

# we will be able to compare the output of each of these with each other, but not with correct answer.

# we can also compare accuracy of the model on the training set itself
  # is testing set accuracy in any way a function of training set accuracy?

#variables that could be changed to begin with: 
  # probably most importantly would be the independent text variables.  we want the simplest / most general models possible with 
  # the least number of independent variables (more variables would mean more potential overfitting, right?)

  # threshold values when making category predictions
   # probably start with the default of 0.5.  We want accurate predictions, and are not as concerned about penalties for predicting falsely etc.
   # so probably don't change these much.

#first let's do the text preprocessing:
basicFiltrins <- function(corpus)
{
  transformed = tm_map(corpus,tolower)
  transformed = tm_map(transformed, PlainTextDocument)
  transformed = tm_map(transformed,removePunctuation)
  transformed = tm_map(transformed,removeWords,c(stopwords("english")))
  tm_map(transformed,stemDocument)
}
iconvlist()

# magic encoding param from stackoverflow
ebayTrain = read.csv("./eBayiPadTrain.csv",stringsAsFactors = FALSE,fileEncoding="latin1")
#ebayTrain = read.csv("./eBayiPadTrain.csv",stringsAsFactors = FALSE,encoding="UTF-32")
ebayTest = read.csv("./eBayiPadTest.csv",stringsAsFactors = FALSE,fileEncoding="latin1")
str(ebayTrain)
#relabel / relevel
levels(as.factor(ebayTrain$biddable))
length(levels(as.factor(ebayTrain$condition)))
levels(as.factor(ebayTrain$cellular))
length(levels(as.factor(ebayTrain$carrier)))
length(levels(as.factor(ebayTrain$color)))
length(levels(as.factor(ebayTrain$storage)))
length(levels(as.factor(ebayTrain$productline)))

length(levels(as.factor(ebayTest$condition)))
levels(as.factor(ebayTest$cellular))
length(levels(as.factor(ebayTest$carrier)))
length(levels(as.factor(ebayTest$color)))
length(levels(as.factor(ebayTest$storage)))
length(levels(as.factor(ebayTest$productline)))

ebayTrain$condition = as.factor(ebayTrain$condition)
ebayTest$condition <- factor(ebayTest$condition,levels=levels(ebayTrain$condition))

ebayTrain$carrier = as.factor(ebayTrain$carrier)
ebayTest$carrier <- factor(ebayTest$carrier,levels=levels(ebayTrain$carrier))

ebayTrain$color = as.factor(ebayTrain$color)
ebayTest$color <- factor(ebayTest$color,levels=levels(ebayTrain$color))

ebayTrain$storage = as.factor(ebayTrain$storage)
ebayTest$storage <- factor(ebayTest$storage,levels=levels(ebayTrain$storage))

ebayTrain$productline = as.factor(ebayTrain$productline)
ebayTest$productline <- factor(ebayTest$productline,levels=levels(ebayTrain$productline))

ebayTrain$cellular = as.factor(ebayTrain$cellular)
ebayTest$cellular <- factor(ebayTest$cellular,levels=levels(ebayTrain$cellular))

ebayTrain$biddable = as.factor(ebayTrain$biddable)
ebayTest$biddable <- factor(ebayTest$biddable,levels=levels(ebayTrain$biddable))

ebayTrain$sold = as.factor(ebayTrain$sold)
ebayTest$sold <- factor(ebayTest$sold,levels=levels(ebayTrain$sold))


str(ebayTrain)
str(ebayTest)
#############re label re level

dtmFromDescription = function(ebayFrame)
{
  corpus = Corpus(VectorSource(ebayFrame$description))
  corpus = basicFiltrins(corpus)
  
  # a dtm is a matrix of document terms
  dtm = DocumentTermMatrix(corpus)
  
  # remove terms that have at least 0.99 documents (rows) for which the term appears 0 times
  spdtm = removeSparseTerms(dtm,0.99)
  ebayDescriptionSparse = as.data.frame(as.matrix(spdtm),row.names = FALSE)
  colnames(ebayDescriptionSparse) = make.names(colnames(ebayDescriptionSparse))
  colnames(ebayDescriptionSparse)
  
  #tack this sucker back onto the original with a join
  cbind(ebayFrame,ebayDescriptionSparse)  
}

############################# Making text dtm for train
corpus = Corpus(VectorSource(ebayTrain$description))
corpus = basicFiltrins(corpus)

# a dtm is a matrix of document terms
dtm = DocumentTermMatrix(corpus)
#dtm$dimnames

# remove terms that have at least 0.99 documents (rows) for which the term appears 0 times
spdtm = removeSparseTerms(dtm,0.96)
ebayDescriptionSparse = as.data.frame(as.matrix(spdtm),row.names = FALSE)
colnames(ebayDescriptionSparse) = make.names(colnames(ebayDescriptionSparse))
colnames(ebayDescriptionSparse)

nrow(ebayDescriptionSparse)
nrow(ebayTrain)

#tack this sucker back onto the original with a join

#########################  Making text dtm
######################## Making text dtm for test
corpusTest = Corpus(VectorSource(ebayTest$description))
corpusTest = basicFiltrins(corpusTest)

# a dtm is a matrix of document terms
dtmTest = DocumentTermMatrix(corpusTest)
#dtm$dimnames

# remove terms that have at least 0.99 documents (rows) for which the term appears 0 times
spdtmTest = removeSparseTerms(dtmTest,0.96)
ebayDescriptionSparseTest = as.data.frame(as.matrix(spdtmTest),row.names = FALSE)
colnames(ebayDescriptionSparseTest) = make.names(colnames(ebayDescriptionSparseTest))

nrow(ebayDescriptionSparse)
nrow(ebayTrain)


################### Making text dtm for test
##### Final dfs

termsOnlySigInTest = setdiff(colnames(ebayDescriptionSparseTest),colnames(ebayDescriptionSparse))
termsOnlySigInTrain = setdiff(colnames(ebayDescriptionSparse),colnames(ebayDescriptionSparseTest))
termsToKeepInTest = ebayDescriptionSparseTest[,setdiff(colnames(ebayDescriptionSparseTest),termsOnlySigInTest)]
termsToKeepInTrain = ebayDescriptionSparse[,setdiff(colnames(ebayDescriptionSparse),termsOnlySigInTrain)]
#tack this sucker back onto the original with a join
ebayTrainPlusDescriptionTM = cbind(ebayTrain,termsToKeepInTrain)
ebayTestPlusDescriptionTM = cbind(ebayTest,termsToKeepInTest)
str(ebayTrainPlusDescriptionTM)
str(ebayTestPlusDescriptionTM)
######

#how about a first model, predicting on everything else (except description
glmOnAll = glm(family = binomial,data = ebayTrainPlusDescriptionTM, sold ~ . - description - UniqueID)
summary(glmOnAll)
# seems like text variable should be more siginificant?  maybe since they are low number we need to normalize the data?
# then again biddable is only a boolean and it is much more significant

# based on the summary not all variables are even meaningful at all.  how about a more limited set:
str(ebayTrainPlusDescriptionTM)
#warning messages... looks like overfitting?  There be goblins here.
glmRefined = glm(family = binomial,data = ebayTrainPlusDescriptionTM, 
                 sold ~ biddable + startprice + condition + storage + productline + ipad)
summary(glmRefined)

glmRefinedPredictions = predict(glmRefined, newdata = ebayTestPlusDescriptionTM,type = "response")
glmRefinedPredictSold = glmRefinedPredictions < 0.5
glmRefinedResults = data.frame(UniqueID = ebayTest$UniqueID, Probability1 = glmRefinedPredictions)
write.csv(glmRefinedResults, "submission_glmRefinedResults.csv", row.names=FALSE)
# this submission gives AUC of 0.83519, ranked 915 at the time of submission


#how about a CART model predicting on everything else (with default params)?
#cartOnAll = rpart(method = "class",data = ebayTrainPlusDescriptionTM,sold ~ .)
#cartPredictionsTrain = predict(cartOnAll, type = "class")
#cartPredictionsTest = predict(cartOnAll, newdata = ebayTestPlusDescriptionTM, type = "class")

# lets skip straight to random forests
ebayAllForest = randomForest(data = ebayTrainPlusDescriptionTM,sold ~ . - description - UniqueID)
summary(ebayAllForest)

rfPred = predict(ebayAllForest,type = "prob", newdata = ebayTestPlusDescriptionTM)
rfPred[,2]
ebayForestResults = data.frame(UniqueID = ebayTest$UniqueID, Probability1 = rfPred[,2])
write.csv(ebayForestResults, "submission_ebayAllForest.csv", row.names=FALSE)
# this submission gives AUC of 0.85536, ranked 38th at time of submission

# how to exclude insignificant factor variables?
# try measuring correlation as well

#we're gonna need to figure out how to get only the column names that were used from the dtm 
#  tacked onto the training set for the test set.  possibly with the stemming function or else 
#  just subsetting 'where column in..'
#  alternatively just use the ones that appear to be significant from the model


# how about with some k fold cross validation to find the right number of folds and cp value?

