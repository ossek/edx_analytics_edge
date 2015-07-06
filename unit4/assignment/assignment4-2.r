lettersAPBR = read.csv("./letters_ABPR.csv")
str(lettersAPBR)

#1.1
lettersAPBR$isB = as.factor(lettersAPBR$letter == "B")

library(caTools)
set.seed(1000)
letterSplitter = sample.split(lettersAPBR$isB, SplitRatio = 0.5)
lettersTrain = subset(lettersAPBR,letterSplitter == TRUE)
lettersTest = subset(lettersAPBR,letterSplitter == FALSE)

#baseline which always predicts most common (not B)
# what is accuracy of this baseline?
# number of correctly guessed not B's / everything
nrow(lettersTest[lettersTest$isB == FALSE,])/nrow(lettersTest)

#1.2
isBCart = rpart(method = "class",data = lettersTrain,isB ~ . - letter)
isBCartPredictions = predict(isBCart,newdata = lettersTest, type = "class")
isBCartMatrix = as.matrix(table(lettersTest$isB,isBCartPredictions))
r = isBCartMatrix
r
isBCartAccuracyOnTest = (r[1,1] + r[2,2]) / (r[1,1] + r[2,2] + r[1,2] + r[2,1])
isBCartAccuracyOnTest

#1.3
library(randomForest)
set.seed(1000)
isBForest = randomForest(data = lettersTrain,isB ~ . - letter)
isBForestPredictions = predict(isBForest,newdata = lettersTest)
isBForestMatrix = as.matrix(table(lettersTest$isB,isBForestPredictions))
r = isBForestMatrix
r
isBForestAccuracyOnTest = (r[1,1] + r[2,2]) / (r[1,1] + r[2,2] + r[1,2] + r[2,1])
isBForestAccuracyOnTest

#2.1
lettersAPBR$letter = as.factor(lettersAPBR$letter)
set.seed(2000)
letterSplitter2 = sample.split(lettersAPBR$letter, SplitRatio = 0.5)
lettersTrain2 = subset(lettersAPBR,letterSplitter2 == TRUE)
lettersTest2 = subset(lettersAPBR,letterSplitter2 == FALSE)

#baseline model in multiclass is to predict the most frequent.  what is baseline accuracy on testing set?
#frequencies:
nrow(lettersTest2[lettersTest2$letter == 'A',])/nrow(lettersTest2)
nrow(lettersTest2[lettersTest2$letter == 'P',])/nrow(lettersTest2)
nrow(lettersTest2[lettersTest2$letter == 'B',])/nrow(lettersTest2)
nrow(lettersTest2[lettersTest2$letter == 'R',])/nrow(lettersTest2)
#p is most frequent
# p's guessed over everything, which is also just p frequency

#2.2
#make a cart model and predict accuracy by adding all classes along main diagonal of confusion matrix and divide by total.
# don't use the isB that we added earlier since it's related to what we are trying to predict
whichLetterCart = rpart(method = "class",data = lettersTrain2,letter ~ . - isB)
prp(whichLetterCart,digits = 6)
whichLetterCartPredictions = predict(whichLetterCart,lettersTest2,type = "class")
whichLetterCartPredictions
whichLetterCartMatrix = as.matrix(table(lettersTest2$letter,whichLetterCartPredictions))
whichLetterCartMatrix

whichLetterCartAcc = Reduce(function(summ,rowcolnum) summ + whichLetterCartMatrix[rowcolnum,rowcolnum],
                            seq(nrow(whichLetterCartMatrix)),0) / nrow(lettersTest)
whichLetterCartAcc

#2.3
set.seed(1000)
whichLetterForest = randomForest(data = lettersTrain2,letter ~ . - isB)
whichLetterForestPredictions = predict(whichLetterForest,newdata = lettersTest2)
whichLetterForestMatrix = as.matrix(table(lettersTest2$letter,whichLetterForestPredictions))

whichLetterForestAcc = Reduce(function(summ,rowcolnum) summ + whichLetterForestMatrix[rowcolnum,rowcolnum],
                            seq(nrow(whichLetterForestMatrix)),0) / nrow(lettersTest2)
whichLetterForestAcc