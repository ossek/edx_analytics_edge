claimsData = read.csv("./ClaimsData.csv")
str(claimsData)

#bucket200* shows which cost bucket (as defined by D2Hawkeye) a patient fell into.  Here are percents
table(claimsData$bucket2009)/nrow(claimsData)

#goal is to predict the cost bucket the patient fell into in 2009
library(caTools)
set.seed(88)
spl = sample.split(claimsData$bucket2009,SplitRatio = .6)
claimsTrain = subset(claimsData,spl == TRUE)
claimsTest = subset(claimsData,spl == FALSE)

##QQ lecture 2 video 6
mean(claimsTrain$age)

nrow(claimsTrain[claimsTrain$diabetes == 1,])/nrow(claimsTrain)

###
## We now want to see how well the baseline method used by D2Hawkeye performs on this data set.
## Base line method would predict that the cost bucket for a patient in 2009 will be same as it was in 2008
table(claimsTest$bucket2009,claimsTest$bucket2008)

#we want to add along the diagonal of this table
bucketMatrix = as.matrix(table(claimsTest$bucket2009,claimsTest$bucket2008))
r = bucketMatrix
baselineAccuracy = Reduce(function(summ,rowcolnum) summ + r[rowcolnum,rowcolnum],seq(nrow(r)),0) / nrow(claimsTest)

# now a penalty matrix
penaltyMatrix = matrix(c(0,1,2,3,4, 2,0,1,2,3, 4,2,0,1,2, 6,4,2,0,1, 8,6,4,2,0),byrow = TRUE, nrow = 5)
penaltyMatrix

# to compute baseline method's error, multiply baseline matrix by  penalty matrix, sum and divide by obs
penaltyError = sum(bucketMatrix * penaltyMatrix)/nrow(claimsTest)
penaltyError

##QQ video 7
# suppose we use base line of predicting most common outcome instead. this predicts costbucket 1 for everyone.

# what is error?
nrow(claimsTest[claimsTest$bucket2009 == 1,]) / nrow(claimsTest)
#or
as.matrix(table(claimsTest$bucket2009))[1,1]/ nrow(claimsTest)

#penalty
sum(table(claimsTest$bucket2009) * penaltyMatrix[,1]) / nrow(claimsTest)

###########
# vido 8
# make a cart model
# the cp value used below was selected via cross validation on the training set
library(rpart)
library(rpart.plot)
claimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd 
                   + depression + diabetes + heart.failure + ihd + kidney + osteoporosis
                   + stroke + bucket2008 + reimbursement2008, data = claimsTrain,method = "class" ,cp = 0.00005)
prp(claimsTree)

#let's make predictions on the test set
predictTest = predict(claimsTree,newdata = claimsTest, type="class")
table(claimsTest$bucket2009,predictTest)
predictMatrix = as.matrix(table(claimsTest$bucket2009,predictTest))
r = predictMatrix
accuracyCartClaims = Reduce(function(summ,rowcolnum) summ + r[rowcolnum,rowcolnum],seq(nrow(r)),0) / nrow(claimsTest)

#penalty matrix
penaltyMatrix * predictMatrix
# penalty sum
sum(penaltyMatrix * predictMatrix)/nrow(claimsTest)

#we increased accuracy over baseline (from last video), but increased penalty also
#  by default rpart tries to maximize overall accuracy and rpart has no concept of varying penalties for different errors.
#  rpart does allow us to specify a penalty matrix.
claimsTree = rpart(bucket2009 ~ age + arthritis + alzheimers + cancer + copd 
                   + depression + diabetes + heart.failure + ihd + kidney + osteoporosis
                   + stroke + bucket2008 + reimbursement2008, data = claimsTrain,method = "class" ,cp = 0.00005,parms=list(loss = penaltyMatrix) )
predictTest = predict(claimsTree,newdata = claimsTest, type="class")
table(claimsTest$bucket2009,predictTest)
predictMatrix = as.matrix(table(claimsTest$bucket2009,predictTest))
r = predictMatrix
accuracyCartClaims = Reduce(function(summ,rowcolnum) summ + r[rowcolnum,rowcolnum],seq(nrow(r)),0) / nrow(claimsTest)

#penalty matrix
penaltyMatrix * predictMatrix
# penalty sum
sum(penaltyMatrix * predictMatrix)/nrow(claimsTest)
## accuracy and penalty are lowe