#QQ 1
-1.5 + 3*1 + -0.5*5
exp(-1)
1/(1+ exp(1))

#video 3-1-4
quality = read.csv("./quality.csv")
str(quality)

table(quality$PoorCare)
#0 good care, 1 is poor care

#A standard baseline method in a classification problem is to 
#  predict the most frequent outcome
nrow(quality[quality$PoorCare == 0,])/nrow(quality)

#A package to separate our dataset into train and test sets
install.packages("caTools")
library(caTools)
#choose 88 as a seed arbirtrarily
set.seed(88)
?sample.split
#splitting on a given label such as poor care is to try to keep the same ratios of 
#  each outcome in each of train and test set.  Doesn't this give us a false sense 
#  of how accurate our model is on a test set since we make assumptions about the test set's structure?
#  sample.split gives us a vector of TRUE / FALSE where we are supposed to use indexes of TRUE 
#  members to identify rows from the dataset to put in Train set
splitty = sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain = subset(quality,splitty == TRUE)
qualityTest = subset(quality,splitty == FALSE)
nrow(qualityTrain)
nrow(qualityTest)

#IF YOU BUILD IT THEY WILL...be able to predict whether care outcomes are good are bad
#  
?glm
# This is a generalized linear model function.  The family param is a family object whose constructor function (??)
#  takes a param describing
#  model link function.  In this case, binomial family has a default link = 'logit', which is what we really want.
#  note that due to pseudo-random selection of our training set, this result can change
qualityLog = glm(formula = PoorCare ~ OfficeVisits + Narcotics,data = qualityTrain,family = binomial)
summary(qualityLog)

?predict
# This type param is dependent on the type of the model passed in being glm
predictTrain = predict(qualityLog, type="response")
summary(predictTrain)

# Compute average prediction for each of the true outcomes.
tapply(predictTrain,qualityTrain$PoorCare, mean)


#QQ 
#set the seed to get same outcome as the QQ
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
qualityLog2 = glm(formula = PoorCare ~ StartedOnCombination + ProviderCount,data = qualityTrain,family = binomial)
summary(qualityLog2)

#3-1-4
# Let's make a confusion matrix, using a threshold of 0,5:
confusionMatrix_t_p5 = table(qualityTrain$PoorCare,predictTrain > 0.5)
confusionMatrix_t_p5
confusionMatrix_t_p5[1,1]
# cols: we predict outcome is 1 (poor care) based on probability of that outcome being > 0.5
# rows: actual outcome
#  FALSE TRUE
#0    TN   FP
#1    FN   TP

#   FALSE TRUE
#0    70    4
#1    15   10

#From this, get the sensitivity and specificity:
#sensitivity = 10/(10 + 15)
sensitivity1 = confusionMatrix_t_p5[2,2]/(confusionMatrix_t_p5[2,2] + confusionMatrix_t_p5[2,1])
sensitivity1
#specificity = 70/(70 + 4)
specificity1 = confusionMatrix_t_p5[1,1]/(confusionMatrix_t_p5[1,1] + confusionMatrix_t_p5[1,2])
specificity1

#let's try with a higher threshold value:
confusionMatrix_t_p7 = table(qualityTrain$PoorCare,predictTrain > 0.7)
sensitivity2 = confusionMatrix_t_p7[2,2]/(confusionMatrix_t_p7[2,2] + confusionMatrix_t_p7[2,1])
sensitivity2
#specificity = 70/(70 + 4)
specificity2 = confusionMatrix_t_p7[1,1]/(confusionMatrix_t_p7[1,1] + confusionMatrix_t_p7[1,2])
specificity2

# and with lower
#let's try with a higher threshold value:
confusionMatrix_t_p7 = table(qualityTrain$PoorCare,predictTrain > 0.7)
sensitivity2 = confusionMatrix_t_p7[2,2]/(confusionMatrix_t_p7[2,2] + confusionMatrix_t_p7[2,1])
sensitivity2
#specificity = 70/(70 + 4)
specificity2 = confusionMatrix_t_p7[1,1]/(confusionMatrix_t_p7[1,1] + confusionMatrix_t_p7[1,2])
specificity2

sensitivity <- function(trainingActuals,predictions,threshold)
{
  confusionMatrix = table(trainingActuals,predictions > threshold)
  confusionMatrix[2,2]/(confusionMatrix[2,2] + confusionMatrix[2,1])
}

specificity <- function(trainingActuals,predictions,threshold)
{
  confusionMatrix = table(trainingActuals,predictions > threshold)
  confusionMatrix[1,1]/(confusionMatrix[1,1] + confusionMatrix[1,2])
}

sensitivity(qualityTrain$PoorCare,predictTrain,0.2)
specificity(qualityTrain$PoorCare,predictTrain,0.2)


#recap:
sensitivity(qualityTrain$PoorCare,predictTrain,0.5)
specificity(qualityTrain$PoorCare,predictTrain,0.5)

sensitivity(qualityTrain$PoorCare,predictTrain,0.7)
specificity(qualityTrain$PoorCare,predictTrain,0.7)

sensitivity(qualityTrain$PoorCare,predictTrain,0.2)
specificity(qualityTrain$PoorCare,predictTrain,0.2)

## QQ
#TP / (TP+FN)
sens = 20 / (20 + 5)
#TN / (TN+FP)
spec = 15 / (15 +10)
sens
spec

#3-1-5 and 3-1-6
## QQ
install.packages("ROCR")
library(ROCR)

#using the original model we built:
#qualityLog = glm(formula = PoorCare ~ OfficeVisits + Narcotics,data = qualityTrain,family = binomial)
#predictTrain = predict(qualityLog, type="response")
predictTest = predict(qualityLog, type="response",newdata = qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
