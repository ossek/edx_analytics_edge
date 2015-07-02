parole = read.csv("./parole.csv")

#1.1
nrow(parole)
str(parole)

#1.2
nrow(parole[parole$violator == 1,])

#2.1
levels(as.factor(parole$crime))
levels(as.factor(parole$crime))

getLevels <- function(frame)
{
  cols = colnames(parole)
  lapply(cols,function(col) c(col,levels(as.factor(frame[,col])))) 
}
getLevels(parole)
# the correct answer to the question 'which of these factors are ordered with > 3 values
#  is state, crime.  I'm not sure what excludes age, time served, or max.sentence other than that
#  we can intuit that they may be continous values.

#2.2
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
str(parole)

summary(parole$crime)
summary(parole$time.served)

#3.1
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
paroleTrain = subset(parole, split == TRUE)
paroleTest = subset(parole, split == FALSE)

#3.2

#4.1
paroleViolatorModel = glm(violator ~ .,data = paroleTrain,family = binomial)
summary(paroleViolatorModel)

#4.2
exp(1.61)

#4.3 IMPORTANT NOTE -- use all places of precision in coefficients
#Consider a parolee who is male, of white race, aged 50 years at prison release, 
#from the state of Maryland, served 3 months, had a maximum sentence of 12 months, 
#did not commit multiple offenses, and committed a larceny.
# male = 1
# white = 1
# any other state = 1 
# larceny = 2

#odds
exp(-4.2411574 +0.38699 + 0.8867192 + 0.6837 - .1238867*3 - .0001756*50 + .0802954*12)  

#prob
1/(1 + exp(-1 * (-4.2411574 +0.38699 + 0.8867192 + 0.6837 - .1238867*3 - .0001756*50 + .0802954*12)   ))


#5.1
predictions = predict(type="response",paroleViolatorModel,newdata = paroleTest)
max(predictions)

#5.2
table(paroleTest$violator,predictions > 0.5)
confusionMatrix = as.matrix(table(paroleTest$violator,predictions > 0.5))
r = confusionMatrix
# recalling sensitivity TP/(TP+FN)
sensitivity = r[2,2]/(r[2,2]+ r[2,1])
sensitivity
# specificity TN/(TN+FP)
specificity = r[1,1]/(r[1,1]+ r[1,2])
specificity
# accuracy (TN + TP)/(TN+FP+TP+FN)
accuracy = (r[1,1] + r[2,2]) / (r[1,1] + r[1,2] + r[2,1] + r[2,2])
accuracy

#5.3
# accuracy of model predicting that every parolee is non violator
# All - violator/ All
#ie TN+FP/All
#ie baseline accuracy
(r[1,1] + r[1,2]) /(r[1,1] + r[1,2] + r[2,1] + r[2,2]) 

#5.6
library(ROCR)
ROCpred = prediction(predictions,paroleTest$violator)
aucOnTestSet = as.numeric(performance(ROCpred,"auc")@y.values)
aucOnTestSet



