loans = read.csv("loans.csv")
str(loans)
#1.1
nrow(loans[loans$not.fully.paid == TRUE,])/nrow(loans)

#1.2
columnsWithNa <- function(frame)
{
  variables = colnames(frame)
  lapply(variables,
         function(var) 
           if (sum(is.na(frame[,var])) > 0)
           {
             var
           })
}
columnsWithNa(loans)

#1.3
# let's see how much data gets removed if we take out the stuff with na
completes = complete.cases(loans)
nrow(loans[completes == TRUE,])

#1.4
loansImputed = read.csv("./loans_imputed.csv")
#install.packages("mice")
library(mice)
?setdiff
?complete
?mice


#2.1
set.seed(144)
library(caTools)
split = sample.split(loansImputed$not.fully.paid, SplitRatio = 0.7)
loansImputedTrain = subset(loansImputed, split == TRUE)
loansImputedTest = subset(loansImputed, split == FALSE)

loansModel = glm(family = binomial,data = loansImputedTrain,not.fully.paid ~ .)
summary(loansModel)

#2.2
#handwritten notes explain this
-9.317e-03 *-10

exp(-9.317e-03 *-10)

#2.3
loansImputedTest$predicted.risk = predict(loansModel,type="response",newdata = loansImputedTest)

confusionMatrix = as.matrix(table(loansImputedTest$not.fully.paid,loansImputedTest$predicted.risk > 0.5))
confusionMatrix
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
#ie baseline accuracy
(r[1,1] + r[1,2]) /(r[1,1] + r[1,2] + r[2,1] + r[2,2]) 

#2.4
library(ROCR)
ROCpred = prediction(loansImputedTest$predicted.risk,loansImputedTest$not.fully.paid)
aucOnTestSet = as.numeric(performance(ROCpred,"auc")@y.values)
aucOnTestSet

#3.1
intRateModel = glm(family = binomial,data = loansImputedTrain,not.fully.paid ~ int.rate)
summary(intRateModel)

#3.2
intRatePredictions = predict(intRateModel,type="response",newdata = loansImputedTest)
max(intRatePredictions)

#3.3
ROCpred = prediction(intRatePredictions,loansImputedTest$not.fully.paid)
aucOnTestSet = as.numeric(performance(ROCpred,"auc")@y.values)
aucOnTestSet

#4.1
#c * exp(rt)
#$10 investment with an annual interest rate of 6% pay back after 3 years, using continuous compounding of interest?
10*exp(.06*3)

#5.1
# estimating profitability on a 1$ investment over 3 years using
#c * exp(rt) - c 
loansImputedTest$profit = exp(loansImputedTest$int.rate*3) - 1
# and a negative profit of -c  where no repayment happened (we assume pessimistically not paid in full means no payment)
loansImputedTest$profit[loansImputedTest$not.fully.paid == 1] = -1
#max for a 10$ investment
max(loansImputedTest$profit)*10

#6.1
highInterest = subset(loansImputedTest, loansImputedTest$int.rate >= 0.15)
mean(highInterest$profit)
nrow(highInterest[highInterest$not.fully.paid ==TRUE,])/nrow(highInterest)

#6.2
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectLoans = subset(highInterest,highInterest$predicted.risk <= cutoff)
nrow(selectLoans)
sum(selectLoans$profit)
sum(selectLoans$not.fully.paid)
