pisaTest = read.csv("./pisa2009test.csv")
pisaTrain = read.csv("./pisa2009train.csv")

#1.1
str(pisaTrain)

#1.2
?tapply
tapply(pisaTrain$readingScore,pisaTrain$male,mean)

#1.3
columnsWithNa <- function(frame)
{
  variables = colnames(frame)
  lapply(variables,
         function(var) 
           if (sum(is.na(frame[,var])) > 0)
           {
             var
           }
}
columnsWithNa(pisaTrain)

#1.4
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTrain)
str(pisaTest)

#2.1
levels(pisaTrain$grade)
levels(pisaTrain$male)
levels(pisaTrain$raceeth)
is.ordered(pisaTrain$grade)
is.ordered(pisaTrain$male)
is.ordered(pisaTrain$raceeth)
is.factor(pisaTrain$grade)
is.factor(pisaTrain$male)
is.factor(pisaTrain$raceeth)


#2.2
#make binary variables for each of the levels in the unordered factor raceeth
levels(pisaTrain$raceeth)

#3.1
#relevel / set reference level to most common instead of first aplphabetical
str(pisaTrain)
pisaTrain$raceeth = relevel(pisaTrain$raceeth,"White")
pisaTest$raceeth = relevel(pisaTest$raceeth,"White")

#make a model with some shorthand notation for all remaining vars
lmscore = lm(readingScore ~ .,data = pisaTrain)
summary(lmscore)

#3.2 
#let's get the RMSE for lmscore
SSE = sum(lmscore$residuals^2)
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE

#3.3
# find the difference between two score predictions (based on our model)
#   using the relevant coefficient, with all other terms cancelling:
29.54*11 - 29.54*9

#3.4

#4.1
predictedScores = predict(lmscore,newdata = pisaTest)
predictionRange = (max(predictedScores) - min(predictedScores))
predictionRange

#4.2
SSE = sum((predictedScores - pisaTest$readingScore)^2)
SSE
RMSE = sqrt(SSE/nrow(pisaTest))
RMSE

#4.3
mean(pisaTrain$readingScore)
SST = sum((pisaTest$readingScore - mean(pisaTrain$readingScore))^2)
SST

testRSQlmscore = 1 - (SSE/SST)
testRSQlmscore
  