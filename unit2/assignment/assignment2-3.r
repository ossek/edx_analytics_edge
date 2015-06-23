#1.1
flutrain = read.csv("./FluTrain.csv")
str(flutrain)

flutrain$year = as.numeric(format(as.Date(flutrain$Week),"%Y"))
flutrain2004_2011 = subset(flutrain,flutrain$year >= 2004 && flutrain$year <= 2011)
flutrain2004_2011[flutrain2004_2011$ILI == max(flutrain2004_2011$ILI),]
flutrain2004_2011[flutrain2004_2011$Queries == max(flutrain2004_2011$Queries),]

flutrain2004_2011

#1.2
?hist

#1.3
# When handling a 'skewed' independent var such as ILI, it's 
#  useful to predict log of dependent variable instead of the variable 
#  to prevent the small number of very large or very small values from having 'undue' influence on the SSE
plot(x = log(flutrain$ILI),y = flutrain$Queries)

#2.2 
logILImodel = lm(log(ILI) ~ Queries, data = flutrain)
summary(logILImodel)

#2.3
cor(log(flutrain$ILI),flutrain$Queries)
log(1/cor(log(flutrain$ILI),flutrain$Queries))
exp(-0.5*cor(log(flutrain$ILI),flutrain$Queries))
cor(log(flutrain$ILI),flutrain$Queries)^2

#3.3
flutest = read.csv('./FluTest.csv')
?exp
PredTest1 = exp(predict(logILImodel, newdata=flutest))

?which
flutest[flutest$Week == "2012-03-11 - 2012-03-17",'ILI']
estimated = PredTest1[11]
estimated

#3.2
#relative error of our prediction
# aka (Observed ILI - Estimated ILI)/Observed ILI
observed = flutest[flutest$Week == "2012-03-11 - 2012-03-17",'ILI']
(observed - estimated)/observed
