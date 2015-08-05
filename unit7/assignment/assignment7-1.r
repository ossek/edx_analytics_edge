library(ggplot2)
library(maps)
library(ggmap)

statesMap = map_data("state")

#1.1
length(unique(statesMap$group))
#1.2
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

#2.1
polling = read.csv("./PollingImputed.csv")
library(caTools)
pollingTrain = subset(polling,polling$Year == 2004 | polling$Year == 2008)
pollingTest = subset(polling,polling$Year == 2012)

mod2 = glm(Republican~SurveyUSA+DiffCount, data=pollingTrain, family="binomial")
testPrediction = predict(mod2, newdata=pollingTest, type="response")

#vector of repub /dem for each state
testPredictionBinary = as.numeric(testPrediction > 0.5)
#Now, put the predictions and state labels in a data.frame so that we can use ggplot: 
predictionDataFrame = data.frame(testPrediction, testPredictionBinary, pollingTest$State)

#where 1 is repub and 0 is dem
nrow(predictionDataFrame[predictionDataFrame$testPredictionBinary == 1,])
mean(predictionDataFrame$testPrediction)

#2.2
#lower and merge to statesMap
predictionDataFrame$region = tolower(predictionDataFrame$pollingTest.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")

#Lastly, we need to make sure the observations are in order so that the map is drawn properly
predictionMap = predictionMap[order(predictionMap$order),]

nrow(predictionMap)
nrow(statesMap)

#2.3
?merge 
#we note that some data was skipped from the state map side in the merge because we only merge on what's in both (eg alaska is
#missing from predictions)

#2.4
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPredictionBinary)) + geom_polygon(color = "black")

#2.5
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")

#3.1
unique(predictionMap$region)
predictionMap[predictionMap$region == "florida",]
pollingTest[tolower(pollingTest$State) == "florida",]

#3.2
unique(predictionMap[predictionMap$region == "florida",c("testPrediction")])

#4.1
?geom_polygon
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPredictionBinary)) + geom_polygon(color = "black",linetype=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPredictionBinary)) + geom_polygon(color = "black",size=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

#4.2
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = testPredictionBinary)) + geom_polygon(color = "black", alpha = 0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
