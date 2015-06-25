songs = read.csv("./songs.csv")
str(songs)

#1.1
songs2010 = subset(songs,songs$year == 2010)
str(songs2010)

#1.2
nrow(songs[songs$artistname == "Michael Jackson",])

#1.3
songs[songs$artistname == "Michael Jackson" & songs$Top10 == TRUE,c("songtitle")]

#1.4
?as.factor
as.factor(songs$timesignature)

sort(table(songs$timesignature),decreasing = TRUE)

#1.5
songs[songs$tempo == max(songs$tempo),c("songtitle")]

#2.1
songsTrain = subset(songs,songs$year < 2010)
songsTest = subset(songs,songs$year == 2010)

nrow(songsTrain)
str(songsTrain)

#2.2
#2.3
#2.4
# for the purposes of model building, we'll remove this stuff
removeVars = c("year", "songtitle", "artistname", "songID", "artistID")
songsTrain = songsTrain[,!(names(songsTrain) %in% removeVars)]
songsTest = songsTest[,!(names(songsTest) %in% removeVars)]
songsModel = glm(Top10 ~ .,data=songsTrain,family = binomial)
summary(songsModel)

#2.5
cor(songsTrain$loudness,songsTrain$energy)

#3.2
#same model, without loudness
#NOTE subtracting from "." only works with numeric variables
songsLog2 = glm(Top10 ~ . - loudness, data=songsTrain, family=binomial)
summary(songsLog2)

#3.3
songsLog3 = glm(Top10 ~ . - energy, data=songsTrain, family=binomial)
summary(songsLog3)

#3.4
predictions = predict(songsLog3,type = "response",newdata = songsTest)

confusionMatrix = table(songsTest$Top10,predictions > 0.45)
confusionMatrix
#remembering accuracy is 
#(TP+TN)/(TP+TN + FP +FN)
confusionMatrixM = as.matrix(confusionMatrix)
r = confusionMatrixM
r[1,1]
r[2,2]
accuracy = (r[1,1] + r[2,2]) / (r[1,1] + r[1,2] + r[2,1] + r[2,2])
accuracy

#4.1
# comparison to baseline accuracy
# most frequent 0 to all
baselineAccuracy = (r[1,1] + r[1,2]) / (r[1,1] + r[1,2] + r[2,1] + r[2,2])
baselineAccuracy

#4.3
#True pos rate / all pos (TP + FN)
sensitivity = r[2,2]/(r[2,2]+ r[2,1])
sensitivity

#True neg / all neg (TN + FP)
specificity = r[1,1]/(r[1,1]+ r[1,2])
specificity