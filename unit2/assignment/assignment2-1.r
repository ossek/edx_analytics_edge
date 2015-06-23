climate = read.csv("./climate_change.csv")
str(climate)
climateTrain = subset(climate,climate$Year <= 2006)
climateTest = subset(climate,climate$Year > 2006)
str(climateTrain)
str(climateTest)

#1.1
## NOTE: it is imperative to use the 'data' parameter here in order 
## for the predict function to work with a model that is based on the 
## climateTrain set.  it somehow seems to look for same name.
climateTrainModel1 = lm(Temp ~ MEI + CO2 + CH4 
                        + N2O + CFC.11 + CFC.12 +
                          TSI + Aerosols,data = climateTrain)
summary(climateTrainModel1)

#2.1 Try removing all but the variables in question to see if the negative coefficients change
#  Which would indicate multicollinearity
climateTrainModelN20 = lm(climateTrain$Temp ~ climateTrain$N2O)
summary(climateTrainModelN20)
climateTrainModelCFC11 = lm(climateTrain$Temp ~ climateTrain$CFC.11)
summary(climateTrainModelCFC11)

#2.2
#correlation between all possible pairs
cors <- function(climateData)
{
  variables = colnames(climateData)
  lapply(variables,function(var1) 
    lapply(variables, function(var2) 
      c(var1,var2,cor(climateData[,var1],climateData[,var2]))))
}
cors(climateTrain)

#correlation between a variable and all others
corsVar <- function(var1,climateData)
{
  variables = colnames(climateData)
  lapply(variables, function(var2) 
    c(var1,var2,cor(climateData[,var1],climateData[,var2])))
}
corsVar('N2O',climateTrain)
corsVar('CFC.11',climateTrain)

#3
climateTrainModel2 = lm(climateTrain$Temp ~ climateTrain$MEI+ climateTrain$N2O + climateTrain$TSI + climateTrain$Aerosols)
summary(climateTrainModel2)

#4
climateStepModel = step(climateTrainModel1)
summary(climateStepModel)

#5
tempPredictions = predict(climateStepModel,newdata = climateTest)
SSE = sum((climateTest$Temp - tempPredictions)^2)
#difference between our test datapoint actual values and the values of the 'baseline' model
SST = sum((climateTest$Temp - mean(climateTrain$Temp))^2)
testSetRSquared = 1 - SSE/SST
testSetRSquared