source('~/Desktop/task3.R')
source('~/Desktop/logisticRegressionModel.R')

Auto <- read.table("/Users/anahitabilimoria/Desktop/auto.data", header=T, na.strings="?")
Auto <- na.omit(Auto)
dim(Auto)

highList = vector()
for(i in 1:nrow(Auto)) {
  if(Auto[i,'mpg'] >= 23){
    highList <- append(highList, 1)
  }else{
    highList <- append(highList, 0)
  }
}

Auto$origin1 <- ifelse(Auto$origin == 1,1,0)
Auto$origin2 <- ifelse(Auto$origin == 2,1,0)

refinedAuto <- data.frame(Auto$horsepower, Auto$weight, Auto$year, Auto$origin1, Auto$origin2)
refinedAuto <- data.frame(scale(refinedAuto))

refinedAuto$high <- highList

splitList <- trainTestSplit(refinedAuto)

X_train <- splitList$X_train
X_test <- splitList$X_test
y_train <- splitList$y_train
y_test <- splitList$y_test

testCases <- 4
learningRate <- 0.001
iterationNumber <- 10000

trainErrorRateListNR <- vector()
WListNR <- matrix(NA, nrow = 4, ncol = 5)
BListNR <- vector()

trainErrorRateListIR <- vector()
WListIR <- matrix(NA, nrow = 4, ncol = 5)
BListIR <- vector()

for(y in 1:testCases){
  
  predictedList <- logisticRegressionModel(iterationNumber, learningRate, as.matrix(X_train), y_train, flag = 1)
  
  predictedListyPred <- as.integer(as.logical(predictedList$yPred > 0.5))
  errorRateTrain <- sum((y_train-predictedList$yPred)^2)/nrow(X_train)
  accuracyTrain <- 1 - errorRateTrain
  
  trainErrorRateListNR <- append(trainErrorRateListNR, errorRateTrain)
  WListNR[y,] <- predictedList$weightList
  BListNR[y] <- predictedList$B
  
  predictedList <- logisticRegressionModelTask5(learningRate, as.matrix(X_train), y_train)
  
  predictedListyPred <- as.integer(as.logical(predictedList$yPred > 0.5))
  errorRateTrain <- sum((y_train-predictedList$yPred)^2)/nrow(X_train)
  accuracyTrain <- 1 - errorRateTrain

  trainErrorRateListIR <- append(trainErrorRateListIR, errorRateTrain)
  WListIR[y,] <- predictedList$weightList
  BListIR[y] <- predictedList$B
  
}
print("Train error list for Case A:")
print(trainErrorRateListNR)
print("Train error list for Case B:")
print(trainErrorRateListIR)
minError <- which.min(trainErrorRateListNR)
print("Task 4 optimal prediction rules")
print("Error rate:")
print(trainErrorRateListNR[minError])
print("Weights:")
print(WListNR[minError,])
print("Bias:")
print(BListNR[minError])
print("")
minError <- which.min(trainErrorRateListIR)
print("Task 5 optimal prediction rules")
print("Error rate:")
print(trainErrorRateListIR[minError])
print("Weights:")
print(WListIR[minError,])
print("Bias:")
print(BListIR[minError])
print("")

