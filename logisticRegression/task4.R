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

learningRateList <- seq(10^-10, 10^-1, by=0.01)
iterationNumberList <- seq(1000, 10000, by=1000)
errorRateTrainList = vector()
errorRateTestList = vector()

for(i in 1:10){
  currentLearningRate = learningRateList[i]
  currentIterationNumber = as.integer(iterationNumberList[i])
  
  currentPredictedList <- logisticRegressionModel(currentIterationNumber, currentLearningRate, as.matrix(X_train), y_train, flag = 1)
  predictedListyPredCurrent <- as.integer(as.logical(currentPredictedList$yPred > 0.5))
  errorRateTrainCurrent <- sum((y_train-predictedListyPredCurrent)^2)/nrow(X_train)
  errorRateTrainList <- append(errorRateTrainList, errorRateTrainCurrent)
  
  Z2Current <- as.matrix(currentPredictedList$weightList)%*%t(X_test) + currentPredictedList$B
  testSigmoidFunctionCurrent <- 1/(1 + exp(-Z2Current))
  testSigmoidValCurrent <- as.integer(as.logical(testSigmoidFunctionCurrent > 0.5))
  errorRateTestCurrent <- sum((y_test-testSigmoidValCurrent)^2)/nrow(X_test)
  errorRateTestList <- append(errorRateTestList, errorRateTestCurrent)
}

finalMatrix <- cbind(learningRateList, iterationNumberList, errorRateTrainList, errorRateTestList)
print(finalMatrix)