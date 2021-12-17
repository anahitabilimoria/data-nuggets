source('~/Desktop/task3.R')
source('~/Desktop/logisticRegressionModelTask5.R')

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

learningRate = 0.01
predictedList <- logisticRegressionModelTask5(learningRate, as.matrix(X_train), y_train)

predictedListyPred <- as.integer(as.logical(predictedList$yPred > 0.5))
errorRateTrain <- sum((y_train-predictedList$yPred)^2)/nrow(X_train)
accuracyTrain <- 1 - errorRateTrain

Z2 <- as.matrix(predictedList$weightList)%*%t(X_test) + predictedList$B
testSigmoidFunction <- 1/(1 + exp(-Z2))
testSigmoidVal <- as.integer(as.logical(testSigmoidFunction > 0.5))
errorRateTest <- sum((y_test-testSigmoidVal)^2)/nrow(X_test)
accuracyTest <- 1 - errorRateTest

print(predictedList$iterations)
plot(c(1:predictedList$iterations), predictedList$finalCost)
  
