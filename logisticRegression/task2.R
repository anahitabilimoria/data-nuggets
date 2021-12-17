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

X <- refinedAuto[,1:ncol(refinedAuto) - 1]
y <- refinedAuto[,ncol(refinedAuto)]

learningRate <- 0.001
iterationNumber <- 1000

testErrorRateList = vector()

predictedList <- logisticRegressionModel(iterationNumber, learningRate, as.matrix(X), y, flag = 0)
plot(c(1:iterationNumber), predictedList$costList)

predictedListyPred <- as.integer(as.logical(predictedList$yPred > 0.5))




