logisticRegressionModelTask5 <- function(learningRate, X_train, y_train) {
  costs <- vector()
  
  W <- runif(ncol(X_train), min=-0.7, max=0.7)
  bias <- runif(1, -0.7, 0.7)    
  currentIndex = 1
  
  while(TRUE){
    Z <- W%*%t(X_train) + bias
    
    sigmoidFunction <- 1/(1 + exp(-Z))
    
    cost <- -(sum(y_train*log(sigmoidFunction)+(1-y_train)*log(1-sigmoidFunction)))
    
    dW <- (sigmoidFunction-y_train)%*%X_train
    dB <- sum(sigmoidFunction-y_train)
    W <- W - (learningRate * dW)
    bias <- bias - (learningRate * dB)
    
    costs <- append(costs, cost)
    
    if(currentIndex > 10){
      if(((costs[currentIndex - 10] - costs[currentIndex])*100/costs[currentIndex]) <= 0.1){
        break
      } 
    }
    currentIndex <- currentIndex + 1
    print(costs)
  }
  
  return(list("weightList"= W, "B" = bias, "yPred"= sigmoidFunction, "iterations" = currentIndex, "finalCost" = costs))
}
