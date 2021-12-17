logisticRegressionModel <- function(iterationNumber, learningRate, X_train, y_train, flag) {
  costs <- vector()
  
  if(flag == 1){
    W <- runif(ncol(X_train), min=-0.7, max=0.7)
    bias <- runif(1, -0.7, 0.7)    
  }else{
    W <- rep(0,ncol(X_train))
    bias <- 0     
  }

  for (i in 1:iterationNumber) {
    Z <- W%*%t(X_train) + bias
    
    sigmoidFunction <- 1/(1 + exp(-Z))
    
    cost <- -(sum(y_train*log(sigmoidFunction)+(1-y_train)*log(1-sigmoidFunction)))
    
    dW <- (sigmoidFunction-y_train)%*%X_train
    dB <- sum(sigmoidFunction-y_train)
    W <- W - (learningRate * dW)
    bias <- bias - (learningRate * dB)
    
    costs <- append(costs, cost)
  }

  return(list("weightList"= W, "B" = bias, "yPred"= sigmoidFunction, "costList" = costs))
}
