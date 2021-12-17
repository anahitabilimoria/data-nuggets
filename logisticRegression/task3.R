trainTestSplit <- function(dataset) {
  
  set.seed(2110)
  
  sampleSize = round(nrow(dataset)*.50)
  index <- sample(seq_len(nrow(dataset)), size = sampleSize)
  
  train <- dataset[index, ]
  test <- dataset[-index, ]
  
  return(list("X_train"= train[,1:ncol(train) - 1], "X_test" = test[,1:ncol(test) - 1], 
              "y_train"= train[,ncol(train)], "y_test" = test[,ncol(test)]))
}