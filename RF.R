print(Sys.time())
customB <- 100
customH <- 3

setClass( "node", slots=list(is.leaf="logical", prediction="numeric", attr="character", split="numeric", left.node="node", right.node="node" ) )

computeRss <- function(sampleA, sampleB, columnName){
  
  rssA <- sum((sampleA[,columnName] - mean(sampleA[,columnName])) ^2)
  rssB <- sum((sampleB[,columnName] - mean(sampleB[,columnName])) ^2)
  
  rssSum <- rssA + rssB
  
  return (rssSum)
}

optimalSplit <- function(attributeSample, sampleBTS){
  minRssSum <- Inf
  for(attribute in attributeSample){
    
    for(uniqueValue in unique(sampleBTS[,attribute])){
      sampleA <- sampleBTS[sampleBTS[attribute]<=uniqueValue,]
      sampleB <- sampleBTS[sampleBTS[attribute]>uniqueValue,]
      
      rssSum <- computeRss(sampleA, sampleB, 'medv')
      
      if(rssSum < minRssSum){
        minRssSum <- rssSum
        minAttr <- attribute
        optimalUniqueValue <- uniqueValue
      }
      
    }
    
  }

  return (list(minRssSum = minRssSum, minAttr = minAttr, optimalUniqueValue = optimalUniqueValue))
}

generate <- function(data,indx,depth){		#if you do not want to limit the depth of the decision tree, you can set depth = len(indx)
  node <- new("node")
  d <- dim(data)[2]
  if (depth == 0){
    node@is.leaf <- TRUE
    node@prediction <- if(length(indx) == 0) 0 else mean(data[indx,d])
  }else{
    #extract all labels in data[indx], and check if they are all the same
    #if they are all the same, no split is needed, "node" is a leaf, and the prediction value is same as the common value of all the labels
    all_labels <- data[indx,d]
    all_labels <- unique(all_labels)
    if (length(all_labels) == 1){
      node@is.leaf <- TRUE
      node@prediction <- data[indx[1],d]
    }else{
      #in this case, "node" is internal, a split is needed, and we need to compute the optimal split
      node@is.leaf <- FALSE
      featureList <- colnames(data)
      featureList <- featureList[! featureList %in% ('medv')]      
      attributeSample <- sample(featureList, ceiling(length(featureList)/3))
      print(attributeSample)
      opt_split <- optimalSplit(attributeSample,data)
      
      node@attr <- opt_split$minAttr
      node@split <- opt_split$optimalUniqueValue
      
      #after computing the optimal split, we split data into two parts, one going to the left child, and the other going to the right child
      #we compute the indices of observations that go to left or right below
      Lindx <- c()
      Rindx <- c()
      for (i in indx){
        if (data[i,node@attr] <= node@split){
          Lindx <- append(Lindx,i)
        }
        else{
          Rindx <- append(Rindx,i)
        }
      }
      #then we create two new nodes, which are the left child and the right child of "node"
      #after this, we recursively call the function "generate" on the two children, to create one sub-tree under each child
      Lnode <- generate(data,Lindx,depth-1)
      Rnode <- generate(data,Rindx,depth-1)
      node@left.node <- Lnode
      node@right.node <- Rnode
    }
  }
  return(node)
}

traverse <- function(currentRow, root){
 currentNode <- root
 
 while (currentNode@is.leaf == FALSE){
   if(currentRow[,currentNode@attr] <= currentNode@split){
     currentNode <- currentNode@left.node
   }else{
     currentNode <- currentNode@right.node
   }
 }
 
  return (currentNode@prediction)
}

library(MASS)
# Boston <- na.omit(Boston)

set.seed(2110)
sampleSize = round(nrow(Boston)*.50)
index <- sample(seq_len(nrow(Boston)), size = sampleSize)

train <- Boston[index, ]
test <- Boston[-index, ]
testMSEList  <- c()
trainMSEList <- c()

trainMat <- matrix(0, customB, nrow(train))
testMat <- matrix(0, customB, nrow(test))
  
for(B in 1:customB) {

  sampleIndex <- sample(seq_len(nrow(train)), replace = TRUE)
  sampleBTS <- train[sampleIndex,]
  root <- generate(sampleBTS,1:(dim(sampleBTS)[1]),customH)
  
  for(trainIndex in 1:nrow(train)){
    currentRow <- train[trainIndex, ]
    trainMat[B,trainIndex] <- traverse(currentRow, root)
  }
  
  for(testIndex in 1:nrow(test)){
    currentRow <- test[testIndex, ]
    testMat[B,testIndex] <- traverse(currentRow, root)
  }  
  
}

trainAverage <- c()
testAverage <- c()

for(item in 1:nrow(train)){
  trainAverage <- append(trainAverage, mean(trainMat[,item]))
  testAverage <- append(testAverage, mean(testMat[,item]))
}

trainMSE <- sum((train[['medv']] - trainAverage)^2)/nrow(train)
testMSE <- sum((test[['medv']] - testAverage)^2)/nrow(test)

# testMSEList <- append(testMSEList, testMSE)
# trainMSEList <- append(trainMSEList, trainMSE)

# plot(customHList, trainMSEList, type = "l", col = "red", xlim = c(3,7), ylim = c(18,50), xlab = 'Depth', ylab = 'MSE')
# lines(customHList, testMSEList, type = 'l', col = "blue")
# legend(80, 48, legend=c("Train MSE", "Test MSE"),col=c("red", "blue"), lty=1, cex=0.8)

print(Sys.time())
