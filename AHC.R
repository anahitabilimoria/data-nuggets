setClass( "node", slots=list(is.leaf="logical", height="numeric", observations="vector", left.node="node", right.node="node" ) )

singleLinkage <- function(distMat){
  currentMat <- distMat
  
  while(TRUE){
    
    minElement <- min(currentMat, na.rm = TRUE)
    minElementLoc <- which(currentMat==minElement, arr.ind=TRUE)
    currentMat[minElementLoc[1,1],] <- NA
    currentMat[,minElementLoc[1,2]] <- NA
    
    print(minElement)
    print(minElementLoc)
    parentNode <- new("node")
    parentNode@is.leaf <- FALSE
    parentNode@height <- minElement
    
    parentNode@observations <- c(leafList[[minElementLoc[1,1]]]@observations, leafList[[minElementLoc[1,2]]]@observations)
    parentNode@left.node <- leafList[[minElementLoc[1,1]]]
    parentNode@right.node <- leafList[[minElementLoc[1,2]]]
    
    leafList <- append(leafList, parentNode)
    
    compareMat <- matrix(NA, 1, (nrow(distMat) + nrow(currentMat) - 64))
    
    for(obs in parentNode@observations){
      distanceArr <- c()
      obsRow <- ncidata[obs,]
      for(nciObj in 1:nrow(ncidata)){
        if(nciObj != obs && !(nciObj %in% parentNode@observations)){
          distance <- dist(rbind(obsRow, ncidata[nciObj,]))
        }else{
          distance <- NA
        }
        distanceArr <- append(distanceArr, distance)
      }
      
      for(nciObj in 65:nrow(currentMat)){
        distanceCluster <- c()
        observationArr <- leafList[[nciObj]]@observations
        if((nrow(currentMat) - nrow(ncidata)) >= 1){
          if(! any(parentNode@observations %in% leafList[[nciObj]]@observations) ){
            for(clusterObs in observationArr){
              distanceCluster <- append(distanceCluster, dist(rbind(ncidata[obs,], ncidata[clusterObs,])))
            }
            distanceElem <- if(min(distanceCluster, na.rm = TRUE) == Inf || length(distanceCluster) == 0) NA else min(distanceCluster, na.rm = TRUE)
            distanceArr <- append(distanceArr, distanceElem)  
          }
          else{
            for(clusterObs in observationArr){
              distanceCluster <- append(distanceCluster, NA)
            }
            distanceElem <- if(min(distanceCluster, na.rm = TRUE) == Inf || length(distanceCluster) == 0) NA else min(distanceCluster, na.rm = TRUE)
            distanceArr <- append(distanceArr, distanceElem) 
          }        
        }
      }
      
      compareMat <- rbind(compareMat, distanceArr)
      
    }
    
    compareMat <- as.matrix(compareMat[-1,])
    minDistList <- c()
    for(nrows in 1:ncol(compareMat)){
      minDistList <- append(minDistList, if( min(compareMat[, nrows], na.rm = TRUE) == Inf) NA else min(compareMat[, nrows], na.rm = TRUE) )
    }
    
    currentMat <- rbind(currentMat, minDistList)
    minDistList <- append(minDistList, NA)
    currentMat <- cbind(currentMat, minDistList)
    currentMat[lower.tri(currentMat)] <- NA
    
    print(parentNode@observations)
    for(obs in parentNode@observations){
      currentMat[obs,] <- NA
      currentMat[,obs] <- NA
    } 
    
    if(length(parentNode@observations) == nrow(ncidata)){
      break
    }
  }
  
  return (leafList)
}

completeLinkage <- function(distMat){
    currentMat <- distMat
  
    while(TRUE){
    
    maxElement <- max(currentMat, na.rm = TRUE)
    maxElementLoc <- which(currentMat==maxElement, arr.ind=TRUE)
    currentMat[maxElementLoc[1,1],] <- NA
    currentMat[,maxElementLoc[1,2]] <- NA
    
    print(maxElement)
    print(maxElementLoc)
    parentNode <- new("node")
    parentNode@is.leaf <- FALSE
    parentNode@height <- maxElement
    
    parentNode@observations <- c(leafList[[maxElementLoc[1,1]]]@observations, leafList[[maxElementLoc[1,2]]]@observations)
    parentNode@left.node <- leafList[[maxElementLoc[1,1]]]
    parentNode@right.node <- leafList[[maxElementLoc[1,2]]]
    
    leafList <- append(leafList, parentNode)
    
    compareMat <- matrix(NA, 1, (nrow(distMat) + nrow(currentMat) - 64))
    
    for(obs in parentNode@observations){
      distanceArr <- c()
      obsRow <- ncidata[obs,]
      for(nciObj in 1:nrow(ncidata)){
        if(nciObj != obs && !(nciObj %in% parentNode@observations)){
          distance <- dist(rbind(obsRow, ncidata[nciObj,]))
        }else{
          distance <- NA
        }
        distanceArr <- append(distanceArr, distance)
      }
      
      for(nciObj in 65:nrow(currentMat)){
        distanceCluster <- c()
        observationArr <- leafList[[nciObj]]@observations
        if((nrow(currentMat) - nrow(ncidata)) >= 1){
          if(! any(parentNode@observations %in% leafList[[nciObj]]@observations) ){
            for(clusterObs in observationArr){
              distanceCluster <- append(distanceCluster, dist(rbind(ncidata[obs,], ncidata[clusterObs,])))
            }
            distanceElem <- if(max(distanceCluster, na.rm = TRUE) == Inf || length(distanceCluster) == 0) NA else max(distanceCluster, na.rm = TRUE)
            distanceArr <- append(distanceArr, distanceElem)  
          }
          else{
            for(clusterObs in observationArr){
              distanceCluster <- append(distanceCluster, NA)
            }
            distanceElem <- if(max(distanceCluster, na.rm = TRUE) == Inf || length(distanceCluster) == 0) NA else max(distanceCluster, na.rm = TRUE)
            distanceArr <- append(distanceArr, distanceElem) 
          }        
        }
      }
      
      compareMat <- rbind(compareMat, distanceArr)
      
    }
    
    compareMat <- as.matrix(compareMat[-1,])
    maxDistList <- c()
    for(nrows in 1:ncol(compareMat)){
      maxDistList <- append(maxDistList, if( max(compareMat[, nrows], na.rm = TRUE) == Inf) NA else max(compareMat[, nrows], na.rm = TRUE) )
    }
    
    currentMat <- rbind(currentMat, maxDistList)
    maxDistList <- append(maxDistList, NA)
    currentMat <- cbind(currentMat, maxDistList)
    currentMat[lower.tri(currentMat)] <- NA
    
    print(parentNode@observations)
    for(obs in parentNode@observations){
      currentMat[obs,] <- NA
      currentMat[,obs] <- NA
    } 
    
    if(length(parentNode@observations) == nrow(ncidata)){
      break
    }
  }
  
  return (leafList)
}

averageLinkage <- function(distMat){
    currentMat <- distMat
  
    while(TRUE){
    
    meanElement <- mean(currentMat, na.rm = TRUE)
    meanElementLoc <- which(currentMat==meanElement, arr.ind=TRUE)
    currentMat[meanElementLoc[1,1],] <- NA
    currentMat[,meanElementLoc[1,2]] <- NA
    
    print(meanElement)
    print(meanElementLoc)
    parentNode <- new("node")
    parentNode@is.leaf <- FALSE
    parentNode@height <- meanElement
    
    parentNode@observations <- c(leafList[[meanElementLoc[1,1]]]@observations, leafList[[meanElementLoc[1,2]]]@observations)
    parentNode@left.node <- leafList[[meanElementLoc[1,1]]]
    parentNode@right.node <- leafList[[meanElementLoc[1,2]]]
    
    leafList <- append(leafList, parentNode)
    
    compareMat <- matrix(NA, 1, (nrow(distMat) + nrow(currentMat) - 64))
    
    for(obs in parentNode@observations){
      distanceArr <- c()
      obsRow <- ncidata[obs,]
      for(nciObj in 1:nrow(ncidata)){
        if(nciObj != obs && !(nciObj %in% parentNode@observations)){
          distance <- dist(rbind(obsRow, ncidata[nciObj,]))
        }else{
          distance <- NA
        }
        distanceArr <- append(distanceArr, distance)
      }
      
      for(nciObj in 65:nrow(currentMat)){
        distanceCluster <- c()
        observationArr <- leafList[[nciObj]]@observations
        if((nrow(currentMat) - nrow(ncidata)) >= 1){
          if(! any(parentNode@observations %in% leafList[[nciObj]]@observations) ){
            for(clusterObs in observationArr){
              distanceCluster <- append(distanceCluster, dist(rbind(ncidata[obs,], ncidata[clusterObs,])))
            }
            distanceElem <- if(mean(distanceCluster, na.rm = TRUE) == Inf || length(distanceCluster) == 0) NA else mean(distanceCluster, na.rm = TRUE)
            distanceArr <- append(distanceArr, distanceElem)  
          }
          else{
            for(clusterObs in observationArr){
              distanceCluster <- append(distanceCluster, NA)
            }
            distanceElem <- if(mean(distanceCluster, na.rm = TRUE) == Inf || length(distanceCluster) == 0) NA else mean(distanceCluster, na.rm = TRUE)
            distanceArr <- append(distanceArr, distanceElem) 
          }        
        }
      }
      
      compareMat <- rbind(compareMat, distanceArr)
      
    }
    
    compareMat <- as.matrix(compareMat[-1,])
    meanDistList <- c()
    for(nrows in 1:ncol(compareMat)){
      meanDistList <- append(meanDistList, if( mean(compareMat[, nrows], na.rm = TRUE) == Inf) NA else mean(compareMat[, nrows], na.rm = TRUE) )
    }
    
    currentMat <- rbind(currentMat, meanDistList)
    meanDistList <- append(meanDistList, NA)
    currentMat <- cbind(currentMat, meanDistList)
    currentMat[lower.tri(currentMat)] <- NA
    
    print(parentNode@observations)
    for(obs in parentNode@observations){
      currentMat[obs,] <- NA
      currentMat[,obs] <- NA
    } 
    
    if(length(parentNode@observations) == nrow(ncidata)){
      break
    }
  }
  
  return (leafList)
}

centroidLinkage <- function(distMat){
  currentMat <- distMat
  
  while(TRUE){
    medianElement <- median(currentMat, na.rm = TRUE)
    medianElementLoc <- which(currentMat==medianElement, arr.ind=TRUE)
    currentMat[medianElementLoc[1,1],] <- NA
    currentMat[,medianElementLoc[1,2]] <- NA
    
    print(medianElement)
    print(medianElementLoc)
    parentNode <- new("node")
    parentNode@is.leaf <- FALSE
    parentNode@height <- medianElement
    
    parentNode@observations <- c(leafList[[medianElementLoc[1,1]]]@observations, leafList[[medianElementLoc[1,2]]]@observations)
    parentNode@left.node <- leafList[[medianElementLoc[1,1]]]
    parentNode@right.node <- leafList[[medianElementLoc[1,2]]]
    
    leafList <- append(leafList, parentNode)
    
    compareMat <- matrix(NA, 1, (nrow(distMat) + nrow(currentMat) - 64))
    
    for(obs in parentNode@observations){
      distanceArr <- c()
      obsRow <- ncidata[obs,]
      for(nciObj in 1:nrow(ncidata)){
        if(nciObj != obs && !(nciObj %in% parentNode@observations)){
          distance <- dist(rbind(obsRow, ncidata[nciObj,]))
        }else{
          distance <- NA
        }
        distanceArr <- append(distanceArr, distance)
      }
      
      for(nciObj in 65:nrow(currentMat)){
        distanceCluster <- c()
        observationArr <- leafList[[nciObj]]@observations
        if((nrow(currentMat) - nrow(ncidata)) >= 1){
          if(! any(parentNode@observations %in% leafList[[nciObj]]@observations) ){
            for(clusterObs in observationArr){
              distanceCluster <- append(distanceCluster, dist(rbind(ncidata[obs,], ncidata[clusterObs,])))
            }
            distanceElem <- if(median(distanceCluster, na.rm = TRUE) == Inf || length(distanceCluster) == 0) NA else median(distanceCluster, na.rm = TRUE)
            distanceArr <- append(distanceArr, distanceElem)  
          }
          else{
            for(clusterObs in observationArr){
              distanceCluster <- append(distanceCluster, NA)
            }
            distanceElem <- if(median(distanceCluster, na.rm = TRUE) == Inf || length(distanceCluster) == 0) NA else median(distanceCluster, na.rm = TRUE)
            distanceArr <- append(distanceArr, distanceElem) 
          }        
        }
      }
      
      compareMat <- rbind(compareMat, distanceArr)
      
    }
    
    compareMat <- as.matrix(compareMat[-1,])
    medianDistList <- c()
    for(nrows in 1:ncol(compareMat)){
      medianDistList <- append(medianDistList, if( median(compareMat[, nrows], na.rm = TRUE) == Inf) NA else median(compareMat[, nrows], na.rm = TRUE) )
    }
    
    currentMat <- rbind(currentMat, medianDistList)
    medianDistList <- append(medianDistList, NA)
    currentMat <- cbind(currentMat, medianDistList)
    currentMat[lower.tri(currentMat)] <- NA
    
    print(parentNode@observations)
    for(obs in parentNode@observations){
      currentMat[obs,] <- NA
      currentMat[,obs] <- NA
    } 
    
    if(length(parentNode@observations) == nrow(ncidata)){
      break
    }
  }

  return (leafList)
}

ncidata <- read.table("/path/to/ncidata.txt") 
ncidata <- t(ncidata)

distMat <- as.matrix(dist(ncidata, method = "euclidean", diag = FALSE, upper = FALSE))
distMat[lower.tri(distMat)] <- NA
distMat[distMat == 0] <- NA

leafList <- c()
for (i in 1:nrow(ncidata)){
  node <- new('node')
  node@is.leaf <- TRUE
  node@height <- 0
  node@observations <- c(i)
  
  leafList <- append(leafList, node)
}  

singleLeafList <- singleLinkage(distMat)

completeLeafList <- completeLinkage(distMat)

averageLeafList <- averageLinkage(distMat)

centroidLeafList <- centroidLinkage(distMat)

