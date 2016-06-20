#Author: Annabell Kuldmaa

#If running code following libraries must be installed
library("caret")
library("caTools")

#Change working directory according to your file system
setwd("E:/Users/AnnabellK/Desktop/Spring2016/GameTheory/RepeatedGames")

#Loading data
game2= (read.table("game_2.txt"))
game1= (read.table("game_1.txt"))

#Calculate Q values for all players
calculateQValues <- function(gameData, gameNum, alpha, k){
  #Game2
  if(gameNum==2){
  payoffRowPlayer = matrix(c(3,1,1,2), nrow=2,ncol=2,byrow = TRUE)
  payoffColumnPlayer = matrix(c(1,3,2,1),nrow=2,ncol=2,byrow = TRUE)
  NE <- 1.67 #expected payoff if playing MSNE
  }
  #Game1
  else{
  payoffRowPlayer = matrix(c(1,0,2,2,1,0,0,2,1), nrow=3,ncol=3,byrow = TRUE)
  payoffColumnPlayer = matrix(c(1,2,0,0,1,2,2,0,1), nrow=3,ncol=3,byrow = TRUE)
  NE <- 1 #expected payoff if playing MSNE
  }
  for(i in seq(1, 50, by=2)){
    if(gameNum==2){
      if (gameData[i,][,1] == 1){
        rowPlayer<- gameData[i,][,2:51]
        columnPlayer<- gameData[i+1,][,2:51]
        x <- calculateQ(columnPlayer, payoffRowPlayer, payoffColumnPlayer, TRUE,NE, k)
        y<- calculateQ(rowPlayer, payoffRowPlayer, payoffColumnPlayer, FALSE,NE, k)
      }
      else{
      rowPlayer<- gameData[i+1,][,2:51]
      columnPlayer<- gameData[i,][,2:51]
      y <- calculateQ(columnPlayer, payoffRowPlayer, payoffColumnPlayer, TRUE,NE,k)
      x <- calculateQ(rowPlayer, payoffRowPlayer, payoffColumnPlayer, FALSE,NE,k)
      
      }
    }
    else{
      rowPlayer<- gameData[i,][,1:50]
      columnPlayer<- gameData[i+1,][,1:50]
      x <- calculateQ(columnPlayer, payoffRowPlayer, payoffColumnPlayer, TRUE,NE, k)
      y <- calculateQ(rowPlayer, payoffRowPlayer, payoffColumnPlayer, FALSE,NE, k)
    }
    if(i==1){
      QValues <- list(x, y)
    }
    else{
      QValues[[i]] <- x
      QValues[[i+1]] <- y
    }
  }
  return(QValues)
}

calculateQ <- function(otherPlayer, payoffRow, payoffColumn, otherIsColumn,NE, k){
  #We hold alpha constant at 0.8
  alpha<-0.8
  if(length(payoffRow) == 4){
    maxSample <- 1
    Q <- matrix(c(0,0,0,0), nrow=2,ncol=2,byrow = TRUE) 
  }
  else{
    maxSample <- 2
    Q <- matrix(c(0,0,0,0,0,0), nrow=3,ncol=3,byrow = TRUE) 
  }
  for (j in 1:(k-1)){
  #Choose random action
    if(otherIsColumn){
      a<- sample(0:maxSample,1)
      b<- otherPlayer[,j+1]
      ra <- payoffRow[a+1,][b+1] 
    }
    else{
      b<- sample(0:maxSample,1)
      a<- otherPlayer[,j+1]
      ra <- payoffColumn[a+1,][b+1]
    }
    Q[a+1,][b+1] <- (1-alpha)*Q[a+1,][b+1] + alpha*(ra + NE)
  }
  return(Q)
}

#predict k-th round
generateModel <- function(gameData, gameNum, k){
  if(gameNum==2){
    features <- gameData[,2:(k-5)]
    y <- gameData[,(k-4)]
    trainingData <- gameData[,2:(k-4)]
    #Change levels of output
    trainingY <- as.factor(y)
    levels(trainingY)[1] <- "Yes"
    levels(trainingY)[2] <- "NO"
  }
  else{
    features <- gameData[,1:(k-8)]
    y <- gameData[,(k-7)]
    trainingData <- gameData[,1:(k-7)]
    #Change levels of output
    trainingY <- as.factor(y)
    levels(trainingY)[1] <- "P"
    levels(trainingY)[2] <- "R"
    levels(trainingY)[3] <- "S" 
  }
  trainCtrl<- trainControl(method="cv")
  tuned <- train(features, as.factor(trainingY), trControl=trainCtrl, method="LogitBoost", metric="Accuracy")
  logModel  <- LogitBoost(features, as.factor(trainingY), nIter=tuned$bestTune$nIter)
  return(logModel)
}

estimateKthRound <- function(gameData, gameNum, k){
  if(gameNum==2){
    features <- gameData[,2:(k-5)]
    P <- 0.6
  }
  else{
    features <- gameData[,1:(k-8)]
    P <- 0.43
  }
  model <-generateModel(gameData, gameNum, k)
  predFromModel <- as.integer(predict(model, features))-1
  genMode <- getMode(apply(features,1,FUN = getMode))
  predFromModel[is.na(predFromModel)] <- genMode
  QValuesForKth <- calculateQValues(gameData, gameNum, 0.8, k)
  for(p in seq(1, 50, by=2)){
    switched <- FALSE
      if (gameData[p,][,1] != 1 && gameNum==2){
        currentPredRow <-predFromModel[[(p+1)]]
        QMatrixRow <- QValuesForKth[[(p+1)]]
        currentPredColumn <-predFromModel[[p]]
        QMatrixColumn <- QValuesForKth[[p]]
        switched <- TRUE
      }
      else{
        currentPredRow <-predFromModel[[p]]
        QMatrixRow <- QValuesForKth[[p]]
        currentPredColumn <-predFromModel[[(p+1)]]
        QMatrixColumn <- QValuesForKth[[(p+1)]]
      }
    indxRow <- (which(QMatrixRow == max(QMatrixRow), arr.ind = TRUE))
    indxColumn <- (which(QMatrixColumn == max(QMatrixColumn), arr.ind = TRUE))
    bestRow<-indxRow[1]
    bestColumn<-indxColumn[2]
    #Not equal
    if((bestRow-1) != currentPredRow){
    if(switched==TRUE){
      mostFrq <-getMode(gameData[(p+1),2:k])
      cntFrq <-getMode(gameData[(p+1),2:k])
      prob <- cntFrq/(k-1)
      #if plays one strategy with high probability
      if(prob > P)
        {predFromModel[[(p+1)]] <- mostFrq
      }
      else{
        predFromModel[[(p+1)]] <- (bestRow-1) 
      }
    }
    else{
      if (gameNum==2){
        mostFrq <-getMode(gameData[(p),2:k])
        cntFrq <-getMode(gameData[(p),2:k])
        prob <- cntFrq/(k-1)
        #if plays one strategy with high probability
        if(prob >  P){
          predFromModel[[p]] <- mostFrq
        }
        else{
          predFromModel[[p]] <- (bestRow-1) 
        }
      }
      else{
        mostFrq <-getMode(gameData[(p),1:k-1])
        cntFrq <-getMode(gameData[(p),1:k-1])
        prob <- cntFrq/(k-1)
        #if plays one strategy with high probability
        if(prob > P){
          predFromModel[[p]] <- mostFrq
        }
        else{
          predFromModel[[p]] <- (bestRow-1) 
        }
      }
    }}
    #Not equal
    if((bestColumn-1) != currentPredColumn){
      if(switched==TRUE){
        mostFrq <-getMode(gameData[p,2:k])
        cntFrq <-getMode(gameData[p,2:k])
        prob <- cntFrq/(k-1)
        #if plays one strategy with high probability
          if(prob > P)
          {predFromModel[[p]] <- mostFrq
          }
          else{
            predFromModel[[p]] <- (bestRow-1) 
          }
      }
      else{
        if (gameNum==2){
          mostFrq <-getMode(gameData[(p+1),2:k])
          cntFrq <-getMode(gameData[(p+1),2:k])
          prob <- cntFrq/(k-1)
          #if plays one strategy with high probability
          if(prob > P){
            predFromModel[[(p+1)]] <- mostFrq
          }
          else{
            predFromModel[[(p+1)]] <- (bestRow-1) 
          }
        }
        else{
          mostFrq <-getMode(gameData[(p+1),1:k-1])
          cntFrq <-getMode(gameData[(p+1),1:k-1])
          prob <- cntFrq/(k-1)
          #if plays one strategy with high probability
          if(prob > P){
            predFromModel[[(p+1)]] <- mostFrq
          }
          else{
            predFromModel[[(p+1)]] <- (bestRow-1) 
          }
        }
      }}      
  }
  return(predFromModel)
}

calculateAccuracy<- function(gameData, gameNum, predictedRounds){
  accuracy <- 0
  if (gameNum == 2){
    gameData<- gameData[,2:51]
  }
  for (p in 31:50){
    actual<- gameData[,p]
    predicted<- predictedRounds[,(p-30)]
    for (i in 1:50){
    if(actual[i] == predicted[i]){
      accuracy=accuracy+1
      }
    }
  }
  return(accuracy/1000)
}
getMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
getModeCount <- function(x) {
  ux <- unique(x)
  max(tabulate(match(x, ux)))
}

estimateRounds <- function(gameData, gameNum){
  predicted <- cbind(estimateKthRound(gameData,gameNum, 31))
  for (r in 32:50){
    predicted <-cbind(predicted, cbind(estimateKthRound(gameData,gameNum, r)))
  }
  return(predicted)
}

estimate <- function(gameData, gameNum){
  for (k in 31:50){
    predictedRound <- cbind(estimateKthRound(gameData,gameNum, k))
    for (k2 in 2:50){
      predictedRound <-cbind(predictedRound, cbind(estimateKthRound(gameData,gameNum, k)))
    }
  
  final <- (apply(predictedRound,1,FUN = getMode))    
  if(k==31){
    predictedFinal <- cbind(final)
  }
  else{
    print(k)
    predictedFinal<- cbind(predictedFinal, final)
    
  }
  }
  return (predictedFinal)
}

#Just for for testing
#Game  1
#for (k in 1:10){
#  res <- estimateRounds(game1,1)
#  print(calculateAccuracy(game1, 1, res))
#}

#Game 2
#for (k in 1:10){
#  res <- estimateRounds(game2,2)
#  print(calculateAccuracy(game2, 2, res))
#}


#Calculating final
#Game 1
predGame1 <- estimate(game1,1)
accuracyGame1 <- calculateAccuracy(game1, 1, predGame1)
#Game 2
predGame2 <- estimate(game2,2)
accuracyGame2 <- calculateAccuracy(game2, 2, predGame2)

