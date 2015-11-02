library(dplyr)

moveIt <- function(x, y) {
  deltaX <- sample(c(-1, 1), 1)
  deltaY <- sample(c(-1, 1), 1)
  
  newX <- x + deltaX
  newY <- y + deltaY
  
  return(list(x = newX, y = newY))
}

makePath <- function(startX, startY, numMoves, i){
  thisPathData <- data.frame(i = i, Move = 0, X = startX, Y = startY)
  for (m in 1:numMoves) {
    thisMove <- moveIt(thisPathData$X[m], thisPathData$Y[m])
    thisPathData[m+1,] <- c(i, m, thisMove$x, thisMove$y)
  }
  return(thisPathData[nrow(thisPathData),])
}

getMaxDist <- function(startX, startY, numMoves, i){
  thisPathData <- data.frame(i = i, Move = 0, X = startX, Y = startY)
  for (m in 1:numMoves) {
    thisMove <- moveIt(thisPathData$X[m], thisPathData$Y[m])
    thisPathData[m+1,] <- c(i, m, thisMove$x, thisMove$y)
  }
  thisPathData <- thisPathData %>%
    mutate(Distance = sqrt(X^2 + Y^2)) %>%
    arrange(desc(Distance))
  return(thisPathData[1,])
}

# prob of 3 or 5 blocks away after 10 moves
numIterations <- 10000
numMoves <- 10
pathData <- data.frame(i = NA, Move = NA, X = NA, Y = NA, row.names = NULL)
for (i in 1:numIterations) {
  pathData[i,] <- makePath(0, 0, numMoves, i)
}

pathData <- pathData %>%
  mutate(Distance = sqrt(X^2 + Y^2))
dist3 <- pathData %>%
  filter(Distance > 3)
nDist3 <- nrow(dist3) / nrow(pathData)

dist5 <- pathData %>%
  filter(Distance > 5)
nDist5 <- nrow(dist5) / nrow(pathData)

# prob of at least 5 blocks away within 10 moves
numIterations <- 10000
numMoves <- 10
for (i in 1:numIterations) {
  pathData[i,] <- getMaxDist(0, 0, numMoves, i)
}
dist5 <- pathData %>%
  filter(Distance > 5)
ndist5 <- nrow(dist5) / nrow(pathData)


# prob of at least 10 blocks away within 60 moves
numIterations <- 10000
numMoves <- 60
pathData <- data.frame(i = NA, Move = NA, X = NA, Y = NA, Distance = NA, row.names = NULL)
for (i in 1:numIterations) {
  pathData[i,] <- getMaxDist(0, 0, numMoves, i)
}
dist10 <- pathData %>%
  filter(Distance > 10)
nDist10 <- nrow(dist10) / nrow(pathData)
