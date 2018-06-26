dataW <- function(x){
  d <- dataB
  n <- nrow(dataB)
  m <- n + 1
  dataB[m,1] <- m
  dataB[m,2] <- as.character(x)
  ls()
}