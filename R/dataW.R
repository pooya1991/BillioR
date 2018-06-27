dataW <- function(x){
  d <- dataB
  n <- nrow(dataB)
  m <- n + 1
  dataB[m,1] <- m
  dataB[m,2] <- as.character(x)
  ndataB <- dataB
  save(ndataB,file = "ndataB.rda")
  drop_upload("mtcars.csv")
}