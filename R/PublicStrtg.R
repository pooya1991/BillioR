PublicStrtg <- function(n){
  library(jsonlite)
  if(n == 1){
    m <- nrow(Price)
    res <- list()
    for (i in 1:m) {
      res[[i]] <- Price[i,]
    }
    res
  }
}