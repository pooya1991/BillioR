PublicBehgozin <- function(stg,SuccessRate = 0,CumulativeReturn = 0){
  d <- PublicStrategies[[stg]][,-2]
  res <- d[d[,2] > SuccessRate/100,]
  res <- res[res[,3] > CumulativeReturn/100,]
  if(nrow(res) > 0){
    split(res, seq(nrow(res)))
  }else{
    res <- data.frame(0,0,0,0)
    colnames(res) <- colnames(d)
    res
  }
}