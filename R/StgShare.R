StgShare <- function(stg,share){
  d <- PublicStrategies[[stg]]
  row.names(d) <- NULL
  res <- d[d[,1] == share,-2]
  if(nrow(res) > 0){
    res
  }else{
    res <- data.frame(0,0,0,0)
    colnames(res) <- colnames(d)[-2]
    res
  }
}