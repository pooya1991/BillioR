require(jsonlite)
require(zoo)
require(xts)
require(TTR)

Backtest <- function(user,name,share,sdate,edate){
  url <- "http://billionet.us/msa/api/stockhistory/065aa4a5c4c6f6a44fea1645ad9cc2c6/"
  lin <- paste0(url,share,sep = "")
  vv <- fromJSON(lin)
  if(vv[[1]][[1]] == 0){
    cc <- vv[[1]][[3]]
    cc <- as.data.frame(sapply(cc, as.numeric))
    tar <- as.Date(as.character(cc[,8]),"%Y%m%d")
    bb <- xts(cc[,1:7],tar)
    colnames(bb) <- c("First","High","Low","Close","Value","Volume","Open")
    HLC <- bb[,c(2,3,4)]
    OHLC <- bb[,c(7,2,3,4)]
    HL <- bb[,c(2,3)]
    C <- bb[,4]
  }
  url1 <- "http://billionet.us/msa/api/strategies/065aa4a5c4c6f6a44fea1645ad9cc2c6/"
  lin1 <- paste0(url1,user,name,share,sep = "")
  zz <- fromJSON(lin)
}