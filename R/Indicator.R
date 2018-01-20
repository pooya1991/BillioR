library(jsonlite)
library(zoo)
library(xts)
library(TTR)

Indicator <- function(share,FUN,n,m,p,q){
  url <- "http://billionet.us/msa/api/stockhistory/065aa4a5c4c6f6a44fea1645ad9cc2c6/"
  lin <- paste(url,share,sep = "")
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
    switch(FUN,
           ADX = result <- ADX(HLC,n),
           Aroon = result <- aroon(HL,n),
           ATR = result <- ATR(HLC,n),
           BBands = result <- BBands(HLC,n,m,p),
           CCI = result <- CCI(HLC,n,m,p),
           chaikinAD = result <- chaikinAD(HLC),
           chaikinVolatility = result <- chaikinVolatility(HL,n),
           CLV = result <- CLV(HLC),
           CMF = result <- CMF(HLC,n),
           CMO = result <- CMO(C,n),
           DonchianChannel = result <- DonchianChannel(HL,n),
           DPO = result <- DPO(C,n,m,p),
           DVI = result <- DVI(C,n),
           EMV = result <- EMV(HL,n,m),
           GMMA = result <- GMMA(C),
           KST = result <- KST(C,n,m,p),
           lags = result <- lags(C,n),
           MACD = result <- MACD(C,n,m,p,q),
           MFI = result <- MFI(HLC,n),
           OBV = result <- OBV(C),
           Pbands = result <- PBands(C,n,m,p),
           ROC = result <- ROC(C,n),
           rollSFM = result <- rollSFM(C,n),
           RSI = result <- RSI(C,n,m),
           SAR = result <- SAR(HL,n,m),
           stoch = result <- stoch(HLC,n,m,p,q),
           TDI = result <- TDI(C,n,m),
           TRIX = result <- TRIX(C,n,m,p),
           ultimateOscillator = result <- ultimateOscillator(HLC),
           VHF = result <- VHF(C,n),
           volatility = result <- volatility(OHLC,n),
           williamsAD = result <- williamsAD(HLC),
           WPR = result <- WPR(HLC,n),
           ZigZag <- result <- ZigZag(HL,n),
           SMA = result <- SMA(C,n),
           EMA = result <- EMA(C,n),
           DEMA = result <- DEMA(C,n,m),
           WMA = result <- WMA(C,n),
           EVWMA = result <- EVWMA(C,n),
           ZLEMA = result <- ZLEMA(C,n),
           VWAP = result <- VWAP(C,n),
           VMA = result <- VMA(C,n),
           HMA = result <- HMA(C,n),
           ALMA = result <- ALMA(C,n,m,p)
    )
    if(dim(result)[1] > 10){
      ErrorCode <- 0
      ErrorDetail <- "NO Error"
      result <- result[complete.cases(result),]
      result <- as.data.frame(result)
    }else{
      ErrorCode <- 1
      ErrorDetail <- "Cannot Compute"
      result <- "Nothing"
    }
  }else{
    ErrorCode <- 1
    ErrorDetail <- vv[[1]][[2]]
    result <- "Nothing"
  }
  list(ErrorCode,ErrorDetail,result)
}
