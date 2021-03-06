Indicator <- function(share,FUN,n,m,p,q){
  url <- "http://billionet.us/msa/api/stockhistory/065aa4a5c4c6f6a44fea1645ad9cc2c6/"
  lin <- paste(url,share,sep = "")
  library(jsonlite)
  library(zoo)
  library(xts)
  library(TTR)
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
    V <- bb[,6]
    vvvv <- bb[,5]
    switch(FUN,
           ADX = result <- ADX(HLC,n),
           Aroon = result <- aroon(HL,n),
           ATR = result <- ATR(HLC,n),
           BBands = result <- BBands(HLC,n = n,sd = m,maType = p),
           CCI = result <- CCI(HLC,n = n,maType = p,m),
           chaikinAD = result <- chaikinAD(HLC,V),
           chaikinVolatility = result <- chaikinVolatility(HL,n),
           CLV = result <- CLV(HLC),
           CMF = result <- CMF(HLC,V,n),
           CMO = result <- CMO(C,n),
           DonchianChannel = result <- DonchianChannel(HL,n),
           DPO = result <- DPO(C,n,shift = m,maType = p),
           DVI = result <- DVI(C,n),
           EMV = result <- EMV(HL,V,n,maType = m),
           KST = result <- KST(C,n = c(n,n,n,floor((3 * n) / 2)),nROC = c(n, n + floor(n/2),2 * n, 2*n + floor(n/2)),nSig = m,maType = p),
           lags = result <- lags(bb[,as.character(m)],as.numeric(n)),
           MACD = result <- MACD(C,n,m,p,q),
           MFI = result <- MFI(HLC,V,n),
           OBV = result <- OBV(C,V),
           Pbands = result <- PBands(C,n,sd = m,maType = p),
           ROC = result <- ROC(C,n),
           rollSFM = result <- rollSFM(C,n),
           RSI = result <- RSI(C,n,m),
           SAR = result <- SAR(HL,accel = c(n,m)),
           stoch = result <- stoch(HLC,nFastK = n,nFastD = m,nSlowD = p,maType = q),
           TDI = result <- TDI(C,n,m),
           TRIX = result <- TRIX(C,n,m,p),
           ultimateOscillator = result <- ultimateOscillator(HLC,n = c(5, 10, 20), wts = c(4, 2, 1)),
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
      n <- dim(result)[1]
      m <- dim(result)[2]
      date <- index(result)
      result <- as.data.frame(result)
      row.names(result) <- NULL
      colnames(result) <- gsub("\\.","",colnames(result))
      nat <- list()
      for(i in 1:n){
        if(m == 1){
          a <- data.frame(result[i,])
          colnames(a) <- FUN
          q <- a
        }else{
          q <- result[i,]
        }
        nat[[i]] <- list(date[i],q)
        names(nat[[i]]) <- c("Date","Data")
      }
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
  list(ErrorCode,ErrorDetail,nat)
}
