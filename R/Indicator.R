Indicator <- function(share,FUN,n,m,p,q){
  url <- "http://billionet.us/msa/api/stockhistory/065aa4a5c4c6f6a44fea1645ad9cc2c6/"
  lin <- paste(url,share,sep = "")
  library(jsonlite)
  library(zoo)
  library(xts)
  library(TTR)
  library(quantmod)
  # Ichimoku Indicator Function
  ichimoku <- function(HLC, nFast=9, nMed=26, nSlow=52) {
    turningLine <- (runMax(Hi(HLC), nFast)+runMin(Lo(HLC), nFast))/2
    baseLine <- (runMax(Hi(HLC), nMed)+runMin(Lo(HLC), nMed))/2
    spanA <- lag((turningLine+baseLine)/2, nMed)
    spanB <- lag((runMax(Hi(HLC), nSlow)+runMin(Lo(HLC), nSlow))/2, nMed)
    plotSpan <- lag(Cl(HLC), -nMed) #for plotting the original Ichimoku only
    laggingSpan <- lag(Cl(HLC), nMed)
    lagSpanA <- lag(spanA, nMed)
    lagSpanB <- lag(spanB, nMed)
    out <- cbind(turnLine=turningLine, baseLine=baseLine, spanA=spanA, spanB=spanB, plotSpan=plotSpan, laggingSpan=laggingSpan, lagSpanA, lagSpanB)
    colnames(out) <- c("turnLine", "baseLine", "spanA", "spanB", "plotLagSpan", "laggingSpan", "lagSpanA","lagSpanB")
    return (out)
  }
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
    switch(FUN,
           ADX = result <- ADX(HLC,n),
           Aroon = result <- aroon(HL,n),
           ATR = result <- ATR(HLC,n),
           BBands = result <- BBands(HLC,n = n,sd = m,maType = "SMA"),
           CCI = result <- CCI(HLC,n = n,maType = "SMA",m),
           chaikinAD = result <- chaikinAD(HLC,V),
           chaikinVolatility = result <- chaikinVolatility(HL,n),
           CLV = result <- CLV(HLC),
           CMF = result <- CMF(HLC,V,n),
           CMO = result <- CMO(C,n),
           DonchianChannel = result <- DonchianChannel(HL,n),
           DPO = result <- DPO(C,n,shift = m,maType = "SMA"),
           DVI = result <- DVI(C,n),
           EMV = result <- EMV(HL,V,n,maType = "SMA"),
           ichimoku = result <- ichimoku(HLC = HLC,nFast = n,nMed = m,nSlow = p),
           KST = result <- KST(C,n = c(n,n,n,floor((3 * n) / 2)),nROC = c(n, n + floor(n/2),2 * n, 2*n + floor(n/2)),nSig = m,maType = "SMA"),
           lags = result <- lag(bb[,m],n),
           MACD = result <- MACD(C,n,m,p,"SMA"),
           MFI = result <- MFI(HLC,V,n),
           momentum = result <- momentum(C,n),
           OBV = result <- OBV(C,V),
           Pbands = result <- PBands(C,n,sd = m,maType = p),
           ROC = result <- ROC(C,n),
           rollSFM = result <- rollSFM(C,n),
           RSI = result <- RSI(C,n,"SMA"),
           SAR = result <- SAR(HL,accel = c(n,m)),
           stoch = result <- stoch(HLC,nFastK = n,nFastD = m,nSlowD = p,maType = "SMA"),
           SMI = result <- SMI(HLC,n = n,nFast = m,nSlow = p,nSig = q),
           TDI = result <- TDI(C,n,m),
           TRIX = result <- TRIX(C,n,m,"SMA"),
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
