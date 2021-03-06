PranaDailySignal <- function(Stg,Share,Timeframe = "hourly",ShareID){
  requireNamespace("jsonlite")
  library(zoo)
  library(xts)
  library(TTR)
  library(quantmod)
  day <- 11
  RealTime <- function(x, t) {
    switch (x,
            time = result <- strftime(t,"%H:%M:%OS"),
            weekday = result <- strftime(t,"%A"),
            both = result <- strftime(t,"%H:%M:%OS %A")
    )
    result
  }
  Time <- function(vaght = "", rooz = "",HLC){
    result <- paste(vaght,rooz,sep = "")
    return(result)
  }
  Low <- "Low"
  High <- "High"
  Open <- "Open"
  Close <- "Close"
  HL <- "HL"
  HLC <- "HLC"
  HLCC <- "HLCC"
  Highest <- function(OHLC,Interval,COLUMN){
    switch (COLUMN,
            Close = result <- runMax(OHLC[,4],Interval),
            Open = result <- runMax(OHLC[,1], Interval),
            High = result <- runMax(OHLC[,2], Interval),
            Low = result <- runMax(OHLC[,3], Interval),
            HL = result <- runMax((OHLC[,2] + OHLC[,3])/2, Interval),
            HLC = result <- runMax((OHLC[,2] + OHLC[,3] + OHLC[,4])/3, Interval),
            HLCC = result <- runMax((OHLC[,2] + OHLC[,3]+ 2*OHLC[,4])/4, Interval)
    )
    result
  }
  Lowest <- function(OHLC,Interval,COLUMN){
    switch (COLUMN,
            Close = result <- runMin(OHLC[,4],Interval),
            Open = result <- runMin(OHLC[,1], Interval),
            High = result <- runMin(OHLC[,2], Interval),
            Low = result <- runMin(OHLC[,3], Interval),
            HL = result <- runMin((OHLC[,2] + OHLC[,3])/2, Interval),
            HLC = result <- runMin((OHLC[,2] + OHLC[,3] + OHLC[,4])/3, Interval),
            HLCC = result <- runMin((OHLC[,2] + OHLC[,3]+ 2*OHLC[,4])/4, Interval)
    )
    result
  }
  # Ichimoku Indicator Function
  ichimoku <- function(HLC, nFast=9, nMed=26, nSlow=52) {
    turningLine <- (runMax(Hi(HLC), nFast)+runMin(Lo(HLC), nFast))/2
    baseLine <- (runMax(Hi(HLC), nMed)+runMin(Lo(HLC), nMed))/2
    spanA <- lag((turningLine+baseLine)/2, nMed)
    spanB <- lag((runMax(Hi(HLC), nSlow)+runMin(Lo(HLC), nSlow))/2, nMed)
    plotSpan <- lag(Cl(HLC), nMed) #for plotting the original Ichimoku only
    laggingSpan <- lag(Cl(HLC), nMed)
    lagSpanA <- lag(spanA, nMed)
    lagSpanB <- lag(spanB, nMed)
    out <- cbind(turnLine=turningLine, baseLine=baseLine, spanA=spanA, spanB=spanB, plotSpan=plotSpan, laggingSpan=laggingSpan, lagSpanA, lagSpanB)
    colnames(out) <- c("turnLine", "baseLine", "spanA", "spanB", "plotLagSpan", "laggingSpan", "lagSpanA","lagSpanB")
    return (out)
  }
  # Indicators function
  Indis <- function(bb,FUN,n,m,p,q){
    switch(FUN,
           ClosePrice = result <- bb[,4],
           HighPrice = result <- bb[,2],
           LowPrice = result <- bb[,3],
           OpenPrice = result <- bb[,1],
           Volume = result <- bb[,5],
           ADX = result <- ADX(bb[,c(2,3,4)],n),
           Aroon = result <- aroon(bb[,c(2,3)],n),
           aroon = result <- aroon(bb[,c(2,3)],n),
           ATR = result <- ATR(bb[,c(2,3,4)],n),
           BBands = result <- BBands(bb[,c(2,3,4)],n = n,sd = m,maType = "SMA"),
           CCI = result <- CCI(bb[,c(2,3,4)],n = n,maType = "SMA",m),
           chaikinAD = result <- chaikinAD(bb[,c(2,3,4)],bb[,5]),
           chaikinVolatility = result <- chaikinVolatility(bb[,c(2,3)],n),
           CLV = result <- CLV(bb[,c(2,3,4)]),
           CMF = result <- CMF(bb[,c(2,3,4)],bb[,5],n),
           CMO = result <- CMO(bb[,4],n),
           DonchianChannel = result <- DonchianChannel(bb[,c(2,3)],n),
           DPO = result <- DPO(bb[,4],n,shift = m,maType = "SMA"),
           DVI = result <- DVI(bb[,4],n),
           EMV = result <- EMV(bb[,c(2,3)],bb[,5],n,maType = "SMA"),
           Highest = result <- Highest(OHLC = bb[,c(1:4)],Interval = n,COLUMN = m),
           ichimoku = result <- ichimoku(HLC = bb[,c(2,3,4)],nFast = n,nMed = m,nSlow = p),
           KST = result <- KST(bb[,4],n = c(n,n,n,floor((3 * n) / 2)),nROC = c(n, n + floor(n/2),2 * n, 2*n + floor(n/2)),nSig = m,maType = "SMA"),
           Lowest = result <- Lowest(OHLC = bb[,c(1:4)],Interval = n,COLUMN = m),
           lags = result <- lag(bb[,m],n),
           MACD = result <- MACD(bb[,4],n,m,p,"SMA"),
           MFI = result <- MFI(bb[,c(2,3,4)],bb[,5],n),
           momentum = result <- momentum(bb[,4],n),
           OBV = result <- OBV(bb[,4],bb[,5]),
           Pbands = result <- PBands(bb[,4],n,sd = m,maType = "SMA"),
           RealTime = result <- RealTime(x = n, t = index(bb)),
           ROC = result <- ROC(bb[,4],n),
           rollSFM = result <- rollSFM(bb[,4],n),
           RSI = result <- RSI(bb[,4],n,"SMA"),
           SAR = result <- SAR(bb[,c(2,3)],accel = c(n,m)),
           stoch = result <- stoch(bb[,c(2,3,4)],nFastK = n,nFastD = m,nSlowD = p,maType = "SMA"),
           SMI = result <- SMI(HLC,n = n,nFast = m,nSlow = p,nSig = q),
           TDI = result <- TDI(bb[,4],n,m),
           Time = result <- Time(vaght = n, rooz = m, HLC = bb),
           TRIX = result <- TRIX(bb[,4],n,m,"SMA"),
           ultimateOscillator = result <- ultimateOscillator(bb[,c(2,3,4)],n = c(5, 10, 20), wts = c(4, 2, 1)),
           VHF = result <- VHF(bb[,4],n),
           volatility = result <- volatility(bb[,c(7,2,3,4)],n),
           williamsAD = result <- williamsAD(bb[,c(2,3,4)]),
           WPR = result <- WPR(bb[,c(2,3,4)],n),
           ZigZag <- result <- ZigZag(bb[,c(2,3)],n),
           SMA = result <- SMA(bb[,4],n),
           EMA = result <- EMA(bb[,4],n),
           DEMA = result <- DEMA(bb[,4],n,m),
           WMA = result <- WMA(bb[,4],n),
           EVWMA = result <- EVWMA(bb[,4],n),
           ZLEMA = result <- ZLEMA(bb[,4],n),
           VWAP = result <- VWAP(bb[,4],bb[,5],n),
           VMA = result <- VMA(bb[,4],n),
           HMA = result <- HMA(bb[,4],n),
           ALMA = result <- ALMA(bb[,4],n,m,p)
    )
    result
  }
  Share <- as.character(Share)
  db <- jsonlite::fromJSON(Share)
  bb <- db[[1]]
  bb <- xts(bb[,1:5],order.by = as.POSIXct(bb[,6]))
  dd <- db[[2]]
  dd <- xts(dd[,1:5],order.by = as.Date(dd[,6]))
  # get the Strategy
  x <- as.character(Stg)
  Stg <- jsonlite::fromJSON(x)
  if(Stg$BUY$Status == "Set"){
    EnRuls <- Stg$BUY$Enter$Rules
    EnRels <- Stg$BUY$Enter$Rels
    n <- nrow(EnRuls)
    for (i in 1:n) {
      m <- length(EnRuls[[1]][[i]]$Indicator)
      qqq <-"Ind_1"
      for (j in 1:m) {
        Ind <- EnRuls[[1]][[i]]$Indicator[[j]]
        l <- nrow(EnRuls[[1]][[i]]$Parameters[[j]])
        indslag <- EnRuls[[1]][[i]]$Lag[[j]]
        qq <- ""
        if(l > 0){
          for (t in 1:l) {
            qq <- paste(qq,EnRuls[[1]][[i]]$Parameters[[j]][t,2],sep = ",")
          }
        }
        k <- which(Indo[,21] == Ind)
        if(indslag > 0){
          b <- paste("Ind_",j," <- Lag(Indis(bb = bb,FUN = Indo[k,1]",qq, ")[,Indo[k,22]],",indslag,")", sep = "")
        } else {
          b <- paste("Ind_",j," <- Indis(bb = bb,FUN = Indo[k,1]",qq, ")[,Indo[k,22]]", sep = "")
        }
        eval(parse(text = b))
      }
      m <- m - 1
      if(EnRuls[[2]][[i]][m] == "cross<"){
        for (s in 1:m) {
          qqq <- paste(qqq,EnRuls[[2]][[i]][s],"Ind_",s+1,sep = "")
        }
        a1 <- gsub("cross<",">=",qqq)
        b <- paste("c1 <- ",a1,sep = "")
        eval(parse(text = b))
        a2 <- gsub("cross<","<",qqq)
        b <- paste("c2 <- ",a2,sep = "")
        eval(parse(text = b))
        c2 <- lag(c2,1)
        b <- paste("rull_",i," <- (c1 & c2)",sep = "")
        eval(parse(text = b))
      }else if(EnRuls[[2]][[i]][m] == "cross>"){
        for (s in 1:m) {
          qqq <- paste(qqq,EnRuls[[2]][[i]][s],"Ind_",s+1,sep = "")
        }
        a1 <- gsub("cross>","<=",qqq)
        b <- paste("c1 <- ",a1,sep = "")
        eval(parse(text = b))
        a2 <- gsub("cross>",">",qqq)
        b <- paste("c2 <- ",a2,sep = "")
        eval(parse(text = b))
        c2 <- lag(c2,1)
        b <- paste("rull_",i," <- (c1 & c2)",sep = "")
        eval(parse(text = b))
      }else{
        for (s in 1:m) {
          qqq <- paste(qqq,EnRuls[[2]][[i]][s],"Ind_",s+1,sep = "")
        }
        b <- paste("rull_",i," <- (",qqq,")",sep = "")
        eval(parse(text = b))
      }
    }
    q <- "rull_1"
    if(n > 1){
      n <- n - 1
      for (s in 1:n) {
        q <- paste(q,EnRels[s],"rull_",s+1,sep = "")
      }
      q <- gsub("or", " | ", q)
      q <- gsub("and", " & ", q)
    }
    b <- paste("BUY_Enter <- (",q,")",sep = "")
    eval(parse(text = b))
    BB <- BUY_Enter[which(BUY_Enter),]
  }
  if(nrow(BB) > 0){
    t <- index(dd)[nrow(dd)]
    n <- index(dd)[nrow(dd) - day]
    m <- index(BB)[nrow(BB)]
    if(as.Date(m) > n){
      Result <- data.frame(Ticker = ShareID,Signal = "B",SignalDate = as.Date(m),PassedDays = as.numeric(t - as.Date(m)),Price = as.numeric(bb[m,4]))
    }else {
      Result <- data.frame(Ticker = ShareID,Signal = "N",SignalDate = as.Date(m),PassedDays = as.numeric(t - as.Date(m)),Price = as.numeric(bb[m,4]))
    }
  }else{
    Result <- data.frame(Ticker = ShareID,Signal = "N",SignalDate = Sys.Date(),PassedDays = 10,Price = as.numeric(dd[nrow(dd),4]))
  }
  
  res <- list(Result = Result)
  return(res)
}