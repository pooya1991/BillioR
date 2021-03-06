PranaBacktest <- function(Stg,UID,Share,Timeframe = "hourly",StartDate = "2014-01-01",EndDate = Sys.Date(),Vol = 1000,MaxPos = 10,Fee = T,Over = T,ReEnterType = 0,ReEnterAmm = 0){
  requireNamespace("jsonlite")
  library(zoo)
  library(xts)
  library(TTR)
  library(quantmod)
  library(dplyr)
  library(MASS)
  library(Boom)
  library(BoomSpikeSlab)
  library(bsts)
  time <- "time"
  weekday <- "weekday"
  boyh <- "both"
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
  ReEnt <- function(B,bb,type,amm){
    n <- nrow(B)
    tar <- index(B)
    nn <- -c(1:n)
    pp <- as.numeric(bb[tar,4])
    BB <- data.frame(Price = pp,Trade = nn)
    res <- xts(BB,order.by = tar)
    tar[n+1] <- Sys.time()
    ta <- tar[-1]
    nB <- vector()
    nBp <- vector()
    nBt <- vector()
    k <- 0
    if(type == "Percentage_Below"){
      for (i in 1:n) {
        baz <- paste(tar[i],ta[i],sep = "/")
        temp <- bb[baz]
        np <- as.numeric(temp[tar[i],4])
        m <- tar[i]
        while (!is.na(m)) {
          np <- floor(GeometricSequence(2,np,((100 - amm)/100))[2])
          baz <- paste(m,ta[i],sep = "/")
          ntemp <- temp[baz]
          m <- index(ntemp[which(ntemp[,3] < np & np < ntemp[,4]),])[1]
          if(!is.na(m)){
            k <- k + 1
            nB[k] <- as.character(m)
            nBp[k] <- np
            nBt[k] <- i
          }
        }
      }
    }else if(type == "PriceTick_Below"){
      for (i in 1:n) {
        baz <- paste(tar[i],ta[i],sep = "/")
        temp <- bb[baz]
        np <- as.numeric(temp[tar[i],4])
        m <- tar[i]
        while (!is.na(m)) {
          np <- np - amm
          baz <- paste(m,ta[i],sep = "/")
          ntemp <- temp[baz]
          m <- index(ntemp[which(ntemp[,3] < np & np < ntemp[,4]),])[1]
          if(!is.na(m)){
            k <- k + 1
            nB[k] <- as.character(m)
            nBp[k] <- np
            nBt[k] <- i
          }
        }
      }
    }else if(type == "Percentage_Above"){
      for (i in 1:n) {
        baz <- paste(tar[i],ta[i],sep = "/")
        temp <- bb[baz]
        np <- as.numeric(temp[tar[i],4])
        m <- tar[i]
        while (!is.na(m)) {
          np <- floor(GeometricSequence(2,np,((100 + amm)/100))[2])
          baz <- paste(m,ta[i],sep = "/")
          ntemp <- temp[baz]
          m <- index(ntemp[which(ntemp[,4] > np & np > ntemp[,3]),])[1]
          if(!is.na(m)){
            k <- k + 1
            nB[k] <- as.character(m)
            nBp[k] <- np
            nBt[k] <- i
          }
        }
      }
    }else if(type == "PriceTick_Above"){
      for (i in 1:n) {
        baz <- paste(tar[i],ta[i],sep = "/")
        temp <- bb[baz]
        np <- as.numeric(temp[tar[i],4])
        m <- tar[i]
        while (!is.na(m)) {
          np <- np + amm
          baz <- paste(m,ta[i],sep = "/")
          ntemp <- temp[baz]
          m <- index(ntemp[which(ntemp[,4] > np & np > ntemp[,3]),])[1]
          if(!is.na(m)){
            k <- k + 1
            nB[k] <- as.character(m)
            nBp[k] <- np
            nBt[k] <- i
          }
        }
      }
    }
    BB <- data.frame(Price = nBp,Trade = nBt)
    result <- xts(BB,order.by = as.POSIXct(nB))
    result <- rbind(res,result)
    result
  }
  MaxPosition <- function(B,S,MaxPos){
    n <- nrow(B)
    val <- rep(1,n)
    k <- 1
    while (k <= n) {
      s <- index(B)[k]
      e <- index(S[S[,2] == k,])
      ttt <- which(index(S) <= e & val > 0)
      yyy <- ttt[index(S[ttt,]) < s]
      b <- MaxPos - length(ttt) + length(yyy)
      if(b >= 0){
        k <- k + b + 1
      }else{
        tt <- k + MaxPos
        k <- MaxPos + k + abs(b)
        ss <- k - 1
        val[tt:ss] <- 0
      }
    }
    val
  }
  Report <- function(Result,dd,MaxPos){
    n <- nrow(Result) / 2
    Trade <- rep(c(1:n),each = 2)
    SuccessRate <- nrow(Result[Result[,4] > 0 & !is.na(Result[,4]),]) / n
    MeanProfit <- mean(Result[Result[,4] > 0 & !is.na(Result[,4]),4])
    MeanLost <- mean(Result[Result[,4] <= 0 & !is.na(Result[,4]),4])
    MaxDrwDwn <- min(Result[Result[,4] <= 0 & !is.na(Result[,4]),4])
    TotalRet <- cumprod(1 + Result[!is.na(Result[,4]),4])[n] - 1
    TurnOver <- sum(Result[,1]) * Vol
    TotalVol <- n * Vol
    OpenPos <- vector()
    a1 <- c(1:n)
    a2 <- 2*a1
    reE <- Result[a2,]
    reS <- Result[a1,]
    reE[,3] <- as.POSIXct(reE[,3])
    reS[,3] <- as.POSIXct(reS[,3])
    for (i in 1:n) {
      t <- reE[i,3]
      tt <- reS[j,3]
      a <- which((reE[,3] <= tt) & (reE[,3] >= t))
      OpenPos[i] <- length(a)
    }
    MaxOpenPos <- max(OpenPos)
    MaxOpenPos <- min(MaxOpenPos,MaxPos)
    if(MaxOpenPos == 0){
      MaxOpenPos <- 1
    }
    temtar <- vector()
    doreneg <- vector()
    for (i in 1:n) {
      baz <- paste(as.Date(reS[i,3]),as.Date(reE[i,3]),sep = "/")
      doreneg[2*i] <- length(index(dd[baz]))
      temtar <- c(temtar,index(dd[baz]))
    }
    m <- length(unique(temtar))
    l <- nrow(dd)
    AcToDeAc <- m / (l - m)
    radif <- c(1:nrow(Result))
    Nat <- vector()
    Nat[a2] <- (Result[a2,1]-Result[a1,1])*Vol
    lis <- data.frame(radif,Result[,3],Result[,2],Result[,1],rep(Vol,nrow(Result)),(Result[,1]*Vol),Nat,doreneg,Trade,Result[,4])
    colnames(lis) <- c("RowNumber","DateTime","Side","Price","OrderVolume","OrderValue","ProfitOrLoss","PreservePeriods","BuyRowNumber","Return")
    Natije <- data.frame(MaxOpenPos,MaxDrwDwn,AcToDeAc,SuccessRate,MeanProfit,TotalRet,TurnOver,MeanLost,TotalVol)
    names(Natije) <- c("MaxOpenPosition","MaxConsecutiveDecline","ActiveToDeactiveDaysRatio","SuccessRate","MeanProfit","TotalReturn","TurnOver","MeanLoss","TotalVolume")
    finalresult <- list(overal_result = Natije, details= lis)
    finalresult
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
    ExRuls <- Stg$BUY$Exit$Rules
    ExRels <- Stg$BUY$Exit$Rels
    StpLst <- Stg$BUY$Exit$StopLoss
    TkPrft <- Stg$BUY$Exit$TakeProfit
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
    if(length(BUY_Enter) < 1){
      lis <- data.frame(0,0,0,0,0,0,0,0,0,0)
      colnames(lis) <- c("RowNumber","DateTime","Side","Price","OrderVolume","OrderValue","ProfitOrLoss","PreservePeriods","BuyRowNumber","Return")
      Natije <- list(0,0,0,0,0,0,0,0,0,lis)
      names(Natije) <- c("MaxOpenPosition","MaxConsecutiveDecline","ActiveToDeactiveDaysRatio","SuccessRate","MeanProfit","TotalReturn","TurnOver","MeanLoss","TotalVolume","Detail")
      return(Natije)
    }
    BB <- BUY_Enter[which(BUY_Enter),]
    if(length(BB) < 1){
      lis <- data.frame(0,0,0,0,0,0,0,0,0,0)
      colnames(lis) <- c("RowNumber","DateTime","Side","Price","OrderVolume","OrderValue","ProfitOrLoss","PreservePeriods","BuyRowNumber","Return")
      Natije <- list(0,0,0,0,0,0,0,0,0,lis)
      names(Natije) <- c("MaxOpenPosition","MaxConsecutiveDecline","ActiveToDeactiveDaysRatio","SuccessRate","MeanProfit","TotalReturn","TurnOver","MeanLoss","TotalVolume","Detail")
      return(Natije)
    }
    #check overbuy of the signal
    if(Over){
      ta <- as.Date(index(BB))
      n <- length(ta)
      ov <- vector("numeric",n)
      ov[dd[ta,4] / dd[ta,1] < 1.05] <- 1
      BB <- BB[ov == 1,]
    }
    #ReEnter in a position
    if(ReEnterType =="Percentage_Above" | ReEnterType =="Percentage_Below" | ReEnterType =="PriceTick_Above" | ReEnterType =="PriceTick_Below"){
      B <- ReEnt(B=BB,bb=bb,type = ReEnterType,amm = ReEnterAmm)
    }else{
      n <- nrow(BB)
      tar <- index(BB)
      nn <- -c(1:n)
      pp <- as.numeric(bb[tar,4])
      BB <- data.frame(Price = pp,Trade = nn)
      B <- xts(BB,order.by = tar)
    }
    #Evaluate Exit Conditions
    C <- vector()
    nEstp <- vector()
    nEstpP <- vector()
    nEstpt <- vector()
    nEtkp <- vector()
    nEtkpP <- vector()
    nEtkpt <- vector()
    if(nrow(B) > 0){
      if(is.null(ExRuls)) {
        n = 0
      } else {
        n <- nrow(ExRuls)
      }
      if(n > 0){
        for (i in 1:n) {
          m <- length(ExRuls[[1]][[i]]$Indicator)
          qqq <-"Ind_1"
          for (j in 1:m) {
            Ind <- ExRuls[[1]][[i]]$Indicator[[j]]
            l <- nrow(ExRuls[[1]][[i]]$Parameters[[j]])
            qq <- ""
            for (t in 1:l) {
              qq <- paste(qq,ExRuls[[1]][[i]]$Parameters[[j]][t,2],sep = ",")
            }
            k <- which(Indo[,21] == Ind)
            b <- paste("Ind_",j," <- Indis(bb = bb,FUN = Indo[k,1]",qq, ")[,Indo[k,22]]", sep = "")
            eval(parse(text = b))
          }
          m <- m - 1
          if(ExRuls[[2]][m] == "cross<"){
            for (s in 1:m) {
              qqq <- paste(qqq,ExRuls[[2]][[i]][s],"Ind_",s+1,sep = "")
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
          }else if(ExRuls[[2]][[i]][m] == "cross>"){
            for (s in 1:m) {
              qqq <- paste(qqq,ExRuls[[2]][[i]][s],"Ind_",s+1,sep = "")
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
              qqq <- paste(qqq,ExRuls[[2]][[i]][s],"Ind_",s+1,sep = "")
            }
            b <- paste("rull_",i," <- (",qqq,")",sep = "")
            eval(parse(text = b))
          }
        }
        q <- "rull_1"
        if(nrow(ExRuls) > 1){
          n <- n - 1
          for (s in 1:n) {
            q <- paste(q,EnRels[s],"rull_",s+1,sep = "")
          }
          q <- gsub("or", " | ", q)
          q <- gsub("and", " & ", q)
        }
        b <- paste("BUY_ExitRu <- (",q,")",sep = "")
        eval(parse(text = b))
        C <- index(BUY_ExitRu[which(BUY_ExitRu),])
      }
      #Take Profit and Stop Lost
      n <- nrow(B)
      for (i in 1:n) {
        tar <- index(B)[i]
        pri <- B[i,1]
        if(is.null(StpLst)){
          Stp <- as.numeric(pri * 0)
        }else {
          if(StpLst[1,1] == "Percent"){
            Stp <- as.numeric(floor(pri * ((100 - as.numeric(StpLst[1,2]))/100)))
          }else if(StpLst[1,1] == "PriceTick"){
            Stp <- as.numeric(pri - as.numeric(StpLst[1,2]))
          } else {
            Stp <- 0
          }
        }
        if(is.null(TkPrft)){
          Prf <- as.numeric(pri * 1000)
        }else {
          if(TkPrft[1,1] == "Percent"){
            Prf <- as.numeric(floor(pri * ((100 + as.numeric(TkPrft[1,2]))/100)))
          }else if(TkPrft[1,1] == "PriceTick"){
            Prf <- as.numeric(pri + as.numeric(TkPrft[1,2]))
          } else {
            Prf <- as.numeric(pri * 1000)
          }
        }
        baz <- paste(tar,EndDate,sep = "/")
        temp <- bb[baz]
        m <- index(temp)[which(temp[,3] < Stp)[1]]
        if(is.na(m)){
          m <- index(tail(temp,1))
        }
        l <- index(temp)[which(temp[,2] > Prf)[1]]
        if(is.na(l)){
          l <- index(tail(temp,1))
        }
        nEstp[i] <- as.character(m)
        nEstpP[i] <- as.numeric(temp[m,4])
        nEstpt[i] <- i
        nEtkp[i] <- as.character(l)
        nEtkpP[i] <- as.numeric(temp[l,4])
        nEtkpt[i] <- i
      }
      C <- as.character(C)
      Etar <- c(C,nEstp,nEtkp)
      etemp <- rep(0,length(C))
      Etra <- c(etemp,nEstpt,nEtkpt)
      Epri <- vector()
      m <- length(Etar)
      for (i in 1:m) {
        Epri[i] <- as.numeric(bb[Etar[i],4])
      }
      val <- rep(1,m)
      forush <- xts(data.frame(Price = Epri,Trade = Etra, valid = val),order.by = as.POSIXlt(Etar))
      n <- nrow(B)
      S <- as.data.frame(B[1,])
      STar <- vector()
      for (i in 1:n) {
        s <- index(B)[i]
        f1 <- as.vector(forush[,2] == 0)
        f2 <- as.vector(forush[,2] == i)
        f3 <- index(forush) > s
        f4 <- f1+f2 >= 1
        f5 <- as.vector(forush[,3] > 0)
        ff <- which((f3 * f4 * f5) > 0)[1]
        forush[ff,3] <- 0
        S[i,] <- data.frame(as.numeric(forush[ff,1]),i)
        STar[i] <- as.character(index(forush[ff,]))
      }
      S <- xts(S,order.by = as.POSIXct(STar))
      val <- MaxPosition(B = B,S = S,MaxPos = MaxPos)
      BBB <- B[val > 0,]
      SSS <- S[val[S[,2]] > 0,]
      n <- nrow(BBB)
      Tar <- vector()
      Transaction <- as.data.frame(B[1,])
      TT <- rep(c("B","S"),n)
      Ret <- vector()
      for (i in 1:n) {
        ii <- (2 * i) - 1
        jj <- 2 * i
        Tar[jj] <- as.character(index(SSS[i,]))
        Tar[ii] <- as.character(index(BBB[i,]))
        Transaction[ii,] <- B[i,]
        Transaction[jj,] <- S[i,]
        Ret[[ii]] <- NA
        Ret[jj] <- (as.numeric(S[i,1]) / as.numeric(B[i,1])) - 1
      }
      Result <- data.frame(Transaction,TT,Tar,Ret)
      Result <- Result[,-2]
      rownames(Result) <- NULL
      repo <- Report(Result = Result,dd = dd,MaxPos = MaxPos)
    }
  }
  repo
}
