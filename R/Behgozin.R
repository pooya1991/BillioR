Behgozin <- function (x,n,m) 
{
  library(jsonlite)
  library(zoo)
  library(xts)
  library(TTR)
  library(quantmod)
  # get the data from PHP
  x <- as.character(x)
  v <- fromJSON(x)
  # Accepted Success Rate for User
  AccSu <- n
  # Accepted Cumulative Return for User
  AccCu <- m
  StartDate <- v$start_date
  EndDtae <- v$end_date
  share <- v$stock
  # link of downloading historical data of stock
  url <- "http://billionet.us/msa/api/stockhistory/065aa4a5c4c6f6a44fea1645ad9cc2c6/"
  lin <- paste(url, share, sep = "")
  vv <- fromJSON(lin)
  # convert to xts historical data of the stock
  if (vv[[1]][[1]] == 0) {
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
  }
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
  # Indicators function
  Indis <- function(bb,FUN,n,m,p,q){
    switch(FUN,
           ADX = result <- ADX(bb[,c(2,3,4)],n),
           Aroon = result <- aroon(bb[,c(2,3)],n),
           aroon = result <- aroon(bb[,c(2,3)],n),
           ATR = result <- ATR(bb[,c(2,3,4)],n),
           BBands = result <- BBands(bb[,c(2,3,4)],n = n,sd = m,maType = "SMA"),
           CCI = result <- CCI(bb[,c(2,3,4)],n = n,maType = "SMA",m),
           chaikinAD = result <- chaikinAD(bb[,c(2,3,4)],bb[,6]),
           chaikinVolatility = result <- chaikinVolatility(bb[,c(2,3)],n),
           CLV = result <- CLV(bb[,c(2,3,4)]),
           CMF = result <- CMF(bb[,c(2,3,4)],bb[,6],n),
           CMO = result <- CMO(bb[,4],n),
           DonchianChannel = result <- DonchianChannel(bb[,c(2,3)],n),
           DPO = result <- DPO(bb[,4],n,shift = m,maType = "SMA"),
           DVI = result <- DVI(bb[,4],n),
           EMV = result <- EMV(bb[,c(2,3)],bb[,6],n,maType = "SMA"),
           ichimoku = result <- ichimoku(HLC = bb[,c(2,3,4)],nFast = n,nMed = m,nSlow = p),
           KST = result <- KST(bb[,4],n = c(n,n,n,floor((3 * n) / 2)),nROC = c(n, n + floor(n/2),2 * n, 2*n + floor(n/2)),nSig = m,maType = "SMA"),
           lags = result <- lag(bb[,m],n),
           MACD = result <- MACD(bb[,4],n,m,p,"SMA"),
           MFI = result <- MFI(bb[,c(2,3,4)],bb[,6],n),
           momentum = result <- momentum(bb[,4],n),
           OBV = result <- OBV(bb[,4],bb[,6]),
           Pbands = result <- PBands(bb[,4],n,sd = m,maType = "SMA"),
           ROC = result <- ROC(bb[,4],n),
           rollSFM = result <- rollSFM(bb[,4],n),
           RSI = result <- RSI(bb[,4],n,"SMA"),
           SAR = result <- SAR(bb[,c(2,3)],accel = c(n,m)),
           stoch = result <- stoch(bb[,c(2,3,4)],nFastK = n,nFastD = m,nSlowD = p,maType = "SMA"),
           SMI = result <- SMI(HLC,n = n,nFast = m,nSlow = p,nSig = q),
           TDI = result <- TDI(bb[,4],n,m),
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
           VWAP = result <- VWAP(bb[,4],n),
           VMA = result <- VMA(bb[,4],n),
           HMA = result <- HMA(bb[,4],n),
           ALMA = result <- ALMA(bb[,4],n,m,p)
    )
    result
  }
  # Indicators of the Strategy
  Inds <- v$indicators
  n <- length(Inds)
  for (i in 1:n) {
    m <- nrow(Inds[[i]][[4]])
    qqq <- "bb = bb, FUN = Inds[[i]][[1]]"
    if (m > 1) {
      for (j in 1:m) {
        qqq <- paste0(qqq,",", Inds[[i]][[4]][j,1], sep = "")
      }
    }else {
      qqq <- paste(qqq,",",Inds[[i]][[4]][1,1])
    }
    if(length(qqq) > 1){
      qqq <- paste(qqq,collapse="")
    }
    # proper data of each indicator
    b <- paste(Inds[[i]][[2]], " <- Indis(",qqq, ")", sep = "")
    eval(parse(text = b))
    b <- paste("bbbb <- ncol(",Inds[[i]][[2]],")")
    eval(parse(text = b))
    if(bbbb > 1){
      # remove dot from col names
      b <- paste("nn <- colnames(",Inds[[i]][[2]],")")
      eval(parse(text = b))
      nn <- gsub("\\.","",nn)
      b <- paste("colnames(",Inds[[i]][[2]],") <- nn")
      eval(parse(text = b))
      # Seperate each column as a vector
      for (j in 1:bbbb) {
        b <- paste("mm <- colnames(",Inds[[i]][[2]],")[j]")
        eval(parse(text = b))
        nindname <- paste(Inds[[i]][[2]],"_",mm,sep = "")
        b <- paste(nindname," <- ",Inds[[i]][[2]],"[,j]",sep = "")
        eval(parse(text = b))
      }
    }else{
      nindname <- paste(Inds[[i]][[2]],"_",Inds[[i]][[1]],sep = "")
      b <- paste(nindname," <- ",Inds[[i]][[2]],sep = "")
      eval(parse(text = b))
    }
  }
  # Get the Relations
  Rels <- v$relations
  n <- length(Rels)
  # Type of the relations ex. before sell
  type <- vector()
  # Name of each rull which determine the type and orde
  Rules <- vector()
  for (i in 1:n) {
    # Type and Order of relations
    nam <- paste(Rels[[i]][[2]], Rels[[i]][1], sep = "")
    Rules[i] <- nam
    type[i] <- Rels[[i]][[2]]
    q <- Rels[[i]][[3]]
    q[q == "="] <- "=="
    d <- paste(q,collapse = " ")
    b <- paste(nam, " <- ", d)
    eval(parse(text = b))
  }
  b <- ""
  for (i in 1:n) {
    b <- paste(b, ",", Rules[i], sep = "")
  }
  b <- substr(b, 2, nchar(b))
  # rull is the dataframe of all the relus user made
  b <- paste("rull <- cbind(", b, ")", sep = "")
  eval(parse(text = b))
  Mtype <- unique(type)
  l <- length(Mtype)
  for (i in 1:l) {
    q <- which(type == Mtype[i])
    # g is the all rull of the same type ex. before sell
    g <- rull[, q]
    k <- dim(g)[2]
    if (k > 1) {
      e <- rowSums(g) == k
      e <- xts(e,order.by = index(g))
    }
    else {
      e <- g
    }
    b <- paste(Mtype[i], " <- e", sep = "")
    eval(parse(text = b))
  }
  # Stop Loss and Take Profit Conditions - LimP --> Profit
  Lims <- v$limites
  if(Lims[[1]] > 0){
    LimP <- as.numeric(Lims[[2]])
    LimL <- as.numeric(Lims[[3]])
  }
  # Initial Values
  RESULTS <- list()
  suc <- 0
  cumRet <- 1
  t <- 0
  # Buy and Limits are Defined
  if("send" %in% Mtype && "beforsend" %in% Mtype && Lims[[1]] == 1){
    BuySig <- cbind(as.xts(lag(beforsend, 1)), send)
    BuySig <- BuySig[complete.cases(BuySig), ]
    BUY <- rowSums(BuySig) == 2
    BUY <- xts(BUY, index(BuySig))
    b <- paste("BUY <- BUY['", StartDate, "/", EndDtae, "']",sep = "")
    eval(parse(text = b))
    Bact <- which(BUY)
    n <- length(Bact)
    kk <- vector()
    if(n > 0){
      for (i in 1:n) {
        ddd <- index(BUY[Bact[i],])
        ppp <- HLC[index(HLC) >= ddd,]
        pr <- ((ppp[,1]/as.numeric(HLC[ddd,3])) - 1) * 100
        lo <- ((ppp[,2]/as.numeric(HLC[ddd,3])) - 1) * 100
        kk[i] <- min(which(pr > LimP)[1],which(lo < -LimL)[1],na.rm = TRUE)
        if(is.na(kk[i]) || kk[i] == Inf){
          kk[i] <- nrow(pr)
        }
        t <- t + 1
        Date <- as.Date(ddd)
        Action <- "B"
        Price <- C[Date]
        names(Price) <- "Price"
        Return <- NA
        RESULTS[[t]] <- data.frame(Date, Action, Price, Return)
        t <- t + 1
        Date <- as.Date(index(ppp)[kk[i]])
        Action <- "S"
        Price <- C[Date]
        names(Price) <- "Price"
        Ret <- (Price/RESULTS[[t - 1]][, 3]) - 1
        if(Ret > 0.015){
          suc <- suc + 1
        }
        cumRet <- cumRet * (as.numeric(Ret) + 0.985)
        names(Ret) <- "Return"
        RESULTS[[t]] <- data.frame(Date, Action, Price, Ret)
      }
    }
  }
  # Sell and Limits are defined
  if("buy" %in% Mtype && "beforbuy" %in% Mtype && Lims[[1]] == 1){
    SellSig <- cbind(as.xts(lag(beforbuy, 1)), buy)
    SellSig <- SellSig[complete.cases(SellSig), ]
    SELL <- rowSums(SellSig) == 2
    SELL <- xts(SELL, as.Date(index(SellSig)))
    b <- paste("SELL <- SELL['", StartDate, "/", EndDtae, "']", 
               sep = "")
    eval(parse(text = b))
    Sact <- which(SELL)
    n <- length(Sact)
    kk <- vector()
    if(n > 0){
      for (i in 1:n) {
        ddd <- index(SELL[Sact[i],])
        ppp <- HLC[index(HLC) >= ddd,]
        pr <- ((as.numeric(HLC[ddd,3])/ppp[,2]) - 1) * 100
        lo <- ((as.numeric(HLC[ddd,3])/ppp[,1]) - 1) * 100
        kk[i] <- min(which(pr > LimP)[1],which((lo < -LimL))[1],na.rm = TRUE)
        if(is.na(kk[i]) || kk[i] == Inf){
          kk[i] <- nrow(pr)
        }
        t <- t + 1
        Date <- as.Date(ddd)
        Action <- "S"
        Price <- C[Date]
        names(Price) <- "Price"
        Return <- NA
        RESULTS[[t]] <- data.frame(Date, Action, Price, Return)
        t <- t + 1
        Date <- as.Date(index(ppp)[kk[i]])
        Action <- "B"
        Price <- C[Date]
        names(Price) <- "Price"
        Ret <- (Price/RESULTS[[t - 1]][, 3]) - 1
        if(Ret > 0.015){
          suc <- suc + 1
        }
        cumRet <- cumRet * (as.numeric(Ret) + 0.985)
        names(Ret) <- "Return"
        RESULTS[[t]] <- data.frame(Date, Action, Price, Ret)
      }
    }
  }
  # Buy Sell Conditions defined
  if(length(Mtype) == 4 && Lims[[1]] == 0){
    BuySig <- cbind(as.xts(lag(beforsend, 1)), send)
    BuySig <- BuySig[complete.cases(BuySig), ]
    SellSig <- cbind(as.xts(lag(beforbuy, 1)), buy)
    SellSig <- SellSig[complete.cases(SellSig), ]
    BUY <- rowSums(BuySig) == 2
    BUY <- xts(BUY, index(BuySig))
    SELL <- rowSums(SellSig) == 2
    SELL <- xts(SELL, as.Date(index(SellSig)))
    b <- paste("SELL <- SELL['", StartDate, "/", EndDtae, "']", 
               sep = "")
    eval(parse(text = b))
    b <- paste("BUY <- BUY['", StartDate, "/", EndDtae, "']", 
               sep = "")
    eval(parse(text = b))
    Bact <- which(BUY)
    Sact <- which(SELL)
    Sact <- Sact[Sact > Bact[1]]
    Bact <- Bact[Bact < Sact[length(Sact)]]
    n <- min(length(Bact), length(Sact))
    Act <- matrix(nrow = n,ncol = 2)
    Act[1,1] <- Bact[1]
    Act[1,2] <- Sact[1]
    for (i in 2:n) {
      t <- i - 1
      BB <- which(Bact > Act[t,2])
      if(length(BB) >= 1){
        BB <- BB[1]
        Act[i,1] <- Bact[BB]
        CC <- which(Sact > Act[i,1])
        if(length(CC) >= 1){
          CC <- CC[1]
          Act[i,2] <- Sact[CC]
        }
      }
    }
    Act <- Act[complete.cases(Act),]
    Sact <- Act[,2]
    Bact <- Act[,1]
    n <- length(Bact)
    for (i in 1:n) {
      t <- (2 * i) - 1
      tt <- 2 * i
      Date <- as.Date(index(BUY)[Bact[i]])
      Action <- "B"
      Price <- C[Date]
      names(Price) <- "Price"
      Return <- NA
      RESULTS[[t]] <- data.frame(Date, Action, Price, Return)
      Date <- as.Date(index(SELL)[Sact[i]])
      Action <- "S"
      Price <- C[Date]
      names(Price) <- "Price"
      Ret <- (Price/RESULTS[[t]][, 3]) - 1
      if(Ret > 0.015){
        suc <- suc + 1
      }
      cumRet <- cumRet * (as.numeric(Ret) + 0.985)
      names(Ret) <- "Return"
      RESULTS[[tt]] <- data.frame(Date, Action, Price, Ret)
    }
  }
  n <- length(RESULTS) / 2
  successRate <- suc / n
  CumulativeReturn <- as.numeric(cumRet - 1)
  if(AccSu >= successRate && AccCu >= CumulativeReturn){
    Acc <- 1
    nat <- list(Acc,successRate,CumulativeReturn)
  }else{
    Acc <- 0
    nat <- list(Acc,successRate,CumulativeReturn)
  }
  names(nat) <- c("Accept","SuccessRate","CumulativeReturn")
  nat
}
