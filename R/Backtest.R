Backtest <- function (x) 
{
  library(jsonlite)
  library(zoo)
  library(xts)
  library(TTR)
  library(quantmod)
  # get the data from PHP
  x <- as.character(x)
  v <- fromJSON(x)
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
    tar <- as.Date(as.character(cc[, 8]), "%Y%m%d")
    bb <- xts(cc[, 1:7], tar)
    colnames(bb) <- c("First", "High", "Low", "Close", "Value", 
                      "Volume", "Open")
    HLC <- bb[, c(2, 3, 4)]
    OHLC <- bb[, c(7, 2, 3, 4)]
    HL <- bb[, c(2, 3)]
    C <- bb[, 4]
  }
  # Indicators of the Strategy
  Inds <- v$indicators
  n <- length(Inds)
  for (i in 1:n) {
    m <- nrow(Inds[[i]][[4]])
    qqq <- ""
    if (m > 1) {
      for (j in 1:m) {
        qqq <- paste0(qqq,",", Inds[[i]][[4]][j,1], sep = "")
      }
    }else {
      qqq <- paste(",",Inds[[i]][[4]][1,1])
    }
    if(length(qqq) > 1){
      qqq <- paste(qqq,collapse="")
    }
    # proper data of each indicator
    lll <- which(Indicators_Data[,1] == Inds[[i]][[1]])
    Da <- as.character(Indicators_Data[lll,2])
    b <- paste(Inds[[i]][[2]], " <- ", Inds[[i]][[1]], "(",Da,qqq, ")", sep = "")
    eval(parse(text = b))
    b <- paste("bb <- ncol(",Inds[[i]][[2]],")")
    eval(parse(text = b))
    if(bb > 1){
      b <- paste("nn <- colnames(",Inds[[i]][[2]],")")
      eval(parse(text = b))
      nn <- gsub("\\.","",nn)
      b <- paste("colnames(",Inds[[i]][[2]],") <- nn")
      eval(parse(text = b))
      for (j in 1:bb) {
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
    # Type and Order of
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
    }
    else {
      e <- g
    }
    b <- paste(Mtype[i], " <- e", sep = "")
    eval(parse(text = b))
  }
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
  RESULTS <- list()
  suc <- 0
  cumRet <- 1
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
  successRate <- suc / n
  nat <- list(successRate,as.numeric(cumRet - 1),RESULTS)
  names(nat) <- c("SuccessRate","CumulativeReturn","Results")
  nat
}
