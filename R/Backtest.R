Backtest <- function (x) 
{
  library(jsonlite)
  library(zoo)
  library(xts)
  library(TTR)
  library(quantmod)
  x <- as.character(x)
  v <- fromJSON(x)
  StartDate <- v$start_date
  EndDtae <- v$end_date
  share <- v$stock
  url <- "http://billionet.us/msa/api/stockhistory/065aa4a5c4c6f6a44fea1645ad9cc2c6/"
  lin <- paste(url, share, sep = "")
  vv <- fromJSON(lin)
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
  Inds <- v$indicators
  n <- length(Inds)
  for (i in 1:n) {
    m <- length(Inds[[i]]) - 3
    q <- ""
    if (m > 1) {
      for (j in 1:m) {
        t <- j + 3
        q <- paste(q, Inds[[i]][[t]], sep = "")
      }
    }
    else {
      q <- Inds[[i]][[4]]
    }
    b <- paste(Inds[[i]][[2]], " <- ", Inds[[i]][[1]], "(C,", 
               q, ")", sep = "")
    eval(parse(text = b))
  }
  Rels <- v$relations
  n <- length(Rels)
  type <- vector()
  Rules <- vector()
  for (i in 1:n) {
    nam <- paste(Rels[[i]][[2]], Rels[[i]][1], sep = "")
    Rules[i] <- nam
    type[i] <- Rels[[i]][[2]]
    q <- Rels[[i]][[3]]
    q[q == "="] <- "=="
    m <- length(q)
    d <- ""
    for (j in 1:m) {
      d <- paste(d, q[j], sep = "")
    }
    b <- paste(nam, " <- ", d)
    eval(parse(text = b))
  }
  b <- ""
  for (i in 1:n) {
    b <- paste(b, ",", Rules[i], sep = "")
  }
  b <- substr(b, 2, nchar(b))
  b <- paste("rull <- cbind(", b, ")", sep = "")
  eval(parse(text = b))
  Mtype <- unique(type)
  l <- length(Mtype)
  for (i in 1:l) {
    q <- which(type == Mtype[i])
    g <- rull[, q]
    k <- dim(g)[2]
    if (k > 1) {
      e <- rowSums(q) == k
    }
    else {
      e <- g
    }
    b <- paste(Mtype[i], " <- e", sep = "")
    eval(parse(text = b))
  }
  BuySig <- cbind(as.xts(Next(beforsend, 1)), send)
  BuySig <- BuySig[complete.cases(BuySig), ]
  SellSig <- cbind(as.xts(Next(beforbuy, 1)), buy)
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
  n <- min(length(Bact), length(Sact))
  Sact <- Sact[1:n]
  Bact <- Bact[1:n]
  RESULTS <- list()
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
    names(Ret) <- "Return"
    RESULTS[[tt]] <- data.frame(Date, Action, Price, Ret)
  }
  RESULTS
}